use std::{collections::{HashSet, HashMap}, path::Path, ops::Range};

use command_parser::parse_command;
use datapack_common::functions::{Function, Command, command_components::FunctionIdent};
use wasmparser::ValType;

use crate::{lir::{LirProgram, LirFunction, LirBasicBlock, LirInstr, Register, LirTerminator, Condition, Half, DoubleRegister}, ssa::{BlockId, Memory, interp::TypedValue, const_prop::{StaticValue, BitMask}, lir_emitter::RegisterWithInfo}, jump_mode, JumpMode};

/// This variable keeps track of how many commands have been run so far
pub static CMDS_RUN_VAR: &str = "%%commands_run reg";

/// This variable is the maximum number of commands allowed to run in a single tick
pub static MAX_CMDS_VAR: &str = "%%max_commands reg";

fn parse_function<C, T>(id: &str, code: C) -> Function
	where
		C: IntoIterator<Item=T>,
		T: AsRef<str>,
{
	let id = parse_command::<FunctionIdent>(id).unwrap();
	let code = code.into_iter();
	let cmds = code.map(|cmd| cmd.as_ref().parse().unwrap_or_else(|err| panic!("{:?} {:?}", err, cmd.as_ref()))).collect();
	Function { id, cmds }
}

fn create_scoreboard_init(code: &mut Vec<String>) {
	code.push("# Set up scoreboard".to_string());
	code.push("scoreboard objectives remove reg".to_string());
	code.push("scoreboard objectives add reg dummy".to_string());
}

fn create_cmd_count_init(code: &mut Vec<String>) {
	code.push(format!("scoreboard players set {CMDS_RUN_VAR} 0"));
	// TODO: This should check the maxCommandChainLength variable
	code.push(format!("scoreboard players set {MAX_CMDS_VAR} 1000"));
}

fn create_stack_init(code: &mut Vec<String>) {
	code.push("data modify storage wasm:datastack stack set value {}".to_string());
	code.push("data modify storage wasm:localstack stack set value {}".to_string());
	code.push("data modify storage wasm:returnstack stack set value {}".to_string());
	code.push("data modify storage wasm:scratch stack set value {}".to_string());
}

fn create_stdout_init(code: &mut Vec<String>) {
	code.push("data modify storage wasm:stdout buffer set value []".to_string());
}

fn create_pointers_init(code: &mut Vec<String>) {
	code.push("# Remove old armor stand pointers".to_string());
	code.push("kill @e[tag=memoryptr]".to_string());
	code.push("kill @e[tag=turtle]".to_string());
	code.push("kill @e[tag=nextchain]".to_string());

	code.push("# Add armor stand pointers".to_string());
	code.push("summon minecraft:armor_stand 0 0 8 {Marker:1b,Tags:[\"memoryptr\"],CustomName:'\"memoryptr\"',CustomNameVisible:1b}".to_string());
	code.push("summon minecraft:armor_stand 0 0 -2 {Marker:1b,Tags:[\"turtle\"],CustomName:'\"turtle\"',CustomNameVisible:1b}".to_string());
	code.push("summon minecraft:armor_stand 1 1 -1 {Marker:1b,Tags:[\"nextchain\"],CustomName:'\"nextchain\"',CustomNameVisible:1b}".to_string());
}

fn create_constants_init(constants: &HashSet<i32>, code: &mut Vec<String>) {
	let old_style = [-1];

	code.push(format!("scoreboard players set %%PAGE_SPAN_Z reg {PAGE_SPAN_Z}"));
	code.push(format!("scoreboard players set %%PAGE_SPAN_Y reg {PAGE_SPAN_Y}"));

	for v in old_style {
		code.push(format!("scoreboard players set %%{v} reg {v}"));
	}

	for i in 0..32 {
		let v = (1_u32 << i) as i32;
		code.push(format!("scoreboard players set %%{v} reg {v}"));
	}
	
	for v in constants.iter() {
		code.push(format!("scoreboard players set %const%{v} reg {v}"));
	}

	for v in [0] {
		code.push(format!("scoreboard players set %const%{v} reg {v}"));
	}
}

fn create_memory_init(memory: &[Memory], code: &mut Vec<String>) {
	assert!(memory.len() <= 1);

	for (memory_idx, memory) in memory.iter().enumerate() {
		assert_eq!(memory_idx, 0);
		assert_eq!(memory.data.len() % MEMORY_PAGE_SIZE, 0);
		let num_pages = memory.data.len() / MEMORY_PAGE_SIZE;
		for x_offset in 0..num_pages {
			let x_begin = x_offset as i32 * PAGE_SPAN_X;
			let x_end = x_begin + PAGE_SPAN_X - 1;
			let y_end = PAGE_SPAN_Y - 1;
			let z_end = PAGE_SPAN_Z - 1;
			// Web assembly page size is 64KiB
			// Thus an 8x256x8 area where each block is an i32
			// makes up exactly one page

			// Also note that a single fill command can only fill 32768 blocks,
			// so we'll just do it one at a time for safety
			code.push(format!("fill {x_begin} 0 0 {x_end} {y_end} {z_end} minecraft:air replace"));
			code.push(format!("fill {x_begin} 0 0 {x_end} {y_end} {z_end} minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:0}}}}}} replace"));
		}

		for (word_idx, d) in memory.data.chunks_exact(4).enumerate() {
			let mut data = [0; 4];
			data.copy_from_slice(d);
			let data = i32::from_le_bytes(data);

			if data != 0 {
				let (x, y, z) = get_address_pos(word_idx as i32 * 4);
				code.push(format!("setblock {x} {y} {z} minecraft:air replace"));
				code.push(format!("setblock {x} {y} {z} minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:{data}}}}}}} replace"))
			}
		}
	}
}

fn create_globals_init(globals: &[TypedValue], code: &mut Vec<String>) {
	for (idx, val) in globals.iter().enumerate() {
		match val {
			TypedValue::I32(v) => {
				let reg = Register::global_lo(idx as u32);
				code.push(format!("scoreboard players set {reg} {v}"));
			}
			TypedValue::I64(v) => {
				let v_lo = *v as i32;
				let v_hi = (*v >> 32) as i32;
				let (r_lo, r_hi) = DoubleRegister::global(idx as u32).split_lo_hi();
				code.push(format!("scoreboard players set {r_lo} {v_lo}"));
				code.push(format!("scoreboard players set {r_hi} {v_hi}"));
			}
		}
	}
}

fn create_return_addrs_init<I>(blocks: I, code: &mut Vec<String>)
	where I: Iterator<Item=BlockId>
{
	for (idx, block) in blocks.enumerate() {
		let addr_var = get_block_addr_var(block);
		code.push(format!("scoreboard players set {addr_var} {idx}"))
	}
}

// init code:
// create objectives
// set up globals
// set up memory
// reset local stack
// reset data stack
// initialize constants
// initialize "return address" constants
fn create_init_func(program: &LirProgram, constants: &HashSet<i32>) -> Function {
	let mut code = Vec::new();

	create_scoreboard_init(&mut code);
	create_cmd_count_init(&mut code);
	create_stack_init(&mut code);
	create_stdout_init(&mut code);
	create_pointers_init(&mut code);
	create_constants_init(constants, &mut code);
	create_memory_init(&program.memory, &mut code);
	create_globals_init(&program.globals, &mut code);
	create_return_addrs_init(program.all_block_ids(), &mut code);

	parse_function("wasmrunner:init", code)
}

fn create_return_to_saved_func(program: &LirProgram) -> Vec<Function> {
	let reg = Register::temp_lo(0);
	
	let mut code = Vec::new();

	code.push(format!("scoreboard players set {reg} -1"));
	let cond_taken = Register::cond_taken();
	code.push(format!("scoreboard players set {cond_taken} 0"));
	code.push("data modify storage wasm:scratch stack.data set from storage wasm:returnstack stack.data".to_string());
	code.push(format!("execute store result score {reg} run data get storage wasm:returnstack stack.data.ptr 1"));
	code.push("data modify storage wasm:returnstack stack set from storage wasm:returnstack stack.tail".to_string());

	let blocks = program.all_block_ids().enumerate().collect::<Vec<_>>();

	let mut funcs = Vec::new();
	create_nested_return_func(reg, &blocks, &mut funcs);

	let func_name = format!("wasmrunner:__return_to_saved_{}", funcs.len() - 1);

	code.push(format!("function {func_name}"));
	code.push(format!("scoreboard players set {cond_taken} 1"));

	let func = parse_function("wasmrunner:__return_to_saved", &code);
	funcs.push(func);
	funcs
}

fn create_nested_return_func(cond: Register, values: &[(usize, BlockId)], funcs: &mut Vec<Function>) {
	let cond_taken = Register::cond_taken();

	let mut code = Vec::new();

	match values[..] {
		[] => panic!("nested return func was empty"),
		[(addr, block_id)] => {
			let func = get_mc_id(block_id);
			code.push(format!("execute if score {cond_taken} matches 0 if score {cond} matches {addr} run function {func}"));
		}
		[(addr0, block_id0), (addr1, block_id1)] => {
			let func0 = get_mc_id(block_id0);
			code.push(format!("execute if score {cond_taken} matches 0 if score {cond} matches {addr0} run function {func0}"));
			let func1 = get_mc_id(block_id1);
			code.push(format!("execute if score {cond_taken} matches 0 if score {cond} matches {addr1} run function {func1}"));
		}
		_ => {
			create_nested_return_func(cond, &values[..values.len() / 2], funcs);
			let func_name_lesser = format!("wasmrunner:__return_to_saved_{}", funcs.len() - 1);

			create_nested_return_func(cond, &values[values.len() / 2 + 1..], funcs);
			let func_name_greater = format!("wasmrunner:__return_to_saved_{}", funcs.len() - 1);

			let (addr_mid, block_id_mid) = values[values.len() / 2];
			let func_mid = get_mc_id(block_id_mid);

			code.push(format!("execute if score {cond_taken} matches 0 if score {cond} matches ..{} run function {func_name_lesser}", addr_mid - 1));
			code.push(format!("execute if score {cond_taken} matches 0 if score {cond} matches {} run function {func_mid}", addr_mid));
			code.push(format!("execute if score {cond_taken} matches 0 if score {cond} matches {}.. run function {func_name_greater}", addr_mid + 1));
		}
	}

	let func_name = format!("wasmrunner:__return_to_saved_{}", funcs.len());
	let func = parse_function(&func_name, &code);
	funcs.push(func);
}

fn push_return_addr(addr: BlockId, code: &mut Vec<String>) {
	let addr_str = get_mc_id(addr);
	let addr_var = get_block_addr_var(addr);

	code.push("data modify storage wasm:scratch stack.data set value {}".to_string());
	code.push(format!("data modify storage wasm:scratch stack.data.'{addr_str}' set value 1"));
	code.push(format!("execute store result storage wasm:scratch stack.data.ptr int 1 run scoreboard players get {}", addr_var));
	code.push("data modify storage wasm:scratch stack.tail set from storage wasm:returnstack stack".to_string());
	code.push("data modify storage wasm:returnstack stack set from storage wasm:scratch stack".to_string());
}

fn pop_return_addr(code: &mut Vec<String>) {
	code.push("data modify storage wasm:returnstack stack set from storage wasm:returnstack stack.tail".to_string());
}

fn push_data(regs: &[Register], code: &mut Vec<String>) {
	let arr = create_array_with_consts(regs);
	code.push(format!("data modify storage wasm:scratch stack.data set value {arr}"));
	for (idx, reg) in regs.iter().enumerate() {
		if reg.get_const().is_some() { continue; }
		code.push(format!("execute store result storage wasm:scratch stack.data[{idx}] int 1 run scoreboard players get {reg}"));
	}
	code.push("data modify storage wasm:scratch stack.tail set from storage wasm:datastack stack".to_string());
	code.push("data modify storage wasm:datastack stack set from storage wasm:scratch stack".to_string());
}

fn pop_data(regs: &[Register], code: &mut Vec<String>) {
	for (idx, reg) in regs.iter().enumerate() {
		code.push(format!("execute store result score {reg} run data get storage wasm:datastack stack.data[{idx}] 1"));
	}
	code.push("data modify storage wasm:datastack stack set from storage wasm:datastack stack.tail".to_string());
}

fn create_array_with_consts(regs: &[Register]) -> String {
	let mut arr = '['.to_string();
	for (i, r) in regs.iter().enumerate() {
		if let Some(r) = r.get_const() {
			arr.push_str(&r.to_string())
		} else {
			arr.push('0');
		}
		if i != regs.len() - 1 {
			arr.push_str(", ");
		}
	}
	arr.push(']');

	arr
}

fn create_zeroed_array(count: usize) -> String {
	let mut arr = '['.to_string();
	for i in 0..count {
		arr.push('0');
		if i != count - 1 {
			arr.push_str(", ")
		}
	}
	arr.push(']');

	arr
}

fn push_local_frame(ty: &[ValType], code: &mut Vec<String>) {
	let arr = create_zeroed_array(ty.len() * 2);

	code.push(format!("data modify storage wasm:scratch stack.data set value {arr}"));
	code.push("data modify storage wasm:scratch stack.tail set from storage wasm:localstack stack".to_string());
	code.push("data modify storage wasm:localstack stack set from storage wasm:scratch stack".to_string());
}

fn pop_local_frame(_ty: &[ValType], code: &mut Vec<String>) {
	code.push("data modify storage wasm:localstack stack set from storage wasm:localstack stack.tail".to_string());
}

fn local_set(dst: u32, half: Half, src: Register, code: &mut Vec<String>) {
	let idx = dst * 2 + if half == Half::Hi { 1 } else { 0 };
	code.push(format!("execute store result storage wasm:localstack stack.data[{idx}] int 1 run scoreboard players get {src}"));
}

fn local_get(dst: Register, src: u32, half: Half, code: &mut Vec<String>) {
	let idx = src * 2 + if half == Half::Hi { 1 } else { 0 };
	code.push(format!("execute store result score {dst} run data get storage wasm:localstack stack.data[{idx}]"));
}

pub fn get_mc_id(block_id: BlockId) -> String {
	format!("wasmrunner:wasm_{}_{}", block_id.func, block_id.block)
}

/// To avoid needing to do fixups, each block has its "address"
/// stored in a variable with a name based on the block_id.
pub fn get_block_addr_var(block_id: BlockId) -> String {
	format!("%%returnaddr_wasm_{}_{} reg", block_id.func, block_id.block)
}

fn mem_store_unaligned_32(src: Register, addr: i32, offset: i32, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	assert!(offset > 0);
	assert!(offset < 4);

	let (x0, y0, z0) = get_address_pos(addr - offset);
	let (x1, y1, z1) = get_address_pos(addr - offset + 4);

	let tmp1 = Register::temp_lo(100_000);
	let tmp2 = Register::temp_lo(100_001);

	let shift = 1 << (8 * offset);

	const_pool.insert(shift);
	let shift = Register::const_val(shift);

	code.push(format!("execute store result score {tmp1} run data get block {x0} {y0} {z0} RecordItem.tag.Memory 1"));
	code.push(format!("scoreboard players operation {tmp1} %= {shift}"));
	code.push(format!("scoreboard players operation {tmp2} = {src}"));
	code.push(format!("scoreboard players operation {tmp2} *= {shift}"));
	code.push(format!("scoreboard players operation {tmp1} += {tmp2}"));
	code.push(format!("execute store result block {x0} {y0} {z0} RecordItem.tag.Memory int 1 run scoreboard players get {tmp1}"));

	code.push(format!("execute store result score {tmp1} run data get block {x1} {y1} {z1} RecordItem.tag.Memory 1"));
	code.push(format!("scoreboard players operation {tmp2} = {tmp1}"));
	code.push(format!("scoreboard players operation {tmp2} %= {shift}"));
	code.push(format!("scoreboard players operation {tmp1} -= {tmp2}"));
	emit_constant_shru(tmp2, src, 32 - (8 * offset), code, const_pool);
	code.push(format!("scoreboard players operation {tmp1} += {tmp2}"));
	code.push(format!("execute store result block {x1} {y1} {z1} RecordItem.tag.Memory int 1 run scoreboard players get {tmp1}"));
}

fn mem_store_32(src: Register, addr: RegisterWithInfo, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	if INSERT_MEM_PRINTS {
		code.push(tellraw_mem_store(32, src, addr.0));
	}

	if let Some(addr) = addr.get_const() {
		match addr % 4 {
			0 => {
				let (x, y, z) = get_address_pos(addr);
				code.push(format!("execute store result block {x} {y} {z} RecordItem.tag.Memory int 1 run scoreboard players get {src}"));
			}
			offset@(1 | 2 | 3) => { mem_store_unaligned_32(src, addr, offset, code, const_pool); }
			_ => unreachable!(),
		}
	} else if ENABLE_MEM_OPTS && addr.1 != StaticValue::unknown() && addr.1 != (BitMask { set_bits: 0, clr_bits: 1 }.into()) {
		let o = known_offset(addr.1);
		let z = known_z(addr.1);
		let y = known_y(addr.1);
		let x = known_x(addr.1);

		if let (Some(0), None, None, None) = (o, z, y, x) {
			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("function intrinsic:setptr".to_string());
			code.push(format!("execute at @e[tag=memoryptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get {src}"));
		} else if let (Some(0), None, Some(y), Some(x)) = (o, z, y, x) {
			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("scoreboard players operation %ptr reg /= %%4 reg".to_string());
			code.push("scoreboard players operation %ptr reg %= %%8 reg".to_string());
			code.push("execute as @e[tag=memoryptr] store result entity @s Pos[2] double 1 run scoreboard players get %ptr reg".to_string());
			code.push(format!("execute at @e[tag=memoryptr] store result block {x} {y} ~ RecordItem.tag.Memory int 1 run scoreboard players get {src}"));
		} else {
			todo!("{:X?} {:X?} {:X?} {:X?} {:X?}", addr.1, o, z, y, x);
		}

		/*let o_stride = 1;
		let z_stride = 4;
		let y_stride = 4 * 8;
		let x_stride = 4 * 8 * 256;

		let o_wrap = 4;
		let z_wrap = 4 * 8;
		let y_wrap = 4 * 8 * 256;*/

		/*if let (Some(0), None, Some(y), Some(x)) = (o, z, y, x) {
			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("scoreboard players operation %ptr reg /= %%4 reg".to_string());
			code.push("scoreboard players operation %ptr reg %= %%32 reg".to_string());
			code.push("execute as @e[tag=memoryptr] store result entity @s Pos[2] double 1 run scoreboard players get %ptr reg".to_string());
			code.push(format!("execute at @e[tag=memoryptr] store result score {dst} run data get block {x} {y} ~ RecordItem.tag.Memory 1"));
		} else if let (Some(0), None, None, None) = (o, z, y, x) {
			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("function intrinsic:setptr".to_string());
			code.push(format!("execute at @e[tag=memoryptr] store result score {dst} run data get block ~ ~ ~ RecordItem.tag.Memory 1"));
		} else {*/
	} else {
		code.push(format!("scoreboard players operation %ptr reg = {addr}"));
		code.push(format!("scoreboard players operation %param0%0 reg = {src}"));
		code.push("function intrinsic:setptr".to_string());
		code.push("function intrinsic:store_word".to_string());
	}
}

fn mem_store_16(src: Register, addr: RegisterWithInfo, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	if INSERT_MEM_PRINTS {
		code.push(tellraw_mem_store(16, src, addr.0));
	}
	
	if let Some(addr) = addr.get_const() {
		match addr % 4 {
			0 => {
				let (x, y, z) = get_address_pos(addr);
				code.push(format!("scoreboard players operation %param2%0 reg = {src}"));
				code.push("scoreboard players operation %param2%0 reg %= %%65536 reg".to_string());
				code.push(format!("execute store result score %param0%0 reg run data get block {x} {y} {z} RecordItem.tag.Memory 1"));
				code.push("scoreboard players operation %return%0 reg = %param0%0 reg".to_string());

				code.push("scoreboard players operation %param0%0 reg %= %%65536 reg".to_string());
				code.push("scoreboard players operation %return%0 reg -= %param0%0 reg".to_string());

				code.push("scoreboard players operation %return%0 reg += %param2%0 reg".to_string());
				code.push(format!("execute store result block {x} {y} {z} RecordItem.tag.Memory int 1 run scoreboard players get %return%0 reg"));
			}
			1 => {
				let (x, y, z) = get_address_pos(addr - 1);

				let tmp1 = Register::temp_lo(4321);
				let tmp2 = Register::temp_lo(4322);

				code.push(format!("execute store result score {tmp1} run data get block {x} {y} {z} RecordItem.tag.Memory 1"));
				emit_constant_and(tmp1, tmp1.into(), 0xFF_00_00_FF_u32 as i32, code, const_pool);
				code.push(format!("scoreboard players operation {tmp2} = {src}"));
				code.push(format!("scoreboard players operation {tmp2} *= %%256 reg"));
				code.push(format!("scoreboard players operation {tmp1} += {tmp2}"));
				code.push(format!("execute store result block {x} {y} {z} RecordItem.tag.Memory int 1 run scoreboard players get {tmp1}"));
			}
			2 => {
				let (x, y, z) = get_address_pos(addr - 2);

				code.push(format!("execute store result score %param0%0 reg run data get block {x} {y} {z} RecordItem.tag.Memory 1"));
				code.push("scoreboard players operation %param0%0 reg %= %%65536 reg".to_string());
				code.push(format!("scoreboard players operation %param2%0 reg = {src}"));
				code.push("scoreboard players operation %param2%0 reg *= %%65536 reg".to_string());
				code.push("scoreboard players operation %param0%0 reg += %param2%0 reg".to_string());
				code.push(format!("execute store result block {x} {y} {z} RecordItem.tag.Memory int 1 run scoreboard players get %param0%0 reg"));
			}
			a @ 3 => todo!("{:?}", a),
			_ => unreachable!(),
		}
		// TODO:
	} else if ENABLE_MEM_OPTS && addr.1 != StaticValue::unknown() && addr.1.into_mask() != (BitMask { set_bits: 0, clr_bits: 1 }) {
		let o = known_offset(addr.1);
		let z = known_z(addr.1);
		let y = known_y(addr.1);
		let x = known_x(addr.1);

		todo!("{:X?} {:X?} {:X?} {:X?} {:X?}", addr.1, o, z, y, x);
	} else {
		code.push(format!("scoreboard players operation %ptr reg = {addr}"));
		code.push(format!("scoreboard players operation %param2%0 reg = {src}"));
		code.push("function intrinsic:store_halfword".to_string());
	}
}

fn mem_store_8 (src: Register, addr: RegisterWithInfo, code: &mut Vec<String>) {
	if INSERT_MEM_PRINTS {
		code.push(tellraw_mem_store(8, src, addr.0));
	}

	if let Some(addr) = addr.get_const() {
		match addr % 4 {
			0 => {
				let (x, y, z) = get_address_pos(addr);
				code.push(format!("scoreboard players operation %param2%0 reg = {src}"));
				code.push("scoreboard players operation %param2%0 reg %= %%256 reg".to_string());
				code.push(format!("execute store result score %param0%0 reg run data get block {x} {y} {z} RecordItem.tag.Memory 1"));
				code.push("scoreboard players operation %return%0 reg = %param0%0 reg".to_string());

				code.push("scoreboard players operation %param0%0 reg %= %%256 reg".to_string());
				code.push("scoreboard players operation %return%0 reg -= %param0%0 reg".to_string());

				code.push("scoreboard players operation %return%0 reg += %param2%0 reg".to_string());
				code.push(format!("execute store result block {x} {y} {z} RecordItem.tag.Memory int 1 run scoreboard players get %return%0 reg"));
			}
			1 => {
				let (x, y, z) = get_address_pos(addr - 1);
				code.push(format!("scoreboard players operation %param2%0 reg = {src}"));
				code.push("scoreboard players operation %param2%0 reg %= %%256 reg".to_string());
				code.push(format!("execute store result score %param0%0 reg run data get block {x} {y} {z} RecordItem.tag.Memory 1"));
				code.push("scoreboard players operation %return%0 reg = %param0%0 reg".to_string());

				code.push("scoreboard players operation %param0%0 reg %= %%65536 reg".to_string());
				code.push("scoreboard players operation %param0%0 reg /= %%256 reg".to_string());
				code.push("scoreboard players operation %param0%0 reg *= %%256 reg".to_string());
				code.push("scoreboard players operation %return%0 reg -= %param0%0 reg".to_string());
				code.push("scoreboard players operation %param2%0 reg *= %%256 reg".to_string());

				code.push("scoreboard players operation %return%0 reg += %param2%0 reg".to_string());
				code.push(format!("execute store result block {x} {y} {z} RecordItem.tag.Memory int 1 run scoreboard players get %return%0 reg"));
			}
			2 => {
				let (x, y, z) = get_address_pos(addr - 2);
				code.push(format!("scoreboard players operation %param2%0 reg = {src}"));
				code.push("scoreboard players operation %param2%0 reg %= %%256 reg".to_string());
				code.push(format!("execute store result score %param0%0 reg run data get block {x} {y} {z} RecordItem.tag.Memory 1"));
				code.push("scoreboard players operation %return%0 reg = %param0%0 reg".to_string());

				code.push("scoreboard players operation %param0%0 reg %= %%16777216 reg".to_string());
				code.push("scoreboard players operation %param0%0 reg /= %%65536 reg".to_string());
				code.push("scoreboard players operation %param0%0 reg *= %%65536 reg".to_string());
				code.push("scoreboard players operation %return%0 reg -= %param0%0 reg".to_string());
				code.push("scoreboard players operation %param2%0 reg *= %%65536 reg".to_string());

				code.push("scoreboard players operation %return%0 reg += %param2%0 reg".to_string());
				code.push(format!("execute store result block {x} {y} {z} RecordItem.tag.Memory int 1 run scoreboard players get %return%0 reg"));
			}
			3 => {
				let (x, y, z) = get_address_pos(addr - 3);
				code.push(format!("scoreboard players operation %param2%0 reg = {src}"));
				code.push("scoreboard players operation %param2%0 reg %= %%256 reg".to_string());
				code.push(format!("execute store result score %param0%0 reg run data get block {x} {y} {z} RecordItem.tag.Memory 1"));
				code.push("scoreboard players operation %param0%0 reg %= %%16777216 reg".to_string());
				code.push("scoreboard players operation %param2%0 reg *= %%16777216 reg".to_string());
				code.push("scoreboard players operation %param0%0 reg += %param2%0 reg".to_string());
				code.push(format!("execute store result block {x} {y} {z} RecordItem.tag.Memory int 1 run scoreboard players get %param0%0 reg"));
			}
			_ => unreachable!(),
		}
	} else if ENABLE_MEM_OPTS && addr.1 != StaticValue::unknown() && addr.1.into_mask() != (BitMask { set_bits: 0, clr_bits: 1 }) {
		let o = known_offset(addr.1);
		let z = known_z(addr.1);
		let y = known_y(addr.1);
		let x = known_x(addr.1);
		
		if let (Some(0), None, None, None) = (o, z, y, x) {
			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("function intrinsic:setptr".to_string());

			code.push(format!("scoreboard players operation %param2%0 reg = {src}"));
			code.push("scoreboard players operation %param2%0 reg %= %%256 reg".to_string());
			code.push("execute at @e[tag=memoryptr] store result score %param0%0 reg run data get block ~ ~ ~ RecordItem.tag.Memory 1".to_string());
			code.push("scoreboard players operation %return%0 reg = %param0%0 reg".to_string());

			code.push("scoreboard players operation %param0%0 reg %= %%256 reg".to_string());
			code.push("scoreboard players operation %return%0 reg -= %param0%0 reg".to_string());

			code.push("scoreboard players operation %return%0 reg += %param2%0 reg".to_string());
			code.push("execute at @e[tag=memoryptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %return%0 reg".to_string());
		} else {
			todo!("{:X?} {:X?} {:X?} {:X?} {:X?}", addr.1, o, z, y, x);
		}

	} else {
		code.push(format!("scoreboard players operation %ptr reg = {addr}"));
		code.push(format!("scoreboard players operation %param2%0 reg = {src}"));
		code.push("function intrinsic:setptr".to_string());
		code.push("function intrinsic:store_byte".to_string());
	}
}

const PAGE_SPAN_Z: i32 = 32;
const PAGE_SPAN_Y: i32 = 256;
const PAGE_SPAN_X: i32 = 2;

const MEMORY_PAGE_SIZE: usize = 65536;

const MEMORY_PAGE_BLOCKS: usize = MEMORY_PAGE_SIZE / 4;

fn get_address_pos(addr: i32) -> (i32, i32, i32) {
	assert!(addr >= 0);
	assert_eq!(addr % 4, 0);

	let mut word_addr = addr / 4;

	let z = word_addr % PAGE_SPAN_Z;
	word_addr /= PAGE_SPAN_Z;
	let y = word_addr % PAGE_SPAN_Y;
	word_addr /= PAGE_SPAN_Y;
	let x = word_addr;

	(x, y, z)
}

const INSERT_MEM_PRINTS: bool = false;

fn tellraw_mem_store(size: u32, src: Register, addr: Register) -> String {
	let mut s = format!(r#"tellraw @a [{{"text":"Memory store of size {size} to "}},"#);

	s.push_str(r#"{"score":{"name":""#);
	s.push_str(&addr.scoreboard_pair().0.to_string());
	s.push_str(r#"","objective":"reg"}}"#);

	s.push_str(r#",{"text":", value: "},"#);

	s.push_str(r#"{"score":{"name":""#);
	s.push_str(&src.scoreboard_pair().0.to_string());
	s.push_str(r#"","objective":"reg"}}"#);

	s.push(']');

	s
}

fn tellraw_mem_load(size: u32, dst: Register, addr: Register) -> String {
	let mut s = format!(r#"tellraw @a [{{"text":"Memory load of size {size} from "}},"#);

	s.push_str(r#"{"score":{"name":""#);
	s.push_str(&addr.scoreboard_pair().0.to_string());
	s.push_str(r#"","objective":"reg"}}"#);

	s.push_str(r#",{"text":", value: "},"#);

	s.push_str(r#"{"score":{"name":""#);
	s.push_str(&dst.scoreboard_pair().0.to_string());
	s.push_str(r#"","objective":"reg"}}"#);

	s.push(']');

	s
}

fn mem_load_unaligned_32(dst: Register, addr: i32, offset: i32, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	assert!(offset > 0);
	assert!(offset < 4);

	let (x0, y0, z0) = get_address_pos(addr - offset);
	let (x1, y1, z1) = get_address_pos(addr - offset + 4);

	let tmp1 = Register::temp_lo(100_000);

	code.push(format!("execute store result score {dst} run data get block {x0} {y0} {z0} RecordItem.tag.Memory 1"));
	emit_constant_shru(dst, dst, 8 * offset, code, const_pool);

	let shift = 1 << (32 - 8 * offset);
	const_pool.insert(shift);
	let shift = Register::const_val(shift);

	code.push(format!("execute store result score {tmp1} run data get block {x1} {y1} {z1} RecordItem.tag.Memory 1"));
	code.push(format!("scoreboard players operation {tmp1} *= {shift}"));
	code.push(format!("scoreboard players operation {dst} += {tmp1}"))
}

fn mem_load_64(dst: DoubleRegister, addr: RegisterWithInfo, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	if addr.get_const().is_some() {
		todo!()
	} else {
		code.push(format!("scoreboard players operation %ptr reg = {addr}"));
		code.push("function intrinsic:load_doubleword".to_string());
		code.push(format!("scoreboard players operation {} = %return%0%lo reg", dst.lo()));
		code.push(format!("scoreboard players operation {} = %return%0%hi reg", dst.hi()));
	}
}

// _ -> byte
// 4 -> z
// 4 * 8 -> y
// 4 * 8 * 256 -> x

// xxxx_xxxx_xxxx_xxxx xxx_yyyyyyyy_zzz_bb

fn known_offset(value: StaticValue) -> Option<i32> {
	if known_bits(value) & 0b11 == 0b11 {
		Some((value.into_mask().set_bits as i32) & 0b11)
	} else {
		None
	}
}

fn known_z(value: StaticValue) -> Option<i32> {
	if known_bits(value) & 0b111_00 == 0b111_00 {
		Some(((value.into_mask().set_bits as i32) & 0b111_00) >> 2)
	} else {
		None
	}
}

#[allow(clippy::unusual_byte_groupings)]
fn known_y(value: StaticValue) -> Option<i32> {
	if known_bits(value) & 0b1111_1111_000_00 == 0b1111_1111_000_00 {
		Some(((value.into_mask().set_bits as i32) & 0b1111_1111_000_00) >> 5)
	} else {
		None
	}
}

fn known_x(value: StaticValue) -> Option<i32> {
	if known_bits(value) & 0xFFFF_FFFF_FFFF_E000 == 0xFFFF_FFFF_FFFF_E000 {
		let v = (value.into_mask().set_bits as u64) >> 13;
		Some(v as i32)
	} else {
		None
	}
}

fn known_bits(value: StaticValue) -> u64 {
	match value {
		StaticValue::Mask(msk) => msk.set_bits | msk.clr_bits,
		StaticValue::Constant(_) => 0xFFFF_FFFF_FFFF_FFFF,
	}
}

fn mem_load_32(dst: Register, addr: RegisterWithInfo, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	if let Some(addr) = addr.get_const() {
		match addr % 4 {
			0 => {
				let (x, y, z) = get_address_pos(addr);

				code.push(format!("execute store result score {dst} run data get block {x} {y} {z} RecordItem.tag.Memory 1"));
			}
			offset@(1 | 2 | 3) => mem_load_unaligned_32(dst, addr, offset, code, const_pool),
			_ => unreachable!(),
		}
	} else if ENABLE_MEM_OPTS && addr.1 != StaticValue::unknown() && addr.1 != (BitMask { set_bits: 0, clr_bits: 1 }.into()) {
		let o = known_offset(addr.1);
		let z = known_z(addr.1);
		let y = known_y(addr.1);
		let x = known_x(addr.1);

		/*let o_stride = 1;
		let z_stride = 4;
		let y_stride = 4 * 8;
		let x_stride = 4 * 8 * 256;

		let o_wrap = 4;
		let z_wrap = 4 * 8;
		let y_wrap = 4 * 8 * 256;*/

		if let (Some(0), None, Some(y), Some(x)) = (o, z, y, x) {
			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("scoreboard players operation %ptr reg /= %%4 reg".to_string());
			code.push("scoreboard players operation %ptr reg %= %%8 reg".to_string());
			code.push("execute as @e[tag=memoryptr] store result entity @s Pos[2] double 1 run scoreboard players get %ptr reg".to_string());
			code.push(format!("execute at @e[tag=memoryptr] store result score {dst} run data get block {x} {y} ~ RecordItem.tag.Memory 1"));
		} else if let (Some(0), Some(z), None, None) = (o, z, y, x) {
			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("scoreboard players operation %ptr reg /= %%32 reg".to_string());
			code.push("scoreboard players operation %%y reg = %ptr reg".to_string());
			code.push("scoreboard players operation %%y reg %= %%256 reg".to_string());
			code.push("scoreboard players operation %ptr reg /= %%256 reg".to_string());
			code.push("execute as @e[tag=memoryptr] store result entity @s Pos[0] double 1 run scoreboard players get %ptr reg".to_string());
			code.push("execute as @e[tag=memoryptr] store result entity @s Pos[1] double 1 run scoreboard players get %%y reg".to_string());
			code.push(format!("execute at @e[tag=memoryptr] store result score {dst} run data get block ~ ~ {z} RecordItem.tag.Memory 1"));
		} else if let (Some(0), None, None, None) = (o, z, y, x) {
			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("function intrinsic:setptr".to_string());
			code.push(format!("execute at @e[tag=memoryptr] store result score {dst} run data get block ~ ~ ~ RecordItem.tag.Memory 1"));
		} else {
			println!("TODO: {:X?} {:X?} {:X?} {:X?} {:X?}", addr.1, o, z, y, x);

			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("function intrinsic:setptr".to_string());
			code.push("function intrinsic:load_word".to_string());
			code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
		}
	} else {
		code.push(format!("scoreboard players operation %ptr reg = {addr}"));
		code.push("function intrinsic:setptr".to_string());
		code.push("function intrinsic:load_word".to_string());
		code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
	}

	if INSERT_MEM_PRINTS {
		code.push(tellraw_mem_load(32, dst, addr.0));
	}
}

fn mem_load_16(dst: Register, addr: RegisterWithInfo, code: &mut Vec<String>) {
	if let Some(addr) = addr.get_const() {
		match addr % 4 {
			0 => {
				let (x, y, z) = get_address_pos(addr);

				code.push(format!("execute store result score {dst} run data get block {x} {y} {z} RecordItem.tag.Memory 1"));
				code.push(format!("scoreboard players operation {dst} %= %%65536 reg"));

			}
			1 => {
				let (x, y, z) = get_address_pos(addr - 1);

				code.push(format!("execute store result score {dst} run data get block {x} {y} {z} RecordItem.tag.Memory 1"));
				code.push(format!("scoreboard players operation {dst} %= %%16777216 reg"));
				code.push(format!("scoreboard players operation {dst} /= %%256 reg"));
			}
			2 => {
				let (x, y, z) = get_address_pos(addr - 2);

				let tmp1 = Register::temp_lo(100_001);

				code.push(format!("execute store result score {dst} run data get block {x} {y} {z} RecordItem.tag.Memory 1"));
				code.push(format!("execute store success score {tmp1} if score {dst} matches ..-1"));
				code.push(format!("execute if score {tmp1} matches 1 run scoreboard players operation {dst} -= %%-2147483648 reg"));
				code.push(format!("scoreboard players operation {dst} /= %%65536 reg"));
				code.push(format!("execute if score {tmp1} matches 1 run scoreboard players add {dst} 32768"));
			}
			3 => {
				mem_load_8(dst, Register::const_val(addr).into(), code);

				let (x1, y1, z1) = get_address_pos(addr + 1);

				let tmp1 = Register::temp_lo(100_001);

				code.push(format!("execute store result score {tmp1} run data get block {x1} {y1} {z1} RecordItem.tag.Memory 1"));
				code.push(format!("scoreboard players operation {tmp1} %= %%256 reg"));
				code.push(format!("scoreboard players operation {tmp1} *= %%256 reg"));
				code.push(format!("scoreboard players operation {dst} += {tmp1}"));
			}
			_ => unreachable!(),
		}
	} else if ENABLE_MEM_OPTS && addr.1 != StaticValue::unknown() {
		let o = known_offset(addr.1);
		let z = known_z(addr.1);
		let y = known_y(addr.1);
		let x = known_x(addr.1);

		if let (None, None, None, None) = (o, z, y, x) {
			if addr.1.into_mask().clr_bits == 1 {
				code.push(format!("scoreboard players operation %ptr reg = {addr}"));
				code.push("function intrinsic:load_halfword_aligned".to_string());
				code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
			}
		} else if let (Some(0), None, None, None) = (o, z, y, x) {
			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("function intrinsic:setptr".to_string());
			code.push(format!("execute at @e[tag=memoryptr] store result score {dst} run data get block ~ ~ ~ RecordItem.tag.Memory 1"));
			code.push(format!("scoreboard players operation {dst} %= %%65536 reg"));
		} else if let (Some(0), None, None, Some(x)) = (o, z, y, x) {
			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("scoreboard players operation %ptr reg /= %%4 reg".to_string());
			code.push("scoreboard players operation %%z reg = %ptr reg".to_string());
			code.push("scoreboard players operation %%z reg %= %%8 reg".to_string());
			code.push("scoreboard players operation %ptr reg /= %%8 reg".to_string());
			code.push("scoreboard players operation %ptr reg %= %%256 reg".to_string());
			code.push("execute as @e[tag=memoryptr] store result entity @s Pos[2] double 1 run scoreboard players get %%z reg".to_string());
			code.push("execute as @e[tag=memoryptr] store result entity @s Pos[1] double 1 run scoreboard players get %ptr reg".to_string());
			code.push(format!("execute store result score {dst} run data get block {x} ~ ~ RecordItem.tag.Memory 1"));
			code.push(format!("scoreboard players operation {dst} %= %%65536 reg"));
		} else {
			todo!("{:X?} {:X?} {:X?} {:X?} {:X?}", addr.1, o, z, y, x);
		}
	} else {
		code.push(format!("scoreboard players operation %ptr reg = {addr}"));
		code.push("function intrinsic:load_halfword".to_string());
		code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
	}

	if INSERT_MEM_PRINTS {
		code.push(tellraw_mem_load(16, dst, addr.0));
	}
}

const ENABLE_MEM_OPTS: bool = false;

fn mem_load_8 (dst: Register, addr: RegisterWithInfo, code: &mut Vec<String>) {
	if let Some(addr) = addr.get_const() {
		assert!(addr >= 0);
		let addr_offset = addr % 4;
		let aligned_addr = addr - addr_offset;
		let (x, y, z) = get_address_pos(aligned_addr);

		code.push(format!("execute store result score {dst} run data get block {x} {y} {z} RecordItem.tag.Memory 1"));

		if addr_offset != 0 {
			code.push(format!("scoreboard players operation {dst} /= %%{} reg", 1 << (addr_offset * 8)));
		}

		code.push(format!("scoreboard players operation {dst} %= %%256 reg"));
	} else if ENABLE_MEM_OPTS && addr.1 != StaticValue::unknown() && addr.1.into_mask() != (BitMask { set_bits: 0, clr_bits: 1 }) {
		let o = known_offset(addr.1);
		let z = known_z(addr.1);
		let y = known_y(addr.1);
		let x = known_x(addr.1);

		if let (Some(0), None, None, None) = (o, z, y, x) {
			code.push(format!("scoreboard players operation %ptr reg = {addr}"));
			code.push("function intrinsic:setptr".to_string());
			code.push(format!("execute at @e[tag=memoryptr] store result score {dst} run data get block ~ ~ ~ RecordItem.tag.Memory 1"));
			code.push(format!("scoreboard players operation {dst} %= %%256 reg"));
		} else {
			todo!("{:X?} {:X?} {:X?} {:X?} {:X?}", addr.1, o, z, y, x);
		}
	} else {
		code.push(format!("scoreboard players operation %ptr reg = {addr}"));
		code.push("function intrinsic:setptr".to_string());
		code.push("function intrinsic:load_byte".to_string());
		code.push(format!("scoreboard players operation {dst} = %param0%0 reg"));
	}

	if INSERT_MEM_PRINTS {
		code.push(tellraw_mem_load(8, dst, addr.0));
	}
}

// This function must *not* mutate lhs and rhs
fn unsigned_less_than(dst: Register, lhs: Register, rhs: Register, code: &mut Vec<String>) {
	/*
	dst = false
	if lhs < 0 && rhs >= 0 { reg = false }
	if lhs >= 0 && rhs < 0 { reg = true }
	if lhs < 0 && rhs < 0 && lhs < rhs { reg = true }
	if lhs >= 0 && rhs >= 0 && lhs < rhs { reg = true }
	*/

	if let Some(r) = rhs.get_const() {
		assert_ne!(dst, lhs);

		if r > 0 {
			code.push(format!("scoreboard players set {dst} 0"));
			code.push(format!("execute if score {lhs} matches 0.. if score {lhs} < {rhs} run scoreboard players set {dst} 1"));
		} else {
			assert_ne!(r, i32::MIN);
			let r_incl = r - 1;
			code.push(format!("execute store success score {dst} if score {lhs} matches 0.."));
			code.push(format!("execute if score {lhs} matches ..-1 if score {lhs} matches ..{r_incl} run scoreboard players set {dst} 1"));
		}
	} else if let Some(l) = lhs.get_const() {
		assert_ne!(dst, rhs);

		if l == i32::MAX {
			code.push(format!("execute store success score {dst} if score {rhs} matches ..-1"));
		} else if l >= 0 {
			code.push(format!("execute store success score {dst} unless score {rhs} matches 0..{l}"));
		} else {
			let l_incl = l + 1;
			code.push(format!("scoreboard players set {dst} 0"));
			code.push(format!("execute if score {rhs} matches ..-1 if score {rhs} matches {l_incl}.. run scoreboard players set {dst} 1"));
		}
	} else {
		assert_ne!(dst, lhs);
		assert_ne!(dst, rhs);

		code.push(format!("scoreboard players set {dst} 0"));
		code.push(format!("execute if score {lhs} matches 0.. if score {rhs} matches ..-1 run scoreboard players set {dst} 1"));
		code.push(format!("execute if score {lhs} matches ..-1 if score {rhs} matches ..-1 if score {lhs} < {rhs} run scoreboard players set {dst} 1"));
		code.push(format!("execute if score {lhs} matches 0.. if score {rhs} matches 0.. if score {lhs} < {rhs} run scoreboard players set {dst} 1"));
	}
}

fn unsigned_less_than_eq(dst: Register, lhs: Register, rhs: Register, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	// FIXME: Add a test for this (previously I had r_value - 1, which was a bug)

	if let Some(r) = rhs.get_const() {
		let r_value = r as u32;
		if r_value == u32::MAX {
			// n <= u32::MAX is always true; this should be caught in const prop!
			panic!("missed optimization in const propogation: n <= u32::MAX");
		} else if r_value == 0 {
			// n <= 0 for unsigned values is only true if n == 0
			code.push(format!("execute store success score {dst} if score {lhs} matches 0"));
		} else {
			let new_r = (r_value + 1) as i32;
			const_pool.insert(new_r);
			unsigned_less_than(dst, lhs, Register::const_val(new_r), code);
		}
	} else if let Some(l) = lhs.get_const() {
		let l_value = l as u32;
		if l == 0 {
			// 0 <= n is always true; this should be caught in const prop!
			panic!("missed optimization in const propogation: 0 <= n");
		} else {
			let new_l = (l_value - 1) as i32;
			const_pool.insert(new_l);
			unsigned_less_than(dst, Register::const_val(new_l), rhs, code);
		}
	} else {
		unsigned_less_than(dst, lhs, rhs, code);
		// TODO: This assumes lhs and rhs are not mutated
		code.push(format!("execute if score {dst} matches 0 run execute store success score {dst} if score {lhs} = {rhs}"));
	}
}

fn unsigned_greater_than_eq(dst: Register, lhs: Register, rhs: Register, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	unsigned_less_than_eq(dst, rhs, lhs, code, const_pool); /* swapped */
}

fn signed_div(dst: Register, mut lhs: Register, mut rhs: Register, code: &mut Vec<String>) {
	if lhs == dst {
		let new_lhs = Register::temp_lo(22);
		code.push(format!("scoreboard players operation {new_lhs} = {lhs}"));
		lhs = new_lhs;
	} else {
		code.push(format!("scoreboard players operation {dst} = {lhs}"));
	}

	// Minecraft division always rounds towards negative infinity, so we need to correct for that

	if let Some(r) = rhs.get_const() {
		assert_ne!(r, 0);
		if r > 0 {
			// TODO: Find a better way to pick what register this is
			let rem = Register::temp_lo(21);

			code.push(format!("scoreboard players operation {rem} = {lhs}"));
			code.push(format!("scoreboard players operation {rem} %= {rhs}"));

			code.push(format!("scoreboard players operation {dst} /= {rhs}"));

			code.push(format!("execute if score {lhs} matches ..-1 unless score {rem} matches 0 run scoreboard players add {dst} 1"));
		} else {
			// Minecraft division always rounds towards negative infinity, so we need to correct for that

			// TODO: Find a better way to pick what register this is
			let rem = Register::temp_lo(21);

			code.push(format!("scoreboard players operation {rem} = {lhs}"));
			code.push(format!("scoreboard players operation {rem} %= {rhs}"));

			code.push(format!("scoreboard players operation {dst} /= {rhs}"));

			code.push(format!("execute if score {lhs} matches 0.. unless score {rem} matches 0 run scoreboard players add {dst} 1"));
		}
	} else {
		if rhs == dst {
			let new_rhs= Register::temp_lo(23);
			code.push(format!("scoreboard players operation {new_rhs} = {rhs}"));
			rhs = new_rhs;
		}

		// Minecraft division always rounds towards negative infinity, so we need to correct for that

		// TODO: Find a better way to pick what register this is
		let rem = Register::temp_lo(21);

		code.push(format!("scoreboard players operation {rem} = {lhs}"));
		code.push(format!("scoreboard players operation {rem} %= {rhs}"));

		code.push(format!("scoreboard players operation {dst} = {lhs}"));
		code.push(format!("scoreboard players operation {dst} /= {rhs}"));

		code.push(format!("execute if score {lhs} matches ..-1 if score {rhs} matches 0.. unless score {rem} matches 0..0 run scoreboard players add {dst} 1"));
		code.push(format!("execute if score {rhs} matches ..-1 if score {lhs} matches 0.. unless score {rem} matches 0..0 run scoreboard players add {dst} 1"));
	}
}

fn unsigned_div(dst: Register, mut lhs: Register, mut rhs: Register, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	// TODO: Find a better way to pick these registers
	let d1 = Register::temp_lo(30);
	let r1 = Register::temp_lo(31);
	let d2 = Register::temp_lo(32);
	let r2 = Register::temp_lo(33);
	let d3 = Register::temp_lo(34);
	let is_gtu = Register::temp_lo(35);
	let lhs_lo = Register::temp_lo(36);

	if lhs == dst {
		let new_lhs = Register::temp_lo(37);
		code.push(format!("scoreboard players operation {new_lhs} = {lhs}"));
		lhs = new_lhs;
	}

	if rhs == dst {
		let new_rhs = Register::temp_hi(38);
		code.push(format!("scoreboard players operation {new_rhs} = {rhs}"));
		rhs = new_rhs;
	}

	assert_ne!(lhs, dst);
	assert_ne!(rhs, dst);

	// let mut dst = 0;
	code.push(format!("scoreboard players set {dst} 0"));

	// if lhs >= 0 && rhs >= 0 { dst = lhs / rhs }
	code.push(format!("execute if score {lhs} matches 0.. if score {rhs} matches 0.. run scoreboard players operation {dst} = {lhs}"));
	code.push(format!("execute if score {lhs} matches 0.. if score {rhs} matches 0.. run scoreboard players operation {dst} /= {rhs}"));

	// is_gtu = (lhs as u32) >= (rhs as u32)
	unsigned_greater_than_eq(is_gtu, lhs, rhs, code, const_pool);

	// if lhs < 0 && rhs < 0 && is_gtu { dst = 1 }
	code.push(format!("execute if score {lhs} matches ..-1 if score {rhs} matches ..-1 if score {is_gtu} matches 1..1 run scoreboard players set {dst} 1"));

	// lhs_lo = lhs & 0x7F
	code.push(format!("scoreboard players operation {lhs_lo} = {lhs}"));
	code.push(format!("scoreboard players operation {lhs_lo} += %%-2147483648 reg"));

	// d1 = lhs_lo / rhs
	code.push(format!("scoreboard players operation {d1} = {lhs_lo}"));
	code.push(format!("scoreboard players operation {d1} /= {rhs}"));
	// r1 = lhs_lo % rhs
	code.push(format!("scoreboard players operation {r1} = {lhs_lo}"));
	code.push(format!("scoreboard players operation {r1} %= {rhs}"));

	// d2 = i32::MAX / rhs
	code.push(format!("scoreboard players set {d2} {}", i32::MAX));
	code.push(format!("scoreboard players operation {d2} /= {rhs}"));
	// r2 = i32::MAX % rhs
	code.push(format!("scoreboard players set {r2} {}", i32::MAX));
	code.push(format!("scoreboard players operation {r2} %= {rhs}"));

	// r1 += r2
	code.push(format!("scoreboard players operation {r1} += {r2}"));
	// r1 += 1
	code.push(format!("scoreboard players add {r1} 1"));

	// d3 = r1 / rhs
	code.push(format!("scoreboard players operation {d3} = {r1}"));
	code.push(format!("scoreboard players operation {d3} /= {rhs}"));

	// d1 += d2
	code.push(format!("scoreboard players operation {d1} += {d2}"));
	// d1 += d3
	code.push(format!("scoreboard players operation {d1} += {d3}"));

	code.push(format!("execute if score {lhs} matches ..-1 if score {rhs} matches 0.. run scoreboard players operation {dst} = {d1}"));
}

fn unsigned_rem(dst: Register, lhs: Register, rhs: Register, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	assert_ne!(lhs, dst);
	assert_ne!(rhs, dst);

	unsigned_div(dst, lhs, rhs, code, const_pool);

	// lhs - dst * rhs

	code.push(format!("scoreboard players operation {dst} *= {rhs}"));
	code.push(format!("scoreboard players operation {dst} *= %%-1 reg"));
	code.push(format!("scoreboard players operation {dst} += {lhs}"));
}

fn signed_rem(dst: Register, lhs: Register, rhs: Register, code: &mut Vec<String>) {
	assert_ne!(lhs, dst);

	if let Some(r) = rhs.get_const() {
		if r > 0 {
			code.push(format!("scoreboard players operation {dst} = {lhs}"));
			code.push(format!("scoreboard players operation {dst} %= {rhs}"));
			code.push(format!("execute if score {lhs} matches ..-1 unless score {dst} matches 0 run scoreboard players remove {dst} {r}"));
		} else {
			todo!()
		}
	} else {
		assert_ne!(rhs, dst);

		signed_div(dst, lhs, rhs, code);

		// lhs - dst * rhs

		code.push(format!("scoreboard players operation {dst} *= {rhs}"));
		code.push(format!("scoreboard players operation {dst} *= %%-1 reg"));
		code.push(format!("scoreboard players operation {dst} += {lhs}"));
	}
}

fn add_i64_copying(dst: DoubleRegister, lhs: DoubleRegister, rhs: DoubleRegister, code: &mut Vec<String>) {
	if dst == lhs || dst == rhs {
		// TODO: Better way to alloc this register?
		let tmp_dst = DoubleRegister::temp(11);
		add_i64(tmp_dst, lhs, rhs, code);
		code.push(format!("scoreboard players operation {} = {}", dst.lo(), tmp_dst.lo()));
		code.push(format!("scoreboard players operation {} = {}", dst.hi(), tmp_dst.hi()));
	} else {
		add_i64(dst, lhs, rhs, code);
	}
}

fn add_i64(dst: DoubleRegister, lhs: DoubleRegister, rhs: DoubleRegister, code: &mut Vec<String>) {
	// TODO: Better way to alloc this register?
	let carry = Register::temp_lo(10);

	assert_ne!(dst.lo(), carry);
	assert_ne!(lhs.lo(), carry);
	assert_ne!(rhs.lo(), carry);
	assert_ne!(dst, lhs);
	assert_ne!(dst, rhs);
	assert_ne!(lhs, rhs);

	let dst_lo = dst.lo();
	let dst_hi = dst.hi();

	let lhs_lo = lhs.lo();
	let lhs_hi = lhs.hi();

	let rhs_lo = rhs.lo();
	let rhs_hi = rhs.hi();

	code.push(format!("scoreboard players operation {dst_lo} = {lhs_lo}"));
	code.push(format!("scoreboard players operation {dst_hi} = {lhs_hi}"));

	code.push(format!("scoreboard players operation {dst_lo} += {rhs_lo}"));
	code.push(format!("scoreboard players operation {dst_hi} += {rhs_hi}"));

	/*
		Carrying:

		if lhs < 0 && rhs < 0 {
		true
		} else if lhs < 0 && rhs >= 0 {
		lhs + rhs >= 0
		} else if lhs >= 0 && rhs < 0 {
		lhs + rhs >= 0
		} else {
		false
		}
	
	*/

	code.push(format!("scoreboard players set {carry} 0"));
	code.push(format!("execute if score {lhs_lo} matches ..-1 if score {rhs_lo} matches ..-1 run scoreboard players set {carry} 1"));
	code.push(format!("execute if score {lhs_lo} matches ..-1 if score {rhs_lo} matches 0.. if score {dst_lo} matches 0.. run scoreboard players set {carry} 1"));
	code.push(format!("execute if score {lhs_lo} matches 0.. if score {rhs_lo} matches ..-1 if score {dst_lo} matches 0.. run scoreboard players set {carry} 1"));

	code.push(format!("scoreboard players operation {dst_hi} += {carry}"));
}

fn unsigned_less_than_64(dst: Register, lhs: DoubleRegister, rhs: DoubleRegister, code: &mut Vec<String>) {

	let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
	let (rhs_lo, rhs_hi) = rhs.split_lo_hi();

	assert_ne!(dst, lhs_lo);
	assert_ne!(dst, rhs_lo);
	assert_ne!(dst, lhs_hi);
	assert_ne!(dst, rhs_hi);

	/*
		if lhs_hi ltu rhs_hi:
		return true
		if lhs_hi gtu rhs_hi:
		return false
		if lhs_hi == rhs_hi:
		return x_lo ltu y_lo
	*/

	// TODO: Find a better way to allocate these registers
	let hi_is_lesser = Register::temp_lo(3);
	let hi_is_greater = Register::temp_lo(4);
	let hi_is_equal = Register::temp_lo(5);
	let lo_is_lesser = Register::temp_lo(6);

	// TODO: Check this
	//assert!(!matches!(lhs, Register::Temp(3 | 4 | 5 | 6)));
	//assert!(!matches!(rhs, Register::Temp(3 | 4 | 5 | 6)));
	//assert!(!matches!(dst, Register::Temp(3 | 4 | 5 | 6)));

	unsigned_less_than(hi_is_lesser, lhs_hi, rhs_hi, code);
	unsigned_less_than(hi_is_greater, rhs_hi, lhs_hi, code);
	code.push(format!("execute store success score {hi_is_equal} if score {lhs_hi} = {rhs_hi}"));

	unsigned_less_than(lo_is_lesser, lhs_lo, rhs_lo, code);

	code.push(format!("execute if score {hi_is_lesser} matches 1.. run scoreboard players set {dst} 1"));
	code.push(format!("execute if score {hi_is_greater} matches 1.. run scoreboard players set {dst} 0"));
	code.push(format!("execute if score {hi_is_equal} matches 1.. run scoreboard players operation {dst} = {lo_is_lesser}"));
}

fn unsigned_less_than_eq_64(dst: Register, lhs: DoubleRegister, rhs: DoubleRegister, code: &mut Vec<String>) {
	let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
	let (rhs_lo, rhs_hi) = rhs.split_lo_hi();

	unsigned_less_than_64(dst, lhs, rhs, code);
	code.push(format!("execute if score {lhs_lo} = {rhs_lo} run execute if score {lhs_hi} = {rhs_hi} run scoreboard players set {dst} 1"));
}

fn unsigned_greater_than_64(dst: Register, lhs: DoubleRegister, rhs: DoubleRegister, code: &mut Vec<String>) {
	unsigned_less_than_64(dst, rhs, lhs, code)
}

fn unsigned_greater_than_eq_64(dst: Register, lhs: DoubleRegister, rhs: DoubleRegister, code: &mut Vec<String>) {
	unsigned_less_than_eq_64(dst, rhs, lhs, code)
}

fn signed_less_than_64(dst: Register, lhs: DoubleRegister, rhs: DoubleRegister, code: &mut Vec<String>) {
	/*
		if lhs_hi ltu rhs_hi:
			return true
		if lhs_hi gtu rhs_hi:
			return false
		if lhs_hi == rhs_hi:
			return x_lo ltu y_lo

		As written out normally:

		if lhs_hi < rhs_hi {
			true
		} else if lhs_hi > rhs_hi {
			false
		} else {
			(lhs_lo as u32) < (rhs_lo as u32)
		}
	*/

	let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
	let (rhs_lo, rhs_hi) = rhs.split_lo_hi();

	assert_ne!(dst, lhs_lo);
	assert_ne!(dst, lhs_hi);
	assert_ne!(dst, rhs_lo);
	assert_ne!(dst, rhs_hi);

	// dst = (lhs_lo as u32) < (rhs_lo as u32);
	unsigned_less_than(dst, lhs_lo, rhs_lo, code);
	// if lhs_hi < rhs_hi { dst = true }
	code.push(format!("execute if score {lhs_hi} < {rhs_hi} run scoreboard players set {dst} 1"));
	// if lhs_hi > rhs_hi { dst = false }
	code.push(format!("execute if score {lhs_hi} > {rhs_hi} run scoreboard players set {dst} 0"));
}

fn signed_less_than_eq_64(dst: Register, lhs: DoubleRegister, rhs: DoubleRegister, code: &mut Vec<String>) {
	let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
	let (rhs_lo, rhs_hi) = rhs.split_lo_hi();

	signed_less_than_64(dst, lhs, rhs, code);
	code.push(format!("execute if score {lhs_lo} = {rhs_lo} run execute if score {lhs_hi} = {rhs_hi} run scoreboard players set {dst} 1"));
}

fn signed_greater_than_64(dst: Register, lhs: DoubleRegister, rhs: DoubleRegister, code: &mut Vec<String>) {
	signed_less_than_64(dst, rhs, lhs, code)
}

fn signed_greater_than_eq_64(dst: Register, lhs: DoubleRegister, rhs: DoubleRegister, code: &mut Vec<String>) {
	signed_less_than_eq_64(dst, rhs, lhs, code)
}

static BLOCKS: [&str; 15] = [
	"minecraft:air",
	"minecraft:cobblestone",
	"minecraft:granite",
	"minecraft:andesite",
	"minecraft:diorite",
	"minecraft:lapis_block",
	"minecraft:iron_block",
	"minecraft:gold_block",
	"minecraft:diamond_block",
	"minecraft:redstone_block",
	"minecraft:emerald_block",
	"minecraft:dirt",
	"minecraft:oak_wood",
	"minecraft:green_wool", // FIXME: THIS IS LEAVES
	"minecraft:coal_block",
];

fn turtle_set_block(reg: Register, code: &mut Vec<String>) {
	for (idx, block) in BLOCKS.iter().enumerate() {
		// TODO: Replace or destroy?
		code.push(format!("execute at @e[tag=turtle] if score {reg} matches {idx} run setblock ~ ~ ~ {block} replace"));
	}

	let reg_name = reg.scoreboard_pair().0;

	let mut s = format!("execute unless score {reg} matches 0..{} run ", BLOCKS.len() - 1);
	s.push_str(r#"tellraw @a [{"text":"Attempt to set invalid block"},{"score":{"name":""#);
	s.push_str(reg_name.as_ref());
	s.push_str(r#"","objective":"reg"}}]"#);
	code.push(s);

}

fn turtle_fill_block(block: Register, x_span: Register, y_span: Register, z_span: Register, code: &mut Vec<String>) {
	if let (Some(x_span), Some(y_span), Some(z_span)) = (x_span.get_const(), y_span.get_const(), z_span.get_const()) {
		if let Some(block) = block.get_const() {
			let block = BLOCKS[block as usize];
			code.push(format!("execute at @e[tag=turtle] run fill ~ ~ ~ ~{x_span} ~{y_span} ~{z_span} {block} replace"));
		} else {
			for (idx, block_name) in BLOCKS.iter().enumerate() {
				// TODO: Replace or destroy?
				code.push(format!("execute at @e[tag=turtle] if score {block} matches {idx} run fill ~ ~ ~ ~{x_span} ~{y_span} ~{z_span} {block_name} replace"));
			}
		}
	} else {
		todo!()
	}

	/*
	for (idx, block) in BLOCKS.iter().enumerate() {
		// TODO: Replace or destroy?
		code.push(format!("execute at @e[tag=turtle] if score {reg} matches {idx} run setblock ~ ~ ~ {block} replace"));
	}

	let reg_name = reg.scoreboard_pair().0;

	let mut s = format!("execute unless score {reg} matches 0..{} run ", BLOCKS.len() - 1);
	s.push_str(r#"tellraw @a [{"text":"Attempt to set invalid block"},{"score":{"name":""#);
	s.push_str(reg_name.as_ref());
	s.push_str(r#"","objective":"reg"}}]"#);
	code.push(s);
	*/

}

fn turtle_paste_region_masked(x_span: Register, y_span: Register, z_span: Register, code: &mut Vec<String>) {
	if let (Some(x_span), Some(y_span), Some(z_span)) = (x_span.get_const(), y_span.get_const(), z_span.get_const()) {
		let x_end = x_span;
		let y_end = y_span;
		let z_end = -1 + z_span;
		code.push(format!("execute at @e[tag=turtle] run clone 0 0 -1 {x_end} {y_end} {z_end} ~ ~ ~ masked"));
	} else {
		todo!()
	}
}

fn turtle_copy_region(x_span: Register, y_span: Register, z_span: Register, code: &mut Vec<String>) {
	if let (Some(x_span), Some(y_span), Some(z_span)) = (x_span.get_const(), y_span.get_const(), z_span.get_const()) {
		code.push(format!("execute at @e[tag=turtle] run clone ~ ~ ~ ~{x_span} ~{y_span} ~{z_span} 0 0 -1"));
	} else {
		todo!()
	}

	/*
	for (idx, block) in BLOCKS.iter().enumerate() {
		// TODO: Replace or destroy?
		code.push(format!("execute at @e[tag=turtle] if score {reg} matches {idx} run setblock ~ ~ ~ {block} replace"));
	}

	let reg_name = reg.scoreboard_pair().0;

	let mut s = format!("execute unless score {reg} matches 0..{} run ", BLOCKS.len() - 1);
	s.push_str(r#"tellraw @a [{"text":"Attempt to set invalid block"},{"score":{"name":""#);
	s.push_str(reg_name.as_ref());
	s.push_str(r#"","objective":"reg"}}]"#);
	code.push(s);
	*/

}



fn turtle_get_block(reg: Register, code: &mut Vec<String>) {
	code.push(format!("scoreboard players set {reg} 0"));
	for (idx, block) in BLOCKS.iter().enumerate() {
		code.push(format!("execute at @e[tag=turtle] run execute if block ~ ~ ~ {block} run scoreboard players set {reg} {idx}"));
	}
}

fn get_all_bit_runs(mut value: i32) -> impl Iterator<Item=Range<u32>> {
	std::iter::from_fn(move || {
		let run = get_first_bit_run(value)?;

		value &= !bit_run_to_mask(run.clone());

		Some(run)
	})
}

fn get_first_bit_run(value: i32) -> Option<Range<u32>> {
	if value == 0 {
		None
	} else {
		let start = value.trailing_zeros();

		for i in start..32 {
			if value & (1 << i) == 0 {
				return Some(start..i)
			}
		}

		Some(start..32)
	}
}

fn get_single_bit_run(value: i32) -> Option<Range<u32>> {
	let mut bit_runs = get_all_bit_runs(value);

	if let Some(run) = bit_runs.next() {
		if bit_runs.next().is_none() {
			return Some(run)
		}
	}

	None
}

fn bit_run_to_mask(run: Range<u32>) -> i32 {
	let mut v = 0;
	for i in run {
		v |= 1 << i;
	}
	v
}

fn emit_constant_or(dst: Register, lhs: RegisterWithInfo, rhs: i32, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	let RegisterWithInfo(lhs, _lhs_info) = lhs;

	code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
	code.push(format!("scoreboard players set %param1%0 reg {rhs}"));
	code.push("function intrinsic:or".to_string());
	code.push(format!("scoreboard players operation {dst} = %return%0 reg"));

	return;


	if dst != lhs {
		code.push(format!("scoreboard players operation {dst} = {lhs}"));
	}

	if rhs == -1 || rhs == 0 {
		if rhs == -1 {
			code.push(format!("scoreboard players set {dst} -1"));
		} else {
		}
		//todo!("missed optimization in const prop");
	} else if rhs == i32::MIN {
		code.push(format!("execute if score {dst} matches 0.. run scoreboard players operation {dst} reg += %%{}reg", i32::MIN));
	} else if rhs.count_ones() == 1 {
		let shift = rhs.trailing_zeros();
		let thresh = 1 << shift;
		let mask = 1 << (shift + 1);

		const_pool.insert(mask);
		let mask = Register::const_val(mask);

		let tmp_dst = Register::temp_lo(1235);

		code.push(format!("# ({}) = ({}) | ({:#X})", dst, lhs, rhs));
		code.push(format!("scoreboard players operation {tmp_dst} = {lhs}"));
		code.push(format!("scoreboard players operation {tmp_dst} %= {mask}"));
		code.push(format!("execute unless score {tmp_dst} matches {thresh}.. run scoreboard players add {dst} {thresh}"));
	} else if rhs.leading_zeros() + rhs.trailing_ones() == 32 {
		let mask = 1 << rhs.trailing_ones();

		let tmp_low_bits = Register::temp_lo(1235);

		code.push(format!("# ({}) = ({}) | ({:#X})", dst, lhs, rhs));
		code.push(format!("scoreboard players operation {tmp_low_bits} = {dst}"));
		code.push(format!("scoreboard players operation {tmp_low_bits} %= %%{mask} reg"));
		code.push(format!("scoreboard players operation {dst} -= {tmp_low_bits}"));
		code.push(format!("scoreboard players add {dst} {rhs}"));
	} else if rhs.leading_ones() + rhs.trailing_zeros() == 32 {
		let mask = 1 << rhs.trailing_zeros();

		code.push(format!("# ({}) = ({}) | ({:#X})", dst, lhs, rhs));
		code.push(format!("scoreboard players operation {dst} %= %%{mask} reg"));
		code.push(format!("scoreboard players remove {dst} {}", -rhs));
	} else {
		code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
		code.push(format!("scoreboard players set %param1%0 reg {rhs}"));
		code.push("function intrinsic:or".to_string());
		code.push(format!("scoreboard players operation {dst} = %return%0 reg"));

		/* FIXME: THIS IS BROKEN. DO NOT USE.

		let bit_runs = get_all_bit_runs(rhs).collect::<Vec<_>>();
		if bit_runs.len() == 1 {
			let run = bit_runs[0].clone();

			let tmp_dst = Register::temp_lo(1235);
			let tmp_dst2 = Register::temp_lo(1236);

			code.push(format!("scoreboard players operation {tmp_dst} = {dst}"));
			code.push(format!("scoreboard players operation {tmp_dst} %= %%{} reg", 1 << run.start));

			code.push(format!("scoreboard players operation {tmp_dst2} = {dst}"));
			code.push(format!("scoreboard players operation {tmp_dst2} %= %%{} reg", 1 << (run.end + 1)));
			code.push(format!("scoreboard players operation {tmp_dst2} -= {tmp_dst}"));

			code.push(format!("scoreboard players operation {dst} -= {tmp_dst}"));
			code.push(format!("scoreboard players add {dst} {rhs}"));
		} else {
			//todo!("{:X?} {:#X}", lhs_info, rhs);
			// TODO:
			code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
			code.push(format!("scoreboard players set %param1%0 reg {rhs}"));
			code.push("function intrinsic:or".to_string());
			code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
		}

		*/
	}
}

fn emit_constant_and(dst: Register, lhs: RegisterWithInfo, rhs: i32, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	if rhs == 0 {
		code.push(format!("scoreboard players set {dst} 0"));
	} else if rhs == -1 {
		if dst != lhs.0 {
			code.push(format!("scoreboard players operation {dst} = {lhs}"));
		}
	} else if rhs.leading_zeros() + rhs.trailing_ones() == 32 {
		let bit_run = get_single_bit_run(rhs).unwrap();
		
		assert_eq!(bit_run.start, 0);

		if dst != lhs.0 {
			code.push(format!("scoreboard players operation {dst} = {lhs}"));
		}

		if bit_run.end == 32 {
			// Do nothing, because we don't have to zero out any high bits
		} else if bit_run.end == 31 {
			let int_min = i32::MIN;
			const_pool.insert(int_min);
			let int_min = Register::const_val(int_min);

			// Zero out just the high bit
			code.push(format!("execute if score {dst} matches ..-1 run scoreboard players operation {dst} -= {int_min}"));
		} else if bit_run.end < 31 {
			// 0111
			// end == 3
			let modulus = 1 << bit_run.end;
			const_pool.insert(modulus);
			let modulus = Register::const_val(modulus);
			code.push(format!("scoreboard players operation {dst} %= {modulus}"));
		}
	} else {
		code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
		code.push(format!("scoreboard players set %param1%0 reg {rhs}"));
		code.push("function intrinsic:and".to_string());
		code.push(format!("scoreboard players operation {dst} = %return%0 reg"));

		/*
		FIXME: THIS IS BROKEN. DO NOT USE.
		code.push(format!("# ({}) = ({}) & ({:#X})", dst, lhs, rhs));

		let tmp_dst = Register::temp_lo(1235);

		code.push(format!("scoreboard players set {dst} 0"));
		for bit_run in get_all_bit_runs(rhs) {
			assert!(bit_run.end > bit_run.start);

			if bit_run.start == 31 {
				let int_min = i32::MIN;
				const_pool.insert(int_min);
				let int_min = Register::const_val(int_min);
				code.push(format!("execute if score {lhs} matches ..-1 run scoreboard players operation {dst} += {int_min}"));
				continue;
			}

			code.push(format!("scoreboard players operation {tmp_dst} = {lhs}"));
			if bit_run.end == 32 {
				// Do nothing, because we don't have to zero out any high bits
			} else if bit_run.end == 31 {
				let int_min = i32::MIN;
				const_pool.insert(int_min);
				let int_min = Register::const_val(int_min);

				// Zero out just the high bit
				code.push(format!("execute if score {tmp_dst} matches ..-1 run scoreboard players operation {tmp_dst} -= {int_min}"));
			} else if bit_run.end < 31 {
				// 0111
				// end == 3
				let modulus = 1 << bit_run.end;
				const_pool.insert(modulus);
				let modulus = Register::const_val(modulus);
				code.push(format!("scoreboard players operation {tmp_dst} %= {modulus}"));
			}

			if bit_run.start > 0 {
				// 1110
				// start == 1
				let tmp = Register::temp_lo(1234);
				code.push(format!("scoreboard players operation {tmp} = {lhs}"));
				
				let modulus = 1 << bit_run.start;
				const_pool.insert(modulus);
				let modulus = Register::const_val(modulus);
				code.push(format!("scoreboard players operation {tmp} %= {modulus}"));
				code.push(format!("scoreboard players operation {tmp_dst} -= {tmp}"));
			}

			code.push(format!("scoreboard players operation {dst} += {tmp_dst}"));
		}
		*/
	}
}

// Returns the remaining mask that must be handled, if successful.
fn emit_xor_low_bits(reg: Register, rhs: i32, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) -> Result<i32, ()> {
	if rhs.trailing_ones() <= 1 || rhs.trailing_ones() >= 31 {
		return Err(())
	}

	let lo_bits = Register::temp_lo(1235);

	let lo_bits_mod = 1 << rhs.trailing_ones();
	const_pool.insert(lo_bits_mod);
	let lo_bits_mod_reg = Register::const_val(lo_bits_mod);

	// First, zero out the bits we're going to XOR, the low bits.
	code.push(format!("scoreboard players operation {lo_bits} = {reg}"));
	code.push(format!("scoreboard players operation {lo_bits} %= {lo_bits_mod_reg}"));
	code.push(format!("scoreboard players operation {reg} -= {lo_bits}"));

	// Then, bitwise invert (XOR) the low bits, and mask off anything extra.
	const_pool.insert(-1);
	let neg_one = Register::const_val(-1);
	code.push(format!("scoreboard players operation {lo_bits} *= {neg_one}"));
	code.push(format!("scoreboard players remove {lo_bits} 1"));
	code.push(format!("scoreboard players operation {lo_bits} %= {lo_bits_mod_reg}"));

	// Then, add the low part and the high part back together.
	code.push(format!("scoreboard players operation {reg} += {lo_bits}"));

	Ok(rhs & !(lo_bits_mod - 1))
}

fn emit_constant_xor(dst: Register, lhs: RegisterWithInfo, mut rhs: i32, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	let old_code_len = code.len();
	let old_rhs = rhs;

	if dst != lhs.0 {
		code.push(format!("scoreboard players operation {dst} = {lhs}"));
	}

	if rhs == 0 {
		return;
	}

	if rhs == -1 {
		// (-x - 1) = (~x)
		const_pool.insert(-1);
		let neg_one = Register::const_val(-1);
		code.push(format!("scoreboard players operation {dst} *= {neg_one}"));
		code.push(format!("scoreboard players remove {dst} 1"));
		return;
	}

	if rhs == 1 << 31 {
		// TODO: Add a regresstion test; this didn't work because it tried to add a negative number.

		// This adds (1 << 31), but it has to be split into two parts.
		code.push(format!("scoreboard players add {dst} {}", i32::MAX));
		code.push(format!("scoreboard players add {dst} 1"));
		return;
	}
	
	if let Ok(new_rhs) = emit_xor_low_bits(dst, rhs, code, const_pool) {
		rhs = new_rhs;
	}

	if rhs == 0 {
		return;
	}

	if rhs.count_ones() == 1 && rhs != (1 << 30) {
		let shift = rhs.trailing_zeros();
		let thresh = 1 << shift;
		let mask = 1 << (shift + 1);

		const_pool.insert(mask);
		let mask = Register::const_val(mask);

		let tmp_dst = Register::temp_lo(1235);
		let bit_is_set = Register::temp_lo(1236);

		code.push(format!("scoreboard players operation {tmp_dst} = {lhs}"));
		code.push(format!("scoreboard players operation {tmp_dst} %= {mask}"));
		code.push(format!("execute store success score {bit_is_set} if score {tmp_dst} matches {thresh}.."));
		code.push(format!("execute if score {bit_is_set} matches 1 run scoreboard players remove {dst} {thresh}"));
		code.push(format!("execute if score {bit_is_set} matches 0 run scoreboard players add {dst} {thresh}"));
		return;
	}

	// We can't optimize this XOR, so get rid of all the parts we tried to do.
	code.truncate(old_code_len);
	
	// TODO:
	code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
	code.push(format!("scoreboard players set %param1%0 reg {old_rhs}"));
	code.push("function intrinsic:xor".to_string());
	code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
}


fn emit_constant_shru(dst: Register, lhs: Register, rhs: i32, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	let rhs = rhs.rem_euclid(32);

	if rhs == 31 {
		code.push(format!("execute store success score {dst} if score {lhs} matches ..-1"));
	} else if rhs > 0 {
		if dst != lhs {
			code.push(format!("scoreboard players operation {dst} = {lhs}"));
		}

		let dst_is_neg = Register::temp_lo(789);

		code.push(format!("execute store success score {dst_is_neg} if score {dst} matches ..-1"));

		// Zero out the high bit, if necessary
		const_pool.insert(i32::MIN);
		let int_min = Register::const_val(i32::MIN);
		code.push(format!("execute if score {dst_is_neg} matches 1 run scoreboard players operation {dst} -= {int_min}"));

		let factor = 1 << rhs;
		const_pool.insert(factor);
		let factor = Register::const_val(factor);

		code.push(format!("scoreboard players operation {dst} /= {factor}"));

		let high_bit_factor = 1 << (31 - rhs);

		code.push(format!("execute if score {dst_is_neg} matches 1 run scoreboard players add {dst} {high_bit_factor}"));
	} else if dst != lhs {
		code.push(format!("scoreboard players operation {dst} = {lhs}"));
	}
}



fn emit_instr(instr: &LirInstr, parent: &LirProgram, code: &mut Vec<String>, const_pool: &mut HashSet<i32>) {
	match instr {
		&LirInstr::Assign(dst, src) => if dst != src { code.push(format!("scoreboard players operation {dst} = {src}")); },
		&LirInstr::Set(dst, src) => code.push(format!("scoreboard players set {dst} {src}")),
		&LirInstr::Add(dst, src) => code.push(format!("scoreboard players operation {dst} += {src}")),
		&LirInstr::Sub(dst, src) => code.push(format!("scoreboard players operation {dst} -= {src}")),
		&LirInstr::Mul(dst, src) => code.push(format!("scoreboard players operation {dst} *= {src}")),
		&LirInstr::DivS(dst, lhs, rhs) => signed_div(dst, lhs, rhs, code),
		&LirInstr::DivU(dst, lhs, rhs) => unsigned_div(dst, lhs, rhs, code, const_pool),
		&LirInstr::RemS(dst, lhs, rhs) => signed_rem(dst, lhs, rhs, code),
		&LirInstr::RemU(dst, lhs, rhs) => unsigned_rem(dst, lhs, rhs, code, const_pool),

		&LirInstr::MulTo64(dst, lhs, rhs) => {
			let (dst_lo, dst_hi) = dst.split_lo_hi();
			code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
			code.push("function intrinsic:mul_32_to_64".to_string());
			code.push(format!("scoreboard players operation {dst_lo} = %return%0 reg"));
			code.push(format!("scoreboard players operation {dst_hi} = %return%1 reg"));
		}

		&LirInstr::Add64(dst, lhs, rhs) => add_i64_copying(dst, lhs, rhs, code),
		&LirInstr::Sub64(dst, lhs, rhs) => {
			// -rhs == ((-rhs_lo - 1), (-rhs_hi - 1)) + 1

			// TODO: Better way to allocate this register?
			let tmp = DoubleRegister::temp(12);
			let (tmp_lo, tmp_hi) = tmp.split_lo_hi();

			let (rhs_lo, rhs_hi) = rhs.split_lo_hi();

			code.push(format!("scoreboard players operation {tmp_lo} = {rhs_lo}"));
			code.push(format!("scoreboard players operation {tmp_hi} = {rhs_hi}"));

			code.push(format!("scoreboard players operation {tmp_lo} *= %%-1 reg"));
			code.push(format!("scoreboard players remove {tmp_lo} 1"));
			code.push(format!("scoreboard players operation {tmp_hi} *= %%-1 reg"));
			code.push(format!("scoreboard players remove {tmp_hi} 1"));

			let all_ones = 0xFFFF_FFFF_u32 as i32;
			code.push(format!("execute if score {tmp_lo} matches {all_ones} run scoreboard players add {tmp_hi} 1"));
			code.push(format!("scoreboard players add {tmp_lo} 1"));

			add_i64_copying(dst, lhs, tmp, code);
		},

		&LirInstr::DivS64(dst, lhs, rhs) => {
			let (p0_lo, p0_hi) = DoubleRegister::param(0).split_lo_hi();
			let (p1_lo, p1_hi) = DoubleRegister::param(1).split_lo_hi();
			let (ret_lo, ret_hi) = DoubleRegister::return_reg(0).split_lo_hi();
			let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
			let (rhs_lo, rhs_hi) = rhs.split_lo_hi();
			let (dst_lo, dst_hi) = dst.split_lo_hi();

			code.push(format!("scoreboard players operation {p0_lo} = {lhs_lo}"));
			code.push(format!("scoreboard players operation {p0_hi} = {lhs_hi}"));
			code.push(format!("scoreboard players operation {p1_lo} = {rhs_lo}"));
			code.push(format!("scoreboard players operation {p1_hi} = {rhs_hi}"));
			code.push("function intrinsic:i64_sdiv".to_string());
			code.push(format!("scoreboard players operation {dst_lo} = {ret_lo}"));
			code.push(format!("scoreboard players operation {dst_hi} = {ret_hi}"));
		}
		&LirInstr::DivU64(dst, lhs, rhs) => {
			let (p0_lo, p0_hi) = DoubleRegister::param(0).split_lo_hi();
			let (p1_lo, p1_hi) = DoubleRegister::param(1).split_lo_hi();
			let (ret_lo, ret_hi) = DoubleRegister::return_reg(0).split_lo_hi();
			let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
			let (rhs_lo, rhs_hi) = rhs.split_lo_hi();
			let (dst_lo, dst_hi) = dst.split_lo_hi();

			code.push(format!("scoreboard players operation {p0_lo} = {lhs_lo}"));
			code.push(format!("scoreboard players operation {p0_hi} = {lhs_hi}"));
			code.push(format!("scoreboard players operation {p1_lo} = {rhs_lo}"));
			code.push(format!("scoreboard players operation {p1_hi} = {rhs_hi}"));
			code.push("function intrinsic:i64_udiv".to_string());
			code.push(format!("scoreboard players operation {dst_lo} = {ret_lo}"));
			code.push(format!("scoreboard players operation {dst_hi} = {ret_hi}"));
		}
		&LirInstr::RemS64(dst, lhs, rhs) => {
			let (p0_lo, p0_hi) = DoubleRegister::param(0).split_lo_hi();
			let (p1_lo, p1_hi) = DoubleRegister::param(1).split_lo_hi();
			let (ret_lo, ret_hi) = DoubleRegister::return_reg(0).split_lo_hi();
			let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
			let (rhs_lo, rhs_hi) = rhs.split_lo_hi();
			let (dst_lo, dst_hi) = dst.split_lo_hi();

			code.push(format!("scoreboard players operation {p0_lo} = {lhs_lo}"));
			code.push(format!("scoreboard players operation {p0_hi} = {lhs_hi}"));
			code.push(format!("scoreboard players operation {p1_lo} = {rhs_lo}"));
			code.push(format!("scoreboard players operation {p1_hi} = {rhs_hi}"));
			code.push("function intrinsic:i64_srem".to_string());
			code.push(format!("scoreboard players operation {dst_lo} = {ret_lo}"));
			code.push(format!("scoreboard players operation {dst_hi} = {ret_hi}"));
		}
		&LirInstr::RemU64(dst, lhs, rhs) => {
			let (p0_lo, p0_hi) = DoubleRegister::param(0).split_lo_hi();
			let (p1_lo, p1_hi) = DoubleRegister::param(1).split_lo_hi();
			let (ret_lo, ret_hi) = DoubleRegister::return_reg(0).split_lo_hi();
			let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
			let (rhs_lo, rhs_hi) = rhs.split_lo_hi();
			let (dst_lo, dst_hi) = dst.split_lo_hi();

			code.push(format!("scoreboard players operation {p0_lo} = {lhs_lo}"));
			code.push(format!("scoreboard players operation {p0_hi} = {lhs_hi}"));
			code.push(format!("scoreboard players operation {p1_lo} = {rhs_lo}"));
			code.push(format!("scoreboard players operation {p1_hi} = {rhs_hi}"));
			code.push("function intrinsic:i64_urem".to_string());
			code.push(format!("scoreboard players operation {dst_lo} = {ret_lo}"));
			code.push(format!("scoreboard players operation {dst_hi} = {ret_hi}"));

		}

		&LirInstr::Shl(dst, lhs, rhs) => {
			if let Some(rhs) = rhs.get_const() {
				let rhs = rhs.rem_euclid(32);

				if dst != lhs {
					code.push(format!("scoreboard players operation {dst} = {lhs}"));
				}

				if rhs != 0 {
					let m = (1_u32 << rhs) as i32;
					const_pool.insert(m);
					let m = Register::const_val(m);
					code.push(format!("scoreboard players operation {dst} *= {m}"));
				}
			} else {
				code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
				code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
				code.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
				code.push("function intrinsic:shl".to_string());
				code.push(format!("scoreboard players operation {dst} = %param0%0 reg"));
			}
		}
		&LirInstr::ShrS(dst, lhs, rhs) => {
			if dst != lhs {
				code.push(format!("scoreboard players operation {dst} = {lhs}"));
			}

			if let Some(rhs) = rhs.get_const() {
				let rhs = rhs.rem_euclid(32);

				if rhs == 31 {
					code.push(format!("execute store success score {dst} if score {dst} matches ..-1"));
					code.push(format!("scoreboard players operation {dst} *= %%-1 reg"));
				} else if rhs != 0 {
					code.push(format!("scoreboard players operation {dst} /= %%{} reg", 1 << rhs));
				}
			} else {
				assert_ne!(rhs, dst);

				code.push(format!("scoreboard players operation {rhs} %= %%32 reg"));

				for i in 1..31 {
					code.push(format!("execute if score {rhs} matches {i} run scoreboard players operation {dst} /= %%{} reg", 1 << i))
				}
				code.push(format!("execute if score {rhs} matches 31 run execute store success score {dst} if score {dst} matches ..-1"));
				code.push(format!("execute if score {rhs} matches 31 run scoreboard players operation {dst} *= %%-1 reg"));
			}
		}
		&LirInstr::ShrU(dst, lhs, rhs) => {
			if let Some(rhs) = rhs.get_const() {
				emit_constant_shru(dst, lhs, rhs, code, const_pool);
			} else {
				code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
				code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
				code.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
				code.push("function intrinsic:lshr".to_string());
				code.push(format!("scoreboard players operation {dst} = %param0%0 reg"));
			}
		}
		&LirInstr::Rotl(dst, lhs, rhs) => {
			code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
			code.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
			code.push("function intrinsic:rotl".to_string());
			code.push(format!("scoreboard players operation {dst} = %param0%0 reg"));
		}
		&LirInstr::Rotr(dst, lhs, rhs) => {
			code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
			code.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
			code.push("function intrinsic:rotr".to_string());
			code.push(format!("scoreboard players operation {dst} = %param0%0 reg"));
		}

		&LirInstr::Shl64(dst, lhs, rhs) => {
			let (dst_lo, dst_hi) = dst.split_lo_hi();
			let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
			let rhs_lo = rhs.lo();
			code.push(format!("scoreboard players operation %param0%0 reg = {lhs_lo}"));
			code.push(format!("scoreboard players operation %param0%1 reg = {lhs_hi}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs_lo}"));
			code.push("scoreboard players operation %param1%0 reg %= %%64 reg".to_string());
			code.push("function intrinsic:shl_64".to_string());
			code.push(format!("scoreboard players operation {dst_lo} = %param0%0 reg"));
			code.push(format!("scoreboard players operation {dst_hi} = %param0%1 reg"));
		}

		&LirInstr::ShrS64(dst, lhs, rhs) => {
			let (dst_lo, dst_hi) = dst.split_lo_hi();
			let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
			let rhs_lo = rhs.lo();

			code.push(format!("scoreboard players operation %param0%0 reg = {lhs_lo}"));
			code.push(format!("scoreboard players operation %param0%1 reg = {lhs_hi}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs_lo}"));
			code.push("scoreboard players operation %param1%0 reg %= %%64 reg".to_string());
			code.push("function intrinsic:ashr_i64".to_string());
			code.push(format!("scoreboard players operation {dst_lo} = %param0%0 reg"));
			code.push(format!("scoreboard players operation {dst_hi} = %param0%1 reg"));
		}
		&LirInstr::ShrU64(dst, lhs, rhs) => {
			let (dst_lo, dst_hi) = dst.split_lo_hi();
			let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
			let rhs_lo = rhs.lo();

			code.push(format!("scoreboard players operation %param0%0 reg = {lhs_lo}"));
			code.push(format!("scoreboard players operation %param0%1 reg = {lhs_hi}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs_lo}"));
			code.push("scoreboard players operation %param1%0 reg %= %%64 reg".to_string());
			code.push("function intrinsic:lshr_i64".to_string());
			code.push(format!("scoreboard players operation {dst_lo} = %param0%0 reg"));
			code.push(format!("scoreboard players operation {dst_hi} = %param0%1 reg"));
		}
		&LirInstr::Rotl64(dst, lhs, rhs) => {
			let (dst_lo, dst_hi) = dst.split_lo_hi();
			let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
			let rhs_lo = rhs.lo();

			code.push(format!("scoreboard players operation %param0%0 reg = {lhs_lo}"));
			code.push(format!("scoreboard players operation %param0%1 reg = {lhs_hi}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs_lo}"));
			code.push("scoreboard players operation %param1%0 reg %= %%64 reg".to_string());
			code.push("function intrinsic:rotl_64".to_string());
			code.push(format!("scoreboard players operation {dst_lo} = %param0%0 reg"));
			code.push(format!("scoreboard players operation {dst_hi} = %param0%1 reg"));
		}
		&LirInstr::Rotr64(dst, lhs, rhs) => {
			let (dst_lo, dst_hi) = dst.split_lo_hi();
			let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
			let rhs_lo = rhs.lo();

			code.push(format!("scoreboard players operation %param0%0 reg = {lhs_lo}"));
			code.push(format!("scoreboard players operation %param0%1 reg = {lhs_hi}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs_lo}"));
			code.push("scoreboard players operation %param1%0 reg %= %%64 reg".to_string());
			code.push("function intrinsic:rotr_64".to_string());
			code.push(format!("scoreboard players operation {dst_lo} = %param0%0 reg"));
			code.push(format!("scoreboard players operation {dst_hi} = %param0%1 reg"));
		}

		&LirInstr::Xor(dst, lhs, rhs) => {
			if let Some(rhs) = rhs.get_const() {
				emit_constant_xor(dst, lhs, rhs, code, const_pool);
			} else {
				code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
				code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
				code.push("function intrinsic:xor".to_string());
				code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
			}
		}
		&LirInstr::And(dst, lhs, rhs) => {
			if let Some(c) = rhs.get_const() {
				emit_constant_and(dst, lhs, c, code, const_pool)
			} else {
				code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
				code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
				code.push("function intrinsic:and".to_string());
				code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
			}
		} 
		&LirInstr::Or(dst, lhs, rhs) => {
			if let Some(c) = rhs.get_const() {
				emit_constant_or(dst, lhs, c, code, const_pool)
			} else {
				code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
				code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
				code.push("function intrinsic:or".to_string());
				code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
			}
		}

		&LirInstr::PopcntAdd(dst, src) => {
			code.push(format!("scoreboard players operation %param0%0 reg = {src}"));
			code.push("function intrinsic:popcnt".to_string());
			code.push(format!("scoreboard players operation {dst} += %return%0 reg"));
		}
		&LirInstr::Ctz(dst, src) => {
			code.push(format!("scoreboard players operation %param0%0 reg = {src}"));
			code.push("function intrinsic:ctz".to_string());
			code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
		}
		&LirInstr::Clz(dst, src) => {
			code.push(format!("scoreboard players operation %param0%0 reg = {src}"));
			code.push("function intrinsic:clz".to_string());
			code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
		}
		&LirInstr::Ctz64(dst, src) => {
			let (dst_lo, dst_hi) = dst.split_lo_hi();
			let (src_lo, src_hi) = src.split_lo_hi();
			assert_ne!(dst_lo, src_lo);
			assert_ne!(dst_hi, src_hi);
			code.push(format!("scoreboard players operation %param0%0 reg = {src_lo}"));
			code.push(format!("execute if score {src_lo} matches 0 run scoreboard players operation %param0%0 reg = {src_hi}"));
			code.push("function intrinsic:ctz".to_string());
			code.push(format!("execute if score {src_lo} matches 0 run scoreboard players add %return%0 reg 32"));
			code.push(format!("scoreboard players operation {dst_lo} = %return%0 reg"));
			code.push(format!("scoreboard players set {dst_hi} 0"));
		},
		&LirInstr::Clz64(dst, src) => {
			let (dst_lo, dst_hi) = dst.split_lo_hi();
			let (src_lo, src_hi) = src.split_lo_hi();
			assert_ne!(dst_lo, src_lo);
			assert_ne!(dst_hi, src_hi);
			code.push(format!("scoreboard players operation %param0%0 reg = {src_hi}"));
			code.push(format!("execute if score {src_hi} matches 0 run scoreboard players operation %param0%0 reg = {src_lo}"));
			code.push("function intrinsic:clz".to_string());
			code.push(format!("execute if score {src_hi} matches 0 run scoreboard players add %return%0 reg 32"));
			code.push(format!("scoreboard players operation {dst_lo} = %return%0 reg"));
			code.push(format!("scoreboard players set {dst_hi} 0"));
		} 
		&LirInstr::Eqz(dst, src) => {
			code.push(format!("execute store success score {dst} if score {src} matches 0"));
		}
		&LirInstr::Eqz64(dst, src) => {
			let src_lo = src.lo();
			let src_hi = src.hi();
			assert_ne!(dst, src_lo);
			code.push(format!("execute store success score {dst} if score {src_lo} matches 0"));
			code.push(format!("execute if score {dst} matches 1 run execute store success score {dst} if score {src_hi} matches 0"));
		}
		
		&LirInstr::GtS(dst, lhs, rhs) => code.push(format!("execute store success score {dst} if score {lhs} > {rhs}")),
		&LirInstr::GeS(dst, lhs, rhs) => code.push(format!("execute store success score {dst} if score {lhs} >= {rhs}")),
		&LirInstr::LtS(dst, lhs, rhs) => code.push(format!("execute store success score {dst} if score {lhs} < {rhs}")),
		&LirInstr::LeS(dst, lhs, rhs) => code.push(format!("execute store success score {dst} if score {lhs} <= {rhs}")),
		&LirInstr::Eq(dst, lhs, rhs) => code.push(format!("execute store success score {dst} if score {lhs} = {rhs}")),
		&LirInstr::Ne(dst, lhs, rhs) => code.push(format!("execute store success score {dst} unless score {lhs} = {rhs}")),

		&LirInstr::GtU(dst, lhs, rhs) => unsigned_less_than(dst, rhs, lhs, code) /* swapped */,
		&LirInstr::GeU(dst, lhs, rhs) => unsigned_greater_than_eq(dst, lhs, rhs, code, const_pool),
		&LirInstr::LtU(dst, lhs, rhs) => unsigned_less_than(dst, lhs, rhs, code),
		&LirInstr::LeU(dst, lhs, rhs) => unsigned_less_than_eq(dst, lhs, rhs, code, const_pool),

		&LirInstr::GtU64(dst, lhs, rhs) => unsigned_greater_than_64(dst, lhs, rhs, code),
		&LirInstr::GeU64(dst, lhs, rhs) => unsigned_greater_than_eq_64(dst, lhs, rhs, code),
		&LirInstr::LtU64(dst, lhs, rhs) => unsigned_less_than_64(dst, lhs, rhs, code),
		&LirInstr::LeU64(dst, lhs, rhs) => unsigned_less_than_eq_64(dst, lhs, rhs, code),

		&LirInstr::GtS64(dst, lhs, rhs) => signed_greater_than_64(dst, lhs, rhs, code),
		&LirInstr::GeS64(dst, lhs, rhs) => signed_greater_than_eq_64(dst, lhs, rhs, code),
		&LirInstr::LtS64(dst, lhs, rhs) => signed_less_than_64(dst, lhs, rhs, code),
		&LirInstr::LeS64(dst, lhs, rhs) => signed_less_than_eq_64(dst, lhs, rhs, code),

		&LirInstr::Eq64(dst, lhs, rhs) => {
			let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
			let (rhs_lo, rhs_hi) = rhs.split_lo_hi();
			assert_ne!(dst, lhs_lo);
			assert_ne!(dst, lhs_hi);
			assert_ne!(dst, rhs_lo);
			assert_ne!(dst, rhs_hi);
			code.push(format!("scoreboard players set {dst} 0"));
			code.push(format!("execute if score {lhs_lo} = {rhs_lo} run execute if score {lhs_hi} = {rhs_hi} run scoreboard players set {dst} 1"));
		},
		&LirInstr::Ne64(dst, lhs, rhs) => {
			let (lhs_lo, lhs_hi) = lhs.split_lo_hi();
			let (rhs_lo, rhs_hi) = rhs.split_lo_hi();
			assert_ne!(dst, lhs_lo);
			assert_ne!(dst, lhs_hi);
			assert_ne!(dst, rhs_lo);
			assert_ne!(dst, rhs_hi);
			code.push(format!("execute store success score {dst} unless score {lhs_lo} = {rhs_lo}"));
			code.push(format!("execute unless score {dst} matches 1 run execute store success score {dst} unless score {lhs_hi} = {rhs_hi}"));
		},

		crate::lir::LirInstr::Trunc(_, _) => todo!(),

		&LirInstr::SignExtend8(reg) => {
			code.push(format!("scoreboard players operation {reg} %= %%{} reg", u8::MAX as i32 + 1));
			let lo_bound = i8::MIN as u8 as u32;
			let hi_bound = u8::MAX as u8 as u32;
			let high_bits = -(u32::MAX as i32 & !(u8::MAX as i32));
			code.push(format!("execute if score {reg} matches {lo_bound}..{hi_bound} run scoreboard players remove {reg} {high_bits}"));
		}
		&LirInstr::SignExtend16(reg) => {
			code.push(format!("scoreboard players operation {reg} %= %%{} reg", u16::MAX as i32 + 1));
			let lo_bound = i16::MIN as u16 as u32;
			let hi_bound = u16::MAX as u16 as u32;
			let high_bits = -(u32::MAX as i32 & !(u16::MAX as i32));
			code.push(format!("execute if score {reg} matches {lo_bound}..{hi_bound} run scoreboard players remove {reg} {high_bits}"));
		}
		&LirInstr::SignExtend32(reg) => {
			let reg_lo = reg.lo();
			let reg_hi = reg.hi();
			code.push(format!("scoreboard players set {reg_hi} 0"));
			code.push(format!("execute if score {reg_lo} matches ..-1 run scoreboard players set {reg_hi} -1"));
		}

		&LirInstr::LocalSet(dst, half, src) => local_set(dst, half, src, code),
		&LirInstr::LocalGet(dst, src, half) => local_get(dst, src, half, code),
		&LirInstr::GlobalSet(dst, half, src) => {
			let reg = if half == Half::Hi { Register::global_hi(dst) } else { Register::global_lo(dst) };
			code.push(format!("scoreboard players operation {reg} = {src}"));
		},
		&LirInstr::GlobalGet(dst, src, half) => {
			let reg = if half == Half::Hi { Register::global_hi(src) } else { Register::global_lo(src) };
			code.push(format!("scoreboard players operation {dst} = {reg}"));
		},
		&LirInstr::Store32(src, addr) => mem_store_32(src, addr, code, const_pool),
		&LirInstr::Store16(src, addr) => mem_store_16(src, addr, code, const_pool),
		&LirInstr::Store8 (src, addr) => mem_store_8 (src, addr, code),
		&LirInstr::Load64(dst, addr) => mem_load_64(dst, addr, code, const_pool),
		&LirInstr::Load32(dst, addr) => mem_load_32(dst, addr, code, const_pool),
		&LirInstr::Load16(dst, addr) => mem_load_16(dst, addr, code),
		&LirInstr::Load8 (dst, addr) => mem_load_8 (dst, addr, code),
		&LirInstr::SignExtend(_, _) => todo!(),
		&LirInstr::Select { dst, true_reg, false_reg, cond } => {
			if dst == true_reg {
				code.push(format!("execute if score {cond} matches 0 run scoreboard players operation {dst} = {false_reg}"));
			} else {
				if dst != false_reg {
					code.push(format!("scoreboard players operation {dst} = {false_reg}"));
				}
				code.push(format!("execute unless score {cond} matches 0 run scoreboard players operation {dst} = {true_reg}"));
			}
		}
		&LirInstr::Call { func } => {
			if jump_mode() == JumpMode::Direct {
				let func_id = get_mc_id(BlockId { func: func as usize, block: 0 });
				code.push(format!("function {func_id}"));
			} else {
				todo!()
			}
		}
		LirInstr::CallIndirect { table, table_entry } => {
			if jump_mode() == JumpMode::Direct {
				let cond_taken = Register::cond_taken();

				code.push(format!("scoreboard players set {cond_taken} 0"));

				for (idx, arm) in table.iter().enumerate() {
					if let Some(arm) = arm {
						let arm_func = get_mc_id(BlockId { func: *arm, block: 0 });
						code.push(format!("execute if score {cond_taken} matches 0 run execute if score {table_entry} matches {idx} run function {arm_func}"));
					}
				}

				code.push(format!("# !INTERPRETER: ASSERT unless score {cond_taken} matches 0"));
				code.push(format!("execute if score {cond_taken} matches 0 run tellraw @a [{{\"text\":\"BAD CALL INDIRECT\"}}]"));
			} else {
				todo!()
			}
		}
		LirInstr::Push(reg) => push_data(reg, code),
		LirInstr::Pop(reg) => pop_data(reg, code),
		LirInstr::IfCond { cond, instr } => {
			let mut child = Vec::new();
			emit_instr(instr, parent, &mut child, const_pool);

			let prefix;
			match cond {
				Condition::Matches(reg, range) => {
					prefix = format!("execute if score {} matches {}..{} run ", reg, range.start(), range.end());
				}
				Condition::NotMatches(reg, range) => {
					prefix = format!("execute unless score {} matches {}..{} run ", reg, range.start(), range.end());
				}
			}

			for cmd in child.iter_mut() {
				*cmd = format!("{prefix}{cmd}");
			}

			code.extend(child);
		}
		LirInstr::PushLocalFrame(ty) => push_local_frame(ty, code),
		LirInstr::PopLocalFrame(ty) => pop_local_frame(ty, code),

		&LirInstr::PushReturnAddr(block_id) => {
			push_return_addr(block_id, code);
		}
		&LirInstr::PopReturnAddr => {
			pop_return_addr(code);
		}

		LirInstr::Memset { dest, value, length, result } => {
			code.push(format!("scoreboard players operation %param0%0 reg = {dest}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {value}"));
			code.push(format!("scoreboard players operation %param2%0 reg = {length}"));
			code.push("function intrinsic:memset".to_string());
			code.push(format!("scoreboard players operation {result} = %return%0 reg"));
		}

		LirInstr::TurtleSetX(x) => {
			code.push(format!("execute as @e[tag=turtle] store result entity @s Pos[0] double 1 run scoreboard players get {x}"));
		}
		LirInstr::TurtleSetY(y) => {
			code.push(format!("execute as @e[tag=turtle] store result entity @s Pos[1] double 1 run scoreboard players get {y}"));
		}
		LirInstr::TurtleSetZ(z) => {
			code.push(format!("execute as @e[tag=turtle] store result entity @s Pos[2] double 1 run scoreboard players get {z}"));
		}
		&LirInstr::TurtleSetBlock(r) => turtle_set_block(r, code),
		&LirInstr::TurtleFillBlock { block, x_span, y_span, z_span } => turtle_fill_block(block, x_span, y_span, z_span, code),
		LirInstr::TurtleGetBlock(r) => turtle_get_block(*r, code),
		&LirInstr::TurtleCopyRegion { x_span, y_span, z_span } => turtle_copy_region(x_span, y_span, z_span, code),
		&LirInstr::TurtlePasteRegionMasked { x_span, y_span, z_span } => turtle_paste_region_masked(x_span, y_span, z_span, code),
		LirInstr::TurtleCopy => {
			code.push("execute at @e[tag=turtle] run clone ~ ~ ~ ~ ~ ~ -1 -1 -1".to_string());
		}
		LirInstr::TurtlePaste => {
			code.push("execute at @e[tag=turtle] run clone -1 -1 -1 -1 -1 -1 ~ ~ ~".to_string());
		}
		LirInstr::PrintInt(i) => {
			let mut s = String::new();
			s.push_str(r#"tellraw @a [{"text":"Printed "},{"score":{"name":""#);
			let i = i.to_string();
			let (i, _) = i.split_once(' ').unwrap();
			s.push_str(i);
			s.push_str(r#"","objective":"reg"}}]"#);
			code.push(s);
		}
		LirInstr::PutChar(i) => {
			code.push(format!("scoreboard players operation %param0%0 reg = {i}"));
			code.push("function intrinsic:put_char".to_string());
		}
	}
}

// call a split function: push return address

// jump to a split block: push return address

fn emit_block(block_id: BlockId, block: &LirBasicBlock, parent: &LirProgram, const_pool: &mut HashSet<i32>) -> Function {
	let mut code: Vec<String> = Vec::new();

	/*if block_id.block == 0 {
		code.push(format!("tellraw @a [{{\"text\":\"{block_id:?}\"}}]"));
	}*/

	for instr in block.body.iter() {
		emit_instr(instr, parent, &mut code, const_pool);
	}

	// FIXME: This doesn't include called functions, the terminator, or count `execute` commands properly.
	let mut num_cmds = code.len();
	for c in code.iter() {
		if c.contains("intrinsic:and") || c.contains("intrinisic:or") || c.contains("intrinsic:xor") {
			num_cmds += 200;
		} else if c.contains("intrinsic:shl_64") {
			num_cmds += 700;
		} else if c.contains("intrinsic:i64_sdiv") || c.contains("intrinsic:i64_srem") || c.contains("intrinsic:i64_udiv") || c.contains("intrinsic:i64_urem") {
			num_cmds += 80_000;
		}
	}
	code.push(format!("scoreboard players add {CMDS_RUN_VAR} {num_cmds}"));
	
	match &block.term {
		&LirTerminator::Jump(target) => {
			if jump_mode() == JumpMode::Direct {
				let target_mc_id = get_mc_id(target.label);
				if !target.cmd_check {
					code.push(format!("function {target_mc_id}"));
				} else {
					// The return address was pushed beforehand in the LIR emitter, so we don't have to handle that here.

					let sleep_reg = Register::sleep_needed();

					code.push(format!("execute store success score {sleep_reg} if score {CMDS_RUN_VAR} >= {MAX_CMDS_VAR}"));
					code.push(format!("execute if score {sleep_reg} matches 1 run scoreboard players set {CMDS_RUN_VAR} 0"));

					code.push(format!("execute if score {sleep_reg} matches 1 run schedule function {target_mc_id} 1"));
					code.push(format!("execute if score {sleep_reg} matches 0 run function {target_mc_id}"));
				}
			} else {
				todo!()
			}
		}
		&LirTerminator::ScheduleJump(target, delay) => {
			if jump_mode() == JumpMode::Direct {
				// TODO: add the "append" keyword
				code.push(format!("schedule function {} {delay}", get_mc_id(target)));
				code.push(format!("scoreboard players set {CMDS_RUN_VAR} 0"));
			} else {
				todo!()
			}
		}
		&LirTerminator::JumpIf { true_label, false_label, cond } => {
			if jump_mode() == JumpMode::Direct {
				let true_func = get_mc_id(true_label.label);
				let false_func = get_mc_id(false_label.label);

				let cond_taken = Register::cond_taken();
				let sleep_reg = Register::sleep_needed();

				if true_label.cmd_check || false_label.cmd_check {
					code.push(format!("execute store success score {sleep_reg} if score {CMDS_RUN_VAR} >= {MAX_CMDS_VAR}"));
					code.push(format!("execute if score {sleep_reg} matches 1 run scoreboard players set {CMDS_RUN_VAR} 0"))
				}

				code.push(format!("scoreboard players set {cond_taken} 0"));
				if !true_label.cmd_check && !false_label.cmd_check {
					code.push(format!("execute unless score {cond} matches 0 run function {}", true_func));
					code.push(format!("execute if score {cond_taken} matches 0 run function {}", false_func));
				} else if true_label.cmd_check && !false_label.cmd_check {
					code.push(format!("execute if score {sleep_reg} matches 1 unless score {cond} matches 0 run schedule function {} 1", true_func));
					code.push(format!("execute if score {sleep_reg} matches 1 unless score {cond} matches 0 run scoreboard players set {cond_taken} 1"));

					code.push(format!("execute if score {cond_taken} matches 0 unless score {cond} matches 0 run function {}", true_func));
					code.push(format!("execute if score {cond_taken} matches 0 run function {}", false_func));
				} else if !true_label.cmd_check && false_label.cmd_check {
					code.push(format!("execute if score {sleep_reg} matches 1 if score {cond} matches 0 run schedule function {} 1", false_func));
					code.push(format!("execute if score {sleep_reg} matches 1 if score {cond} matches 0 run scoreboard players set {cond_taken} 1"));

					code.push(format!("execute if score {cond_taken} matches 0 if score {cond} matches 0 run function {}", false_func));
					code.push(format!("execute if score {cond_taken} matches 0 run function {}", true_func));
				} else if true_label.cmd_check && false_label.cmd_check {
					code.push(format!("execute if score {sleep_reg} matches 1 unless score {cond} matches 0 run schedule function {} 1", true_func));
					code.push(format!("execute if score {sleep_reg} matches 1 if score {cond} matches 0 run schedule function {} 1", false_func));
					code.push(format!("execute if score {sleep_reg} matches 1 run scoreboard players set {cond_taken} 1"));

					code.push(format!("execute if score {cond_taken} matches 0 unless score {cond} matches 0 run function {}", true_func));
					code.push(format!("execute if score {cond_taken} matches 0 if score {cond} matches 0 run function {}", false_func));
				}
			} else {
				todo!()
			}
		}
		LirTerminator::JumpTable { arms, default, cond } => {
			if jump_mode() == JumpMode::Direct {
				let cond_taken = Register::cond_taken();

				code.push(format!("scoreboard players set {cond_taken} 0"));

				for (idx, arm) in arms.iter().enumerate() {
					if let Some(arm) = arm {
						let arm_func = get_mc_id(*arm);
						code.push(format!("execute if score {cond_taken} matches 0 run execute if score {cond} matches {idx} run function {arm_func}"));
					}
				}

				if let Some(default) = *default {
					let default_func = get_mc_id(default);

					code.push(format!("execute if score {cond_taken} matches 0 run function {default_func}"));
				} else {
					code.push(format!("# !INTERPRETER: ASSERT unless score {cond_taken} matches 0"));
					code.push(format!("execute if score {cond_taken} matches 0 run tellraw @a [{{\"text\":\"BAD JUMP TABLE\"}}]"));
				}
			} else {
				todo!()
			}
		}
		crate::lir::LirTerminator::Return => {
			if jump_mode() == JumpMode::Direct {
				// Do nothing
			} else {
				todo!()
			}
		}
		crate::lir::LirTerminator::ReturnToSaved => {
			if jump_mode() == JumpMode::Direct {
				code.push("function wasmrunner:__return_to_saved".to_string());
			} else {
				todo!()
			}
		}
	}
	
	if jump_mode() == JumpMode::Direct {
		code.push(format!("scoreboard players set {} 1", Register::cond_taken()));
	}

	let block_id_str = get_mc_id(block_id);
	parse_function(&block_id_str, &code)
}

fn emit_function(func: &LirFunction, parent: &LirProgram, const_pool: &mut HashSet<i32>) -> Vec<Function> {
	let mut result = Vec::new();
	for (block_id, block) in func.code.iter() {
		result.push(emit_block(*block_id, block, parent, const_pool));
	}
	result
}

pub fn load_intrinsics() -> Vec<Function> {
	let mut result = Vec::new();
	for file in std::fs::read_dir("./src/intrinsic/").unwrap() {
		let file = file.unwrap();

		let file_name = file.file_name();
		let file_name = file_name.to_string_lossy();

		if file_name.ends_with(".mcfunction") {
			let contents = std::fs::read_to_string(file.path()).unwrap();
			let cmds = contents.lines()
				.map(|l| l.trim())
				.filter(|l| !l.is_empty());

			let func_name = format!("intrinsic:{}", file_name.strip_suffix(".mcfunction").unwrap());
			let func_ident: FunctionIdent = parse_command(&func_name).unwrap();

			result.push(parse_function(&func_name, cmds));
		} else {
			for file2 in std::fs::read_dir(file.path()).unwrap() {
				let file2 = file2.unwrap();

				let file_name2 = file2.file_name();
				let file_name2 = file_name2.to_string_lossy();

				if file_name2.ends_with(".mcfunction") {
					let contents = std::fs::read_to_string(file2.path()).unwrap();
					let cmds = contents.lines()
						.map(|l| l.trim())
						.filter(|l| !l.is_empty() && !l.starts_with('#'))
						.map(|l| l.parse().unwrap()).collect::<Vec<_>>();

					let func_name = format!("intrinsic:{file_name}/{}", file_name2.strip_suffix(".mcfunction").unwrap());
					let func_ident: FunctionIdent = parse_command(&func_name).unwrap();

					result.push(Function { id: func_ident, cmds });

				} else {
					todo!()
				}
			}
			// TODO: Nested
		}
	}

	result
}

pub fn make_export_func(name: &str, id: BlockId) -> Function {
	let wrapper_name = format!("wasmrunner:{name}");
	let wrapper_id = wrapper_name.parse().unwrap();

	let func_id = get_mc_id(id);
	let cmds: Vec<Command> = vec![
		"data modify storage wasm:scratch stack.data set value {}".to_string().parse().unwrap(),
		"data modify storage wasm:scratch stack.tail set from storage wasm:returnstack stack".parse().unwrap(),
		"data modify storage wasm:returnstack stack set from storage wasm:scratch stack".parse().unwrap(),
		format!("function {func_id}").parse().unwrap(),
	];

	Function { id: wrapper_id, cmds }
}

pub fn add_export_funcs(exports: &HashMap<String, BlockId>, code: &mut Vec<Function>) {
	code.extend(exports.iter().map(|(name, id)| make_export_func(name, *id)));
}

static INSERT_FUNC_PRINTS: bool = false;

pub fn emit_program(lir_program: &LirProgram) -> Vec<Function> {
	let mut result = Vec::new();
	let mut constants = lir_program.constants.clone();
	for func in lir_program.code.iter() {
		result.extend(emit_function(func, lir_program, &mut constants));
	}

	if INSERT_FUNC_PRINTS {
		for r in result.iter_mut() {
			let id = r.id.to_string();
			let s = format!(r#"tellraw @a [{{"text":"Entered {id}"}}]"#);
			r.cmds.insert(0, s.parse().unwrap());
		}
	}

	let init_func = create_init_func(lir_program, &constants);
	result.push(init_func);

	let return_to_saved = create_return_to_saved_func(lir_program);
	result.extend(return_to_saved);

	add_export_funcs(&lir_program.exports, &mut result);

	result.extend(load_intrinsics());

	result
}

pub struct Datapack {
    pub description: String,
}

impl Datapack {
    /*
    pub fn run_index(&self) -> Option<usize> {
        self.functions
            .iter()
            .enumerate()
            .find(|(_, f)| f.id == FunctionId::new("run"))
            .map(|(i, _)| i)
    }
    */

    pub fn new() -> Self {
        Self {
            description: "Autogenerated by wasmcraft".to_string(),
        }
    }

    /// Creates a datapack with the given root directory, erasing the previous contents of the folder.
    pub fn save(&self, output_folder: &Path) -> Result<(), std::io::Error> {
        if output_folder.exists() {
            eprintln!("Removing previous contents of output directory");
            std::fs::remove_dir_all(output_folder)?;
        }

        std::fs::create_dir(&output_folder)?;

        let mcmeta_contents = r#"
            { "pack": {
                "pack_format": 5,
                "description": "foo"
            } }
        "#;

        std::fs::write(
            output_folder.join("pack.mcmeta"),
            mcmeta_contents.to_string(),
        )?;

        /*
        std::fs::create_dir_all(output_folder.join(Path::new("data/setup/functions/")))?;
        std::fs::write(
            output_folder.join(Path::new("data/setup/functions/setup.mcfunction")),
            SETUP_STR,
        )?;

        std::fs::create_dir_all(output_folder.join(Path::new("data/stdout/functions/")))?;
        std::fs::write(
            output_folder.join(Path::new("data/stdout/functions/putc.mcfunction")),
            PUTC_STR,
        )?;
        std::fs::write(
            output_folder.join(Path::new("data/stdout/functions/flush.mcfunction")),
            FLUSH_STR,
        )?;
        */

        /*
        for func in self.functions.iter() {
            let contents = func
                .cmds
                .iter()
                .map(|cmd| cmd.to_string())
                .collect::<Vec<_>>();

            let contents = contents.join("\n");

            let path = func.id.path();
            let path_folders = &path[..path.len() - 1];
            let file_name = &path[path.len() - 1];

            let mut full_path = output_folder
                .join(Path::new("data"))
                .join(Path::new(func.id.namespace()))
                .join(Path::new("functions"));

            for folder in path_folders {
                full_path = full_path.join(Path::new(folder));
            }

            std::fs::create_dir_all(&full_path)?;

            full_path = full_path.join(format!("{}.mcfunction", file_name));

            std::fs::write(full_path, contents.as_bytes())?
        }
        */

        Ok(())
    }

    pub fn write_function(&self, output_folder: &Path, namespace: &str, mut name: &str, contents: &str) -> std::io::Result<()> {
        let prefix = if let Some((a, b)) = name.split_once('/') {
            name = b;
            a
        } else {
            ""
        };

        let func_folder = output_folder.join(format!("data/{}/functions/{}", namespace, prefix));
        std::fs::create_dir_all(&func_folder)?;

        let func_name = format!("{}.mcfunction", name);

        std::fs::write(func_folder.join(Path::new(&func_name)), contents)?;

        Ok(())
    }
}

pub fn persist_program(folder_path: &Path, funcs: &[Function]) {
	let datapack = Datapack::new();

	datapack.save(folder_path).unwrap();
	for func in funcs.iter() {
		if func.id.namespace == "intrinsic" {
			continue;
		}
		let contents = func.cmds.iter().map(ToString::to_string).collect::<Vec<_>>();
		let contents = contents.join("\n");
		datapack.write_function(folder_path, "wasmrunner", &func.id.path, &contents).unwrap();
	}

	for i in std::fs::read_dir("./src/intrinsic").unwrap() {
		let i = i.unwrap();
		if i.file_type().unwrap().is_dir() {
			for j in std::fs::read_dir(i.path()).unwrap() {
				let j = j.unwrap();
				if j.file_type().unwrap().is_dir() {
					todo!()
				} else {
					let p = i.file_name();
					let n = j.file_name();
					let n = format!("{}/{}", p.to_string_lossy(), n.to_string_lossy());
					let n = &n[..n.len() - ".mcfunction".len()];

					let contents = std::fs::read_to_string(j.path()).unwrap();
					datapack.write_function(folder_path, "intrinsic", n, &contents).unwrap();
				}
			}
		} else {
			let name = i.file_name();
			let name = name.to_string_lossy();
			let name = &name[..name.len() - ".mcfunction".len()];
			let contents = std::fs::read_to_string(i.path()).unwrap();
			datapack.write_function(folder_path, "intrinsic", name, &contents).unwrap();
		}
	}
}

#[cfg(test)]
mod test {
	use datapack_common::functions::command_components::{Objective, ScoreHolder};
	use datapack_vm::Interpreter;

	use super::*;

	fn test_constant_func<I, E, R, F>(lhs_vals: I, rhs: i32, emitter: E, ex: F) 
		where
			I: Iterator<Item=i32>,
			E: FnOnce(Register, R, i32, &mut Vec<String>, &mut HashSet<i32>),
			F: Fn(i32, i32) -> i32,
			R: From<Register>,
	{
		let dst = Register::return_lo(0);
		let lhs = Register::param_lo(0);

		let mut code = Vec::new();
		let mut const_pool = HashSet::new();
		emitter(dst, lhs.into(), rhs, &mut code, &mut const_pool);

		let func = parse_function("wasmrunner:test_constant", &code);
		let func_id = func.id.clone();

		let xor_0_code = std::fs::read_to_string("src/intrinsic/xor.mcfunction").unwrap();
		let xor_0 = parse_function("intrinsic:xor", xor_0_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let xor_1_code = std::fs::read_to_string("src/intrinsic/xor_normal.mcfunction").unwrap();
		let xor_1 = parse_function("intrinsic:xor_normal", xor_1_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let xor_2_code = std::fs::read_to_string("src/intrinsic/xor_inner.mcfunction").unwrap();
		let xor_2 = parse_function("intrinsic:xor_inner", xor_2_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let program = vec![func, xor_0, xor_1, xor_2];

		let mut interp = Interpreter::new(program, 0);

		//interp.scoreboard.0.insert(Objective::new("reg".to_string()).unwrap(), Default::default());

		for c in const_pool {
			let (holder, obj) = Register::const_val(c).scoreboard_pair();
			interp.set_named_score(&holder, &obj, c);
		}

		let (lhs_holder, lhs_obj) = lhs.scoreboard_pair();
		let (dst_holder, dst_lo) = dst.scoreboard_pair();
		for lhs_val in lhs_vals {
			interp.set_named_score(&lhs_holder, &lhs_obj, lhs_val);

			println!("{:?} {:?}", lhs_val, rhs);

			let interp_idx = interp.get_func_idx(&func_id);
			interp.set_pos(interp_idx);
			interp.run_to_end().unwrap();

			let dst_val = interp.get_named_score(&dst_holder, &dst_lo).unwrap();

			let exp = ex(lhs_val, rhs);

			if dst_val != exp {
				eprintln!("Code:");
				for c in code.iter() {
					eprintln!("    {}", c);
				}
				eprintln!("Actual: {}", dst_val);
				eprintln!("Expected: {}", exp);
				eprintln!("LHS: {}", lhs_val);
				eprintln!("RHS: {}", rhs);
				panic!("actual and expected values differed");
			}
		}
	}

	fn test_constant_and<I>(lhs_vals: I, rhs: i32)
		where I: Iterator<Item=i32>,
	{
		test_constant_func(lhs_vals, rhs, emit_constant_and, |a, b| a & b);
	}

	fn test_constant_shru<I>(lhs_vals: I, rhs: i32)
		where I: Iterator<Item=i32>,
	{
		test_constant_func(lhs_vals, rhs, emit_constant_shru, |a, b| ((a as u32) >> b) as i32);
	}

	fn test_constant_xor<I>(lhs_vals: I, rhs: i32)
		where I: Iterator<Item=i32>,
	{
		test_constant_func(lhs_vals, rhs, emit_constant_xor, |a, b| a ^ b);
	}

	#[test]
	#[ignore]
	fn constant_and() {
		// -1 - INT_MIN * (-1)

		let test_vals = [
			0, -1, 1, 2, 4, i32::MAX, i32::MIN, 17, 39, -58, -107, 0x5555_5555, 1234567, -983253743, 1_000_000_000
		];

		for rhs in test_vals {
			test_constant_and(test_vals.into_iter(), rhs);
		}
	}

	#[test]
	fn constant_shru() {
		let test_vals = [
			0, -1, 1, 2, 4, i32::MAX, i32::MIN, 17, 39, -58, -107, 0x5555_5555, 1234567, -983253743, 1_000_000_000
		];

		for rhs in 0..32 {
			test_constant_shru(test_vals.into_iter(), rhs);
		}
	}

	// TODO:

	#[test]
	fn constant_xor() {
		let test_vals = [
			0, -1, 1, 2, 4, 7, i32::MAX, i32::MIN, 1 << 10, 1 << 11, 1 << 15, 3 << 10, 17, 39, -58, -107, 0x5555_5555, 1234567, -983253743, 1_000_000_000
		];

		for rhs in test_vals {
			test_constant_xor(test_vals.into_iter(), rhs);
		}
	}

	#[test]
	fn test_constant_load_64() {
		let values = [
			0x00_00_00_00, 0x85_86_87_88_u32 as i32, 0x84_83_82_81_u32 as i32, 0x87_65_43_21_u32 as i32, 0x12_34_56_78, 0x80_00_00_00_u32 as i32, 0x00_00_00_01
		];

		for i in 0..4 * values.len() - 7 {
			constant_load_64(&values, i as i32);
		}
	}

	fn run_interp_test(code: Vec<String>) {
		let func = parse_function("wasmrunner:test", code);
		let func_id = func.id.clone();

		let program = vec![func];

		let mut interp = Interpreter::new(program, 0);

		let interp_idx = interp.get_func_idx(&func_id);
		interp.set_pos(interp_idx);
		interp.run_to_end().unwrap();
	}

	fn constant_load_64(memory: &[i32], offset: i32) {
		let memory_bytes = memory.iter().flat_map(|m| m.to_le_bytes()).collect::<Vec<_>>();

		let dst = DoubleRegister::return_reg(0);

		let mut code = Vec::new();

		let mut const_pool = HashSet::new();

		let addr = Register::work_lo(0, 0);

		mem_load_64(dst, addr.into(), &mut code, &mut const_pool);

		let func = parse_function("wasmrunner:test_constant_memory_load", &code);
		let func_id = func.id.clone();

		let ldw_0_code = std::fs::read_to_string("src/intrinsic/load_doubleword.mcfunction").unwrap();
		let ldw_0 = parse_function("intrinsic:load_doubleword", ldw_0_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_1_code = std::fs::read_to_string("src/intrinsic/doubleword/load_aligned.mcfunction").unwrap();
		let ldw_1 = parse_function("intrinsic:doubleword/load_aligned", ldw_1_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_2_code = std::fs::read_to_string("src/intrinsic/doubleword/load_unaligned.mcfunction").unwrap();
		let ldw_2 = parse_function("intrinsic:doubleword/load_unaligned", ldw_2_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_3_code = std::fs::read_to_string("src/intrinsic/load_word.mcfunction").unwrap();
		let ldw_3 = parse_function("intrinsic:load_word", ldw_3_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_4_code = std::fs::read_to_string("src/intrinsic/load_word_unaligned.mcfunction").unwrap();
		let ldw_4 = parse_function("intrinsic:load_word_unaligned", ldw_4_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_5_code = std::fs::read_to_string("src/intrinsic/setptr.mcfunction").unwrap();
		let ldw_5 = parse_function("intrinsic:setptr", ldw_5_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_6_code = std::fs::read_to_string("src/intrinsic/load_byte.mcfunction").unwrap();
		let ldw_6 = parse_function("intrinsic:load_byte", ldw_6_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let program = vec![func, ldw_0, ldw_1, ldw_2, ldw_3, ldw_4, ldw_5, ldw_6];

		let mut interp = Interpreter::new(program, 0);

		//interp.scoreboard.0.insert(Objective::new("reg".to_string()).unwrap(), Default::default());

		for i in 0..32 {
			let c: i32 = 1 << i;
			let holder = ScoreHolder::new(format!("%%{c}")).unwrap();
			let obj = Objective::new("reg".to_string()).unwrap();
			interp.set_named_score(&holder, &obj, c);
		}

		let holder_z = ScoreHolder::new("%%PAGE_SPAN_Z".to_string()).unwrap();
		let holder_y = ScoreHolder::new("%%PAGE_SPAN_Y".to_string()).unwrap();
		let obj = Objective::new("reg".to_string()).unwrap();
		interp.set_named_score(&holder_z, &obj, PAGE_SPAN_Z);
		interp.set_named_score(&holder_y, &obj, PAGE_SPAN_Y);

		for c in const_pool {
			let (holder, obj) = Register::const_val(c).scoreboard_pair();
			interp.set_named_score(&holder, &obj, c);
		}

		for (addr, value) in memory.iter().copied().enumerate() {
			let (x, y, z) = get_address_pos(addr as i32 * 4);
			let cmd = format!("setblock {x} {y} {z} minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:{value}}}}}}}").parse::<Command>().unwrap();
			interp.execute_cmd(&cmd).unwrap();
		}

		let (addr_holder, addr_obj) = addr.scoreboard_pair();	
		interp.set_named_score(&addr_holder, &addr_obj, offset);

		let (dst_lo_holder, dst_lo_obj) = dst.lo().scoreboard_pair();
		let (dst_hi_holder, dst_hi_obj) = dst.hi().scoreboard_pair();
		let interp_idx = interp.get_func_idx(&func_id);
		interp.set_pos(interp_idx);
		interp.run_to_end().unwrap();

		let actual_lo = interp.get_named_score(&dst_lo_holder, &dst_lo_obj).unwrap();
		let actual_hi = interp.get_named_score(&dst_hi_holder, &dst_hi_obj).unwrap();

		let actual = ((actual_hi as i64) << 32) | (actual_lo as u32 as i64);

		let expected = u64::from_le_bytes(memory_bytes[offset as usize..][..8].try_into().unwrap()) as i64;

		if expected != actual {
			eprintln!("Code:");
			for c in code.iter() {
				eprintln!("    {}", c);
			}
			eprintln!("Actual: {:#X}", actual);
			eprintln!("Expected: {:#X}", expected);
			eprintln!("Offset: {:#X}", offset);
			//eprintln!("{:?}", interp.scoreboard);
			panic!("actual and expected values differed");
		}
	}


	#[test]
	fn test_constant_load_16() {
		let values = [
			0x00_00_00_00, 0x85_86_87_88_u32 as i32, 0x84_83_82_81_u32 as i32, 0x87_65_43_21_u32 as i32, 0x12_34_56_78, 0x80_00_00_00_u32 as i32, 0x00_00_00_01
		];

		for i in 0..4 * values.len() - 1 {
			constant_load_16(&values, i as i32);
		}
	}

	fn constant_load_16(memory: &[i32], offset: i32) {
		let memory_bytes = memory.iter().flat_map(|m| m.to_le_bytes()).collect::<Vec<_>>();

		let dst = Register::return_lo(0);

		let mut code = Vec::new();

		let addr = Register::const_val(offset);

		mem_load_16(dst, addr.into(), &mut code);

		let func = parse_function("wasmrunner:test_constant_memory_load", &code);
		let func_id = func.id.clone();
		let program = vec![func];

		let mut interp = Interpreter::new(program, 0);

		//interp.scoreboard.0.insert(Objective::new("reg".to_string()).unwrap(), Default::default());

		let (off_holder, off_obj) = addr.scoreboard_pair();
		interp.set_named_score(&off_holder, &off_obj, offset);

		for i in 0..32 {
			let c: i32 = 1 << i;
			let holder = ScoreHolder::new(format!("%%{c}")).unwrap();
			let obj = Objective::new("reg".to_string()).unwrap();
			interp.set_named_score(&holder, &obj, c);
		}

		for (addr, value) in memory.iter().copied().enumerate() {
			let (x, y, z) = get_address_pos(addr as i32 * 4);
			let cmd = format!("setblock {x} {y} {z} minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:{value}}}}}}}").parse::<Command>().unwrap();
			interp.execute_cmd(&cmd).unwrap();
		}

		let (dst_holder, dst_obj) = dst.scoreboard_pair();
		let interp_idx = interp.get_func_idx(&func_id);
		interp.set_pos(interp_idx);
		interp.run_to_end().unwrap();

		let actual = interp.get_named_score(&dst_holder, &dst_obj).unwrap();

		let expected = u16::from_le_bytes(memory_bytes[offset as usize..][..2].try_into().unwrap()) as i32;

		if expected != actual {
			eprintln!("Code:");
			for c in code.iter() {
				eprintln!("    {}", c);
			}
			eprintln!("Actual: {:#X}", actual);
			eprintln!("Expected: {:#X}", expected);
			panic!("actual and expected values differed");
		}
	}

	#[test]
	fn test_constant_store_32() {
		let values = [0x00_00_00_00, 0xFF_FF_FF_FF_u32 as i32, 0xFF_FF_FF_FF_u32 as i32, 0x87_65_43_21_u32 as i32, 0x12_34_56_78, 0x80_00_00_00_u32 as i32, 0x00_00_00_01];

		for i in 1..4 {
			constant_store_32(&values, 0x01_02_03_04_05_06_07_08, i);
		}

		for i in 1..4 {
			constant_store_32(&values, 0xF1_F2_F3_F4_F5_F6_F7_F8_u64 as i64, i);
		}
	}

	fn constant_store_32(values: &[i32], before: i64, offset: i32) {
		let dst = Register::return_lo(0);

		let src = Register::work_lo(0, 0);

		let mut code = Vec::new();
		let mut const_pool = HashSet::new();

		mem_store_unaligned_32(src, offset, offset, &mut code, &mut const_pool);

		let func = parse_function("wasmrunner:test_constant_memory_store", &code);
		let func_id = func.id.clone();
		let program = vec![func];

		let mut interp = Interpreter::new(program, 0);

		//interp.scoreboard.0.insert(Objective::new("reg".to_string()).unwrap(), Default::default());

		for c in const_pool {
			let (holder, obj) = Register::const_val(c).scoreboard_pair();
			interp.set_named_score(&holder, &obj, c);
		}

		let (x0, y0, z0) = get_address_pos(0);
		let (x1, y1, z1) = get_address_pos(4);

		let v0 = before as i32;
		let v1 = (before >> 32) as i32;

		let c0 = format!("setblock {x0} {y0} {z0} minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:{v0}}}}}}}").parse::<Command>().unwrap();
		let c1 = format!("setblock {x1} {y1} {z1} minecraft:jukebox{{RecordItem:{{id:\"minecraft:stone\",Count:1b,tag:{{Memory:{v1}}}}}}}").parse::<Command>().unwrap();

		interp.execute_cmd(&c0).unwrap();
		interp.execute_cmd(&c1).unwrap();

		let d0 = format!("execute store result score {dst} run data get block {x0} {y0} {z0} RecordItem.tag.Memory 1").parse::<Command>().unwrap();
		let d1 = format!("execute store result score {dst} run data get block {x1} {y1} {z1} RecordItem.tag.Memory 1").parse::<Command>().unwrap();

		let (src_holder, src_obj) = src.scoreboard_pair();
		let (dst_holder, dst_obj) = dst.scoreboard_pair();
		for src_val in values {
			interp.set_named_score(&src_holder, &src_obj, *src_val);

			let interp_idx = interp.get_func_idx(&func_id);
			interp.set_pos(interp_idx);
			interp.run_to_end().unwrap();

			interp.execute_cmd(&d0).unwrap();
			let m0 = interp.get_named_score(&dst_holder, &dst_obj).unwrap();
			interp.execute_cmd(&d1).unwrap();
			let m1 = interp.get_named_score(&dst_holder, &dst_obj).unwrap();
			
			let actual = ((m1 as i64) << 32) | (m0 as u32 as i64);

			let mut expected = before;
			expected &= !(0xFF_FF_FF_FF_i64 << (offset * 8));
			expected |= (*src_val as u32 as i64) << (offset * 8);

			if expected != actual {
				eprintln!("Code:");
				for c in code.iter() {
					eprintln!("    {}", c);
				}
				eprintln!("Actual: {:#X}", actual);
				eprintln!("Expected: {:#X}", expected);
				panic!("actual and expected values differed");
			}
		}
	}

	#[test]
	fn test_memset() {
		const PAGE_SIZE: i32 = 8 * 256 * 8;

		for start_addr in [0, 4, 5, 6, 7] {
			for length in [0, 1, 2, 3, 4, 5, 6, 7, 62] {
				memset_tester(start_addr, 0x88, length);
			}
		}

		memset_tester(0, 0x88, PAGE_SIZE);

		memset_tester(3, 0x88, PAGE_SIZE * 2 + 47);
	}

	fn memset_tester(addr: i32, value: u8, num_bytes: i32) {
		let code = vec![
			format!("scoreboard players set %param0%0 reg {addr}"),
			format!("scoreboard players set %param1%0 reg {value}"),
			format!("scoreboard players set %param2%0 reg {num_bytes}"),
			"function intrinsic:memset".to_string(),
		];

		let func = parse_function("wasmrunner:test_memset", &code);
		let func_id = func.id.clone();

		let ldw_0_code = std::fs::read_to_string("src/intrinsic/memset.mcfunction").unwrap();
		let ldw_0 = parse_function("intrinsic:memset", ldw_0_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_1_code = std::fs::read_to_string("src/intrinsic/memset/body_words.mcfunction").unwrap();
		let ldw_1 = parse_function("intrinsic:memset/body_words", ldw_1_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_2_code = std::fs::read_to_string("src/intrinsic/memset/tail_1_byte.mcfunction").unwrap();
		let ldw_2 = parse_function("intrinsic:memset/tail_1_byte", ldw_2_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_3_code = std::fs::read_to_string("src/intrinsic/memset/tail_2_byte.mcfunction").unwrap();
		let ldw_3 = parse_function("intrinsic:memset/tail_2_byte", ldw_3_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_4_code = std::fs::read_to_string("src/intrinsic/memset/tail_3_byte.mcfunction").unwrap();
		let ldw_4 = parse_function("intrinsic:memset/tail_3_byte", ldw_4_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_5_code = std::fs::read_to_string("src/intrinsic/setptr.mcfunction").unwrap();
		let ldw_5 = parse_function("intrinsic:setptr", ldw_5_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_6_code = std::fs::read_to_string("src/intrinsic/memset/head_1_byte.mcfunction").unwrap();
		let ldw_6 = parse_function("intrinsic:memset/head_1_byte", ldw_6_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_7_code = std::fs::read_to_string("src/intrinsic/memset/head_2_byte.mcfunction").unwrap();
		let ldw_7 = parse_function("intrinsic:memset/head_2_byte", ldw_7_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_8_code = std::fs::read_to_string("src/intrinsic/memset/head_3_byte.mcfunction").unwrap();
		let ldw_8 = parse_function("intrinsic:memset/head_3_byte", ldw_8_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_9_code = std::fs::read_to_string("src/intrinsic/memset/mid_hi_byte.mcfunction").unwrap();
		let ldw_9 = parse_function("intrinsic:memset/mid_hi_byte", ldw_9_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_10_code = std::fs::read_to_string("src/intrinsic/memset/mid_lo_byte.mcfunction").unwrap();
		let ldw_10 = parse_function("intrinsic:memset/mid_lo_byte", ldw_10_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_11_code = std::fs::read_to_string("src/intrinsic/memset/mid_2_byte.mcfunction").unwrap();
		let ldw_11 = parse_function("intrinsic:memset/mid_2_byte", ldw_11_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let ldw_12_code = std::fs::read_to_string("src/intrinsic/memset/normal.mcfunction").unwrap();
		let ldw_12 = parse_function("intrinsic:memset/normal", ldw_12_code.lines().map(str::trim).filter(|l| !l.is_empty()));

		let program = vec![func, ldw_0, ldw_1, ldw_2, ldw_3, ldw_4, ldw_5, ldw_6, ldw_7, ldw_8, ldw_9, ldw_10, ldw_11, ldw_12];

		let mut interp = Interpreter::new(program, 0);

		for i in 0..32 {
			let c: i32 = 1 << i;
			let holder = ScoreHolder::new(format!("%%{c}")).unwrap();
			let obj = Objective::new("reg".to_string()).unwrap();
			interp.set_named_score(&holder, &obj, c);
		}

		let holder_z = ScoreHolder::new("%%PAGE_SPAN_Z".to_string()).unwrap();
		let holder_y = ScoreHolder::new("%%PAGE_SPAN_Y".to_string()).unwrap();
		let obj = Objective::new("reg".to_string()).unwrap();
		interp.set_named_score(&holder_z, &obj, PAGE_SPAN_Z);
		interp.set_named_score(&holder_y, &obj, PAGE_SPAN_Y);

		//interp.scoreboard.0.insert(Objective::new("reg".to_string()).unwrap(), Default::default());

		for i in 0..MEMORY_PAGE_BLOCKS * 3 {
			let pos = get_address_pos(i as i32 * 4);
			let v0 = 0x55_55_55_55;
			interp.set_block_raw(pos, datapack_vm::interpreter::Block::Jukebox(v0));
		}

		let interp_idx = interp.get_func_idx(&func_id);
		interp.set_pos(interp_idx);
		interp.run_to_end().unwrap();

		let mut bytes = Vec::new();

		for i in 0..MEMORY_PAGE_BLOCKS * 3 {
			let pos = get_address_pos(i as i32 * 4);
			let block = interp.get_block(pos);

			if let Some(datapack_vm::interpreter::Block::Jukebox(m0)) = block {
				bytes.extend(m0.to_le_bytes());
			} else {
				panic!();
			}
		}

		let modified_range = addr..(addr + num_bytes);

		for (i, byte) in bytes.iter().copied().enumerate() {
			if modified_range.contains(&(i as i32)) {
				if byte != value {
					eprintln!("Byte wasn't written to but should have been!");
					eprintln!("Start: {:#X}", addr);
					eprintln!("Value: {:#X}", value);
					eprintln!("Length: {:#X}", num_bytes);
					eprintln!();
					eprintln!("Address: {:#X}", i);
					eprintln!("Actual value: {:#X}", byte);
					eprintln!("Actual word: {:X?}", &bytes[(i / 4) * 4..][..4]);
					panic!()
				}
			} else if byte != 0x55 {
				eprintln!("Byte should not have been written to!");
				eprintln!("Start: {:#X}", addr);
				eprintln!("Value: {:#X}", value);
				eprintln!("Length: {:#X}", num_bytes);

				eprintln!("Actual value: {:#X}", byte);
				panic!();
			}
		}

		/*if expected != actual {
			eprintln!("Code:");
			for c in code.iter() {
				eprintln!("    {}", c);
			}
			eprintln!("Actual: {:#X}", actual);
			eprintln!("Expected: {:#X}", expected);
			panic!("actual and expected values differed");
		}*/
	}
}