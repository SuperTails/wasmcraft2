use std::collections::HashSet;

use command_parser::{CommandParse, parse_command};
use datapack_common::functions::{Function, Command, command_components::FunctionIdent};
use wasmparser::Type;

use crate::{lir::{LirProgram, LirFunction, LirBasicBlock, LirInstr, Register, LirTerminator, Condition, Half, DoubleRegister}, ssa::BlockId, jump_mode, JumpMode};

fn create_scoreboard_init(code: &mut Vec<String>) {
	code.push("# Set up scoreboard".to_string());
	code.push("scoreboard objectives remove reg".to_string());
	code.push("scoreboard objectives add reg dummy".to_string());

	code.push("data modify storage wasm:datastack stack set value {}".to_string());
	code.push("data modify storage wasm:localstack stack set value {}".to_string());
	code.push("data modify storage wasm:scratch stack set value {}".to_string());
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
	// TODO:

	let old_style = [-1];

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
}

// init code:
// create objectives
// set up globals
// set up memory
// reset local stack
// reset data stack
// initialize constants
fn create_init_func(program: &LirProgram) -> Function {
	let mut code = Vec::new();

	create_scoreboard_init(&mut code);

	create_pointers_init(&mut code);

	create_constants_init(&program.constants, &mut code);

	let cmds = code.into_iter().map(|c| c.parse().unwrap()).collect::<Vec<Command>>();

	let (rest, ident) = FunctionIdent::parse_from_command("wasmrunner:init").unwrap();
	assert!(rest.is_empty());

	Function { id: ident, cmds }
}


fn push_data(reg: Register, code: &mut Vec<String>) {
	code.push(format!("execute store result storage wasm:scratch stack.data int 1 run scoreboard players get {reg}"));
	code.push("data modify storage wasm:scratch stack.tail set from storage wasm:datastack stack".to_string());
	code.push("data modify storage wasm:datastack stack set from storage wasm:scratch stack".to_string());
}

fn pop_data(reg: Register, code: &mut Vec<String>) {
	code.push(format!("execute store result score {reg} run data get storage wasm:datastack stack.data 1"));
	code.push("data modify storage wasm:datastack stack set from storage wasm:datastack stack.tail".to_string());
}

fn push_local_frame(ty: &[Type], code: &mut Vec<String>) {
	let mut arr = "[".to_string();
	for i in 0..ty.len() {
		arr.push_str("0, 0");
		if i != ty.len() - 1 {
			arr.push_str(", ")
		}
	}
	arr.push(']');

	code.push(format!("data modify storage wasm:scratch stack.data set value {arr}"));
	code.push("data modify storage wasm:scratch stack.tail set from storage wasm:localstack stack".to_string());
	code.push("data modify storage wasm:localstack stack set from storage wasm:scratch stack".to_string());
}

fn pop_local_frame(_ty: &[Type], code: &mut Vec<String>) {
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

fn mem_store_32(src: Register, addr: Register, code: &mut Vec<String>) {
	code.push(format!("scoreboard players operation %ptr reg = {addr}"));
	code.push(format!("scoreboard players operation %param0%0 reg = {src}"));
	code.push("function intrinsic:setptr".to_string());
	code.push("function intrinsic:store_word".to_string());
}

fn mem_store_16(src: Register, addr: Register, code: &mut Vec<String>) {
	code.push(format!("scoreboard players operation %ptr reg = {addr}"));
	code.push(format!("scoreboard players operation %param2%0 reg = {src}"));
	code.push("function intrinsic:store_halfword".to_string());
}

fn mem_store_8 (src: Register, addr: Register, code: &mut Vec<String>) {
	code.push(format!("scoreboard players operation %ptr reg = {addr}"));
	code.push(format!("scoreboard players operation %param2%0 reg = {src}"));
	code.push("function intrinsic:setptr".to_string());
	code.push("function intrinsic:store_byte".to_string());
}

fn mem_load_32(dst: Register, addr: Register, code: &mut Vec<String>) {
	code.push(format!("scoreboard players operation %ptr reg = {addr}"));
	code.push("function intrinsic:setptr".to_string());
	code.push("function intrinsic:load_word".to_string());
	code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
}

fn mem_load_16(dst: Register, addr: Register, code: &mut Vec<String>) {
	code.push(format!("scoreboard players operation %ptr reg = {addr}"));
	code.push("function intrinsic:setptr".to_string());
	// TODO: Aligned optimizations
	code.push("function intrinsic:load_halfword_unaligned".to_string());
	code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
}

fn mem_load_8 (dst: Register, addr: Register, code: &mut Vec<String>) {
	code.push(format!("scoreboard players operation %ptr reg = {addr}"));
	code.push("function intrinsic:setptr".to_string());
	code.push("function intrinsic:load_byte".to_string());
	code.push(format!("scoreboard players operation {dst} = %param0%0 reg"));
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

	assert_ne!(dst, lhs);
	assert_ne!(dst, rhs);

	code.push(format!("scoreboard players set {dst} 0"));
	code.push(format!("execute if score {lhs} matches ..-1 if score {rhs} matches 0.. run scoreboard players set {dst} 0"));
	code.push(format!("execute if score {lhs} matches 0.. if score {rhs} matches ..-1 run scoreboard players set {dst} 1"));
	code.push(format!("execute if score {lhs} matches ..-1 if score {rhs} matches ..-1 if score {lhs} < {rhs} run scoreboard players set {dst} 1"));
	code.push(format!("execute if score {lhs} matches 0.. if score {rhs} matches 0.. if score {lhs} < {rhs} run scoreboard players set {dst} 1"));
}

fn unsigned_greater_than_eq(dst: Register, lhs: Register, rhs: Register, code: &mut Vec<String>) {
	unsigned_less_than(dst, rhs, lhs, code); /* swapped */
	// TODO: This assumes lhs and rhs are not mutated
	code.push(format!("execute if score {dst} matches 0 run execute store success score {dst} if score {lhs} = {rhs}"));
}

fn signed_div(dst: Register, lhs: Register, rhs: Register, code: &mut Vec<String>) {
	assert_ne!(lhs, dst);
	assert_ne!(rhs, dst);

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

fn unsigned_div(dst: Register, lhs: Register, rhs: Register, code: &mut Vec<String>) {
	// TODO: Find a better way to pick these registers
	let d1 = Register::temp_lo(30);
	let r1 = Register::temp_lo(31);
	let d2 = Register::temp_lo(32);
	let r2 = Register::temp_lo(33);
	let d3 = Register::temp_lo(34);
	let is_gtu = Register::temp_lo(35);
	let lhs_lo = Register::temp_lo(36);

	assert_ne!(lhs, dst);
	assert_ne!(rhs, dst);

	// let mut dst = 0;
	code.push(format!("scoreboard players set {dst} 0"));

	// if lhs >= 0 && rhs >= 0 { dst = lhs / rhs }
	code.push(format!("execute if score {lhs} matches 0.. if score {rhs} matches 0.. run scoreboard players operation {dst} = {lhs}"));
	code.push(format!("execute if score {lhs} matches 0.. if score {rhs} matches 0.. run scoreboard players operation {dst} /= {rhs}"));

	// is_gtu = (lhs as u32) >= (rhs as u32)
	unsigned_greater_than_eq(is_gtu, lhs, rhs, code);

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

fn unsigned_rem(dst: Register, lhs: Register, rhs: Register, code: &mut Vec<String>) {
	assert_ne!(lhs, dst);
	assert_ne!(rhs, dst);

	unsigned_div(dst, lhs, rhs, code);

	// lhs - dst * rhs

	code.push(format!("scoreboard players operation {dst} *= {rhs}"));
	code.push(format!("scoreboard players operation {dst} *= %%-1 reg"));
	code.push(format!("scoreboard players operation {dst} += {lhs}"));
}

fn signed_rem(dst: Register, lhs: Register, rhs: Register, code: &mut Vec<String>) {
	assert_ne!(lhs, dst);
	assert_ne!(rhs, dst);

	signed_div(dst, lhs, rhs, code);

	// lhs - dst * rhs

	code.push(format!("scoreboard players operation {dst} *= {rhs}"));
	code.push(format!("scoreboard players operation {dst} *= %%-1 reg"));
	code.push(format!("scoreboard players operation {dst} += {lhs}"));
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



fn emit_instr(instr: &LirInstr, parent: &LirProgram, code: &mut Vec<String>) {
	match instr {
		&LirInstr::Assign(dst, src) => code.push(format!("scoreboard players operation {dst} = {src}")),
		&LirInstr::Set(dst, src) => code.push(format!("scoreboard players set {dst} {src}")),
		&LirInstr::Add(dst, src) => code.push(format!("scoreboard players operation {dst} += {src}")),
		&LirInstr::Sub(dst, src) => code.push(format!("scoreboard players operation {dst} -= {src}")),
		&LirInstr::Mul(dst, src) => code.push(format!("scoreboard players operation {dst} *= {src}")),
		&LirInstr::DivS(dst, lhs, rhs) => signed_div(dst, lhs, rhs, code),
		&LirInstr::DivU(dst, lhs, rhs) => unsigned_div(dst, lhs, rhs, code),
		&LirInstr::RemS(dst, lhs, rhs) => signed_rem(dst, lhs, rhs, code),
		&LirInstr::RemU(dst, lhs, rhs) => unsigned_rem(dst, lhs, rhs, code),
		&LirInstr::Add64(dst, lhs, rhs) => add_i64(dst, lhs, rhs, code),
		crate::lir::LirInstr::Sub64(_, _, _) => todo!(),
		crate::lir::LirInstr::Mul64(_, _, _) => todo!(),
		crate::lir::LirInstr::DivS64(_, _, _) => todo!(),
		crate::lir::LirInstr::DivU64(_, _, _) => todo!(),
		crate::lir::LirInstr::RemS64(_, _, _) => todo!(),
		crate::lir::LirInstr::RemU64(_, _, _) => todo!(),
		&LirInstr::Shl(dst, lhs, rhs) => {
			code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
			code.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
			code.push("function intrinsic:shl".to_string());
			code.push(format!("scoreboard players operation {dst} = %param0%0 reg"));
		}
		&LirInstr::ShrS(dst, lhs, rhs) => {
                        assert_ne!(rhs, dst);

                        code.push(format!("scoreboard players operation {rhs} %= %%32 reg"));

                        if dst != lhs {
                            code.push(format!("scoreboard players operation {dst} = {lhs}"));
                        }

                        for i in 1..31 {
                            code.push(format!("execute if score {rhs} matches {i} run scoreboard players operation {dst} /= %%{} reg", 1 << i))
                        }
                        code.push(format!("execute if score {rhs} matches 31 run execute store success score {dst} if score {dst} matches ..-1"));
                        code.push(format!("execute if score {rhs} matches 31 run scoreboard players operation {dst} *= %%-1 reg"));
		}
		&LirInstr::ShrU(dst, lhs, rhs) => {
			code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
			code.push("scoreboard players operation %param1%0 reg %= %%32 reg".to_string());
			code.push("function intrinsic:lshr".to_string());
			code.push(format!("scoreboard players operation {dst} = %param0%0 reg"));
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
		crate::lir::LirInstr::Shl64(_, _, _) => todo!(),
		crate::lir::LirInstr::ShrS64(_, _, _) => todo!(),
		crate::lir::LirInstr::ShrU64(_, _, _) => todo!(),
		crate::lir::LirInstr::Rotl64(_, _, _) => todo!(),
		crate::lir::LirInstr::Rotr64(_, _, _) => todo!(),

		&LirInstr::Xor(dst, lhs, rhs) => {
			code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
			code.push("function intrinsic:xor".to_string());
			code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
		}
		&LirInstr::And(dst, lhs, rhs) => {
			code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
			code.push("function intrinsic:and".to_string());
			code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
		} 
		&LirInstr::Or(dst, lhs, rhs) => {
			code.push(format!("scoreboard players operation %param0%0 reg = {lhs}"));
			code.push(format!("scoreboard players operation %param1%0 reg = {rhs}"));
			code.push("function intrinsic:or".to_string());
			code.push(format!("scoreboard players operation {dst} = %return%0 reg"));
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
		crate::lir::LirInstr::Ctz64(_, _) => todo!(),
		crate::lir::LirInstr::Clz64(_, _) => todo!(),
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
		&LirInstr::GeU(dst, lhs, rhs) => unsigned_greater_than_eq(dst, lhs, rhs, code),
		&LirInstr::LtU(dst, lhs, rhs) => unsigned_less_than(dst, lhs, rhs, code),
		&LirInstr::LeU(dst, lhs, rhs) => {
			unsigned_less_than(dst, lhs, rhs, code);
			// TODO: This assumes lhs and rhs are not mutated
			code.push(format!("execute if score {dst} matches 0 run execute store success score {dst} if score {lhs} = {rhs}"));
		}

		crate::lir::LirInstr::GtS64(_, _, _) => todo!(),
		crate::lir::LirInstr::GtU64(_, _, _) => todo!(),
		crate::lir::LirInstr::GeS64(_, _, _) => todo!(),
		crate::lir::LirInstr::GeU64(_, _, _) => todo!(),
		crate::lir::LirInstr::LtS64(_, _, _) => todo!(),
		crate::lir::LirInstr::LtU64(_, _, _) => todo!(),
		crate::lir::LirInstr::LeS64(_, _, _) => todo!(),
		crate::lir::LirInstr::LeU64(_, _, _) => todo!(),
		crate::lir::LirInstr::Eq64(_, _, _) => todo!(),
		crate::lir::LirInstr::Ne64(_, _, _) => todo!(),
		crate::lir::LirInstr::Trunc(_, _) => todo!(),

		&LirInstr::SignExtend8(reg) => {
			code.push(format!("scoreboard players operation {reg} %= %%{} reg", u8::MAX as i32 + 1));
			let lo_bound = i8::MIN as u8 as u32;
			let hi_bound = u8::MAX as u8 as u32;
			let high_bits = u32::MAX as i32 & !(u8::MAX as i32);
			code.push(format!("execute if score {reg} matches {lo_bound}..{hi_bound} run scoreboard players add {reg} {high_bits}"));
		}
		&LirInstr::SignExtend16(reg) => {
			code.push(format!("scoreboard players operation {reg} %= %%{} reg", u16::MAX as i32 + 1));
			let lo_bound = i16::MIN as u16 as u32;
			let hi_bound = u16::MAX as u16 as u32;
			let high_bits = u32::MAX as i32 & !(u16::MAX as i32);
			code.push(format!("execute if score {reg} matches {lo_bound}..{hi_bound} run scoreboard players add {reg} {high_bits}"));
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
		&LirInstr::Store32(src, addr) => mem_store_32(src, addr, code),
		&LirInstr::Store16(src, addr) => mem_store_16(src, addr, code),
		&LirInstr::Store8 (src, addr) => mem_store_8 (src, addr, code),
		&LirInstr::Load32(dst, addr) => mem_load_32(dst, addr, code),
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
		&LirInstr::CallIndirect { table_index, table_entry } => {
			if jump_mode() == JumpMode::Direct {
				let cond_taken = Register::cond_taken();

				code.push(format!("scoreboard players set {cond_taken} 0"));

				// TODO: Should this print some kind of error if none of the cases are matched?

				let table = &parent.tables[table_index as usize];
				for (idx, arm) in table.elements.iter().enumerate() {
					let arm = arm.unwrap();
					let arm_func = get_mc_id(BlockId { func: arm, block: 0 });
					code.push(format!("execute if score {cond_taken} matches 0 run execute if score {table_entry} matches {idx} run function {arm_func}"));
				}
			} else {
				todo!()
			}
		}
		&LirInstr::Push(reg) => push_data(reg, code),
		&LirInstr::Pop(reg) => pop_data(reg, code),
		LirInstr::IfCond { cond, instr } => {
			let mut child = Vec::new();
			emit_instr(instr, parent, &mut child);

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
	}
}

fn emit_block(block_id: BlockId, block: &LirBasicBlock, parent: &LirProgram) -> Function {
	let mut code: Vec<String> = Vec::new();

	for instr in block.body.iter() {
		emit_instr(instr, parent, &mut code);
	}
	
	match &block.term {
		&LirTerminator::Jump(target) => {
			if jump_mode() == JumpMode::Direct {
				code.push(format!("function {}", get_mc_id(target)));
			} else {
				todo!()
			}
		}
		&LirTerminator::JumpIf { true_label, false_label, cond } => {
			if jump_mode() == JumpMode::Direct {
				let cond_taken = Register::cond_taken();
				let true_func = get_mc_id(true_label);
				let false_func = get_mc_id(false_label);
				code.push(format!("scoreboard players set {cond_taken} 0"));
				code.push(format!("execute unless score {cond} matches 0 run function {}", true_func));
				code.push(format!("execute if score {cond_taken} matches 0 run function {}", false_func));
			} else {
				todo!()
			}
		}
		LirTerminator::JumpTable { arms, default, cond } => {
			if jump_mode() == JumpMode::Direct {
				let cond_taken = Register::cond_taken();
				let default_func = get_mc_id(*default);

				code.push(format!("scoreboard players set {cond_taken} 0"));

				for (idx, arm) in arms.iter().enumerate() {
					let arm_func = get_mc_id(*arm);
					code.push(format!("execute if score {cond_taken} matches 0 run execute if score {cond} matches {idx} run function {arm_func}"));
				}

				code.push(format!("execute if score {cond_taken} matches 0 run function {default_func}"));
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
	}
	
	if jump_mode() == JumpMode::Direct {
		code.push(format!("scoreboard players set {} 1", Register::cond_taken()));
	}

	let cmds = code.into_iter().map(|c| c.parse().unwrap()).collect::<Vec<Command>>();

	let block_id_str = get_mc_id(block_id);
	let (rest, ident) = FunctionIdent::parse_from_command(&block_id_str).unwrap();
	assert!(rest.is_empty());

	Function { id: ident, cmds }
}

fn emit_function(func: &LirFunction, parent: &LirProgram) -> Vec<Function> {
	let mut result = Vec::new();
	for (block_id, block) in func.code.iter() {
		result.push(emit_block(*block_id, block, parent));
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
				.filter(|l| !l.is_empty() && !l.starts_with('#'))
				.map(|l| l.parse().unwrap()).collect::<Vec<_>>();

			let func_name = format!("intrinsic:{}", file_name.strip_suffix(".mcfunction").unwrap());
			let func_ident: FunctionIdent = parse_command(&func_name).unwrap();

			result.push(Function { id: func_ident, cmds });
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

pub fn emit_program(lir_program: &LirProgram) -> Vec<Function> {
	let mut result = Vec::new();
	result.extend(load_intrinsics());

	for func in lir_program.code.iter() {
		result.extend(emit_function(func, lir_program));
	}

	let init_func = create_init_func(lir_program);
	result.push(init_func);

	result
}