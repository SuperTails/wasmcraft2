use command_parser::CommandParse;
use datapack_common::functions::command_components::NbtPath;

use crate::{validator::wasm_to_ssa, ssa::lir_emitter, lir::interp::LirInterpreter};

pub mod wasm_file;
pub mod validator;
pub mod ssa;
pub mod lir;
pub mod pack_emitter;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CodegenStage {
	Ssa,
	Lir,
	Datapack,
}

const CODEGEN_STAGE: CodegenStage = CodegenStage::Datapack;

const RUN_PROGRAM: bool = true;

const PRINT_OUTPUT: bool = false;

pub fn run(path: &str, output_path: &str) {
	let bytes = std::fs::read(path).unwrap();

	let file = wasm_file::WasmFile::from(&bytes[..]);

	println!("{:?}", file.types);
	println!("{:?}", file.globals);
	println!("{:?}", file.memory);
	println!("{:?}", file.exports);
	println!("{:?}", file.imports);
	println!("{:?}", file.data);
	println!("{:?}", file.tables);
	println!("<elements>");
	println!("{:?}", file.functions);
	//println!("{:?}", file.bodies);
	println!("{:?}", file.bodies.len());

	let ssa_program = wasm_to_ssa(&file);

	if CODEGEN_STAGE == CodegenStage::Ssa {
		return;
	}

	let lir_program = lir_emitter::convert(ssa_program);

	if CODEGEN_STAGE == CodegenStage::Lir {
		return;
	}

	let datapack = pack_emitter::emit_program(&lir_program);
	
	/*for (idx, lir_func) in lir_program.code.iter().enumerate() {
		println!("\n\n============ Function {} ============", idx);
		for (block_id, block) in lir_func.code.iter() {
			println!("\n----------- Block {:?} ---------", block_id);
			for instr in block.body.iter() {
				println!("    {:?}", instr);
			}
			println!("    {:?}", block.term);
		}
	}*/

	if PRINT_OUTPUT {
		for func in datapack.iter() {
			println!("-------- func {} --------", func.id);
			for cmd in func.cmds.iter() {
				println!("\t{}", cmd);
			}
			println!();
		}
	}

	pack_emitter::persist_program(std::path::Path::new(output_path), &datapack);

	if RUN_PROGRAM {
		let mut intrin_cum_times = vec![0; datapack.len()];
		let mut intrin_visited = vec![false; datapack.len()];
		let intrin_funcs = datapack.iter().map(|func| func.id.namespace == "intrinsic").collect::<Vec<_>>();


		let mut interp = datapack_vm::Interpreter::new(datapack, 0);


		let (_, func_name) = datapack_common::functions::command_components::FunctionIdent::parse_from_command("wasmrunner:init").unwrap();

		let interp_idx = interp.get_func_idx(&func_name);
		interp.set_pos(interp_idx);

		interp.run_to_end().unwrap();

		let (_, func_name) = datapack_common::functions::command_components::FunctionIdent::parse_from_command("wasmrunner:_start").unwrap();

		let interp_idx = interp.get_func_idx(&func_name);
		interp.set_pos(interp_idx);

		while !interp.halted() {
			for v in intrin_visited.iter_mut() {
				*v = false;
			}

			for &(func_idx, _) in interp.call_stack_raw().iter().rev() {
				if !intrin_funcs[func_idx] {
					break
				}
				
				if !intrin_visited[func_idx] {
					intrin_visited[func_idx] = true;
					intrin_cum_times[func_idx] += 1;
				}
			}

			let result = interp.step();
			if let Err(result) = result {
				println!("ERR: {:?}", result);
				break
			}

			if interp.call_stack_depth() == 0 {
				let name = "wasm:returnstack".to_string();
				let path: NbtPath = command_parser::parse_command("stack.data").unwrap();
				if interp.nbt_storage.contains_path(&name, &path) {
					println!("return stack was not empty");
					break;
				}
			}

			if interp.call_stack_depth() == 1 {
				let top = interp.call_stack_top().unwrap().0;
				if top.id.namespace == "wasmrunner" && top.id.path == "wasm_8_3" {
					println!("Panicked!");
					break;
				}
			}
		}

		println!("Call stack:");
		for entry in interp.call_stack() {
			print!("{{ {}, {} }},", entry.0.id, entry.1);
		}
		println!();

		println!("NBT Storage:");
		for (key, value) in interp.nbt_storage.0.iter() {
			println!("{key}: {value}");
		}

		let mut traces = interp.indiv_time.into_iter().collect::<Vec<_>>();
		traces.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
		let total: usize = traces.iter().map(|(_, c)| *c).sum();

		println!("\nTOTAL: {}", total);
		for (id, count) in traces {
			println!("{}: {}", id, count);
		}

		println!("\nIntrinsic cumulative times:");
		let mut intrin_cum_times = intrin_cum_times.iter().enumerate().map(|(f, c)| (&interp.program[f], *c)).collect::<Vec<_>>();
		intrin_cum_times.sort_by_key(|(_, c)| -*c);
		for (func, count) in intrin_cum_times.iter() {
			if *count == 0 {
				break;
			}
			println!("{}: {}", func.id, count);
		}
	}
}

// TODO: Test mixed-tick tables

#[derive(Debug, PartialEq, Eq)]
pub enum JumpMode {
	Direct,
}

pub fn jump_mode() -> JumpMode {
	JumpMode::Direct
}