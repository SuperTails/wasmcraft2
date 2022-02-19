use command_parser::CommandParse;

use crate::{validator::wasm_to_ssa, ssa::lir_emitter, lir::interp::LirInterpreter};

pub mod wasm_file;
pub mod validator;
pub mod ssa;
pub mod lir;
pub mod pack_emitter;

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

	let lir_program = lir_emitter::convert(ssa_program);

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

	for func in datapack.iter() {
		println!("-------- func {} --------", func.id);
		for cmd in func.cmds.iter() {
			println!("\t{}", cmd);
		}
		println!();
	}

	pack_emitter::persist_program(std::path::Path::new(output_path), &datapack);

	let mut interp = datapack_vm::Interpreter::new(datapack, 0);

	let (_, func_name) = datapack_common::functions::command_components::FunctionIdent::parse_from_command("wasmrunner:init").unwrap();

	let interp_idx = interp.get_func_idx(&func_name);
	interp.set_pos(interp_idx);

	interp.run_to_end().unwrap();

	let (_, func_name) = datapack_common::functions::command_components::FunctionIdent::parse_from_command("wasmrunner:_start").unwrap();

	let interp_idx = interp.get_func_idx(&func_name);
	interp.set_pos(interp_idx);

	interp.run_to_end().unwrap();

	let mut traces = interp.traces.into_iter().collect::<Vec<_>>();
	traces.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
	let total: usize = traces.iter().map(|(_, c)| *c).sum();

	println!("\nTOTAL: {}", total);
	for (id, count) in traces {
		println!("{}: {}", id, count);
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum JumpMode {
	Direct,
}

pub fn jump_mode() -> JumpMode {
	JumpMode::Direct
}