use crate::{validator::wasm_to_ssa, ssa::lir_emitter, lir::interp::LirInterpreter};

pub mod wasm_file;
pub mod validator;
pub mod ssa;
pub mod lir;

pub fn run(path: &str) {
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
	
	for (idx, lir_func) in lir_program.code.iter().enumerate() {
		println!("\n\n============ Function {} ============", idx);
		for (block_id, block) in lir_func.code.iter() {
			println!("\n----------- Block {:?} ---------", block_id);
			for instr in block.body.iter() {
				println!("    {:?}", instr);
			}
			println!("    {:?}", block.term);
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum JumpMode {
	Direct,
}

pub fn jump_mode() -> JumpMode {
	JumpMode::Direct
}