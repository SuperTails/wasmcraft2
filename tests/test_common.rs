#[path="sexpr.rs"]
mod sexpr;

pub mod wasm_suite_prelude {
	use std::collections::HashMap;

	use wasm_runner::{wasm_file::WasmFile, ssa::{interp::SsaInterpreter, BlockId, SsaBasicBlock}, validator};

	use super::sexpr::SExpr;

	pub struct TestState<'a> {
		wasm_file: WasmFile<'a>,
	
		interp: SsaInterpreter
	}

	impl<'a> TestState<'a> {
		pub fn run_check(&mut self, arg: &str) {
			let arg: SExpr = arg.parse().unwrap();

			todo!("{:?}", arg)

			//self.interp.reset_transient_vars();

			//self.interp.start_at();
		}
	}

	pub fn load_state(wasm_data: &[u8]) -> TestState {
		let wasm_file = WasmFile::from(wasm_data);

		let mut blocks = HashMap::new();

		for func in 0..wasm_file.functions.functions.len() {
			if wasm_file.func_is_defined(func) {
				blocks.extend(validator::validate(&wasm_file, func));
			}
		}

		let interp = SsaInterpreter::new(blocks);

		TestState { wasm_file, interp }
	}
}