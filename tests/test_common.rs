#[path="sexpr.rs"]
mod sexpr;

pub mod wasm_suite_prelude {
	use std::collections::HashMap;

	use wasm_runner::{wasm_file::WasmFile, ssa::{interp::{SsaInterpreter, TypedValue}, BlockId, SsaBasicBlock}, validator};

	use super::sexpr::SExpr;

	pub struct TestState<'a> {
		wasm_file: WasmFile<'a>,
	
		interp: SsaInterpreter
	}

	impl<'a> TestState<'a> {
		pub fn eval(&mut self, expr: &SExpr) -> Vec<TypedValue> {
			match expr {
				SExpr::Node { name, params } if name == "invoke" => {
					let func_name = &params[0];
					let func_params = &params[1..];

					let func_name = if let SExpr::String(s) = func_name {
						&**s
					} else {
						panic!()
					};

					let func_params = func_params.iter().map(|p| self.eval_single(p)).collect::<Vec<_>>();

					let func_idx = self.wasm_file.find_func(func_name).unwrap();

					self.interp.call(func_idx, func_params);

					self.interp.run_until_halted()
				}
				SExpr::Node { name, params } if name == "i32.const" => {
					if let [SExpr::Int(i)] = &params[..] {
						vec![TypedValue::I32((*i) as i32)]
					} else {
						panic!()
					}
				}
				_ => todo!("{:?}", expr)
			}

		}

		pub fn eval_single(&mut self, expr: &SExpr) -> TypedValue {
			let values = self.eval(expr);

			assert_eq!(values.len(), 1);

			values.into_iter().next().unwrap()
		}

		pub fn run_check(&mut self, arg: &str) {
			let arg: SExpr = arg.parse().unwrap();

			match arg {
				SExpr::AssertReturn(expr, expected) => {
					let actual = self.eval(&expr);

					let expected = expected.into_iter().map(|s| self.eval_single(&s)).collect::<Vec<_>>();

					assert_eq!(actual, expected);
				}
				SExpr::AssertTrap(_, _) => {
					println!("Ignoring AssertTrap");
				}
				SExpr::Node { name, .. } if name == "assert_invalid" => {
					println!("Ignoring AssertInvalid");
				}
				_ => todo!("{:?}", arg),
			}
		}
	}

	pub fn load_state(wasm_data: &[u8]) -> TestState {
		let wasm_file = WasmFile::from(wasm_data);

		let mut blocks = HashMap::new();

		let mut local_types = HashMap::new();

		for func in 0..wasm_file.functions.functions.len() {
			if wasm_file.func_is_defined(func) {
				blocks.extend(validator::validate(&wasm_file, func));
				local_types.insert(func, wasm_file.func_locals(func));
			}
		}

		let interp = SsaInterpreter::new(local_types, blocks);

		TestState { wasm_file, interp }
	}
}