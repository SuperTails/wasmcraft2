#[path="sexpr.rs"]
mod sexpr;

pub mod wasm_suite_prelude {
	use std::collections::HashMap;

	use wasm_runner::{wasm_file::WasmFile, ssa::{interp::{SsaInterpreter, TypedValue}, BlockId, SsaBasicBlock}, validator};
use wasmparser::{InitExpr, Operator};

	use super::sexpr::SExpr;

	pub struct TestState<'a> {
		wasm_file: WasmFile<'a>,
	
		interp: SsaInterpreter
	}

	pub fn eval_init_expr(init_expr: &InitExpr) -> Vec<TypedValue> {
		let ops = init_expr.get_operators_reader().into_iter().map(|o| o.unwrap()).collect::<Vec<_>>();

		match &ops[..] {
			&[Operator::I32Const { value }, Operator::End] => {
				vec![TypedValue::I32(value)]
			}
			ops => todo!("{:?}", ops)
		}
	}

	pub fn eval_init_expr_single(init_expr: &InitExpr) -> TypedValue {
		let result = eval_init_expr(init_expr);
		assert_eq!(result.len(), 1);
		result.into_iter().next().unwrap()
	}

	pub fn eval(expr: &SExpr, test_state: Option<&mut TestState>) -> Vec<TypedValue> {
		match expr {
			SExpr::Node { name, params } if name == "invoke" => {
				let test_state = test_state.unwrap();
				
				let func_name = &params[0];
				let func_params = &params[1..];

				let func_name = if let SExpr::String(s) = func_name {
					&**s
				} else {
					panic!()
				};

				let func_params = func_params.iter().map(|p| eval_single(p, Some(test_state))).collect::<Vec<_>>();

				let func_idx = test_state.wasm_file.find_func(func_name).unwrap();

				test_state.interp.call(func_idx, func_params);

				test_state.interp.run_until_halted()
			}
			SExpr::Node { name, params } if name == "i32.const" => {
				if let [SExpr::Int(i)] = &params[..] {
					vec![TypedValue::I32((*i) as i32)]
				} else {
					panic!()
				}
			}
			SExpr::Node { name, params } if name == "i64.const" => {
				if let [SExpr::Int(i)] = &params[..] {
					vec![TypedValue::I64(*i)]
				} else {
					panic!()
				}
			}
			_ => todo!("{:?}", expr)
		}
	}

	pub fn eval_single(expr: &SExpr, test_state: Option<&mut TestState>) -> TypedValue {
		let values = eval(expr, test_state);

		assert_eq!(values.len(), 1);

		values.into_iter().next().unwrap()
	}

	impl<'a> TestState<'a> {
		pub fn eval(&mut self, expr: &SExpr) -> Vec<TypedValue> {
			eval(expr, Some(self))
		}

		pub fn eval_single(&mut self, expr: &SExpr) -> TypedValue {
			eval_single(expr, Some(self))
		}

		pub fn run_check(&mut self, arg: &str) {
			println!("\n{}", arg);

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

		let globals = wasm_file.globals().iter().map(|global| {
			let val = eval_init_expr_single(&global.init_expr);
			assert_eq!(val.ty(), global.ty.content_type);
			val
		}).collect();

		let interp = SsaInterpreter::new(local_types, globals, &wasm_file.memory.memory, blocks);

		TestState { wasm_file, interp }
	}
}