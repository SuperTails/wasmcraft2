#[path="sexpr.rs"]
mod sexpr;

/*
pub mod wasm_suite_prelude {
	use wasm_runner::{wasm_file::WasmFile, ssa::{interp::{SsaInterpreter, TypedValue}, lir_emitter}, validator::wasm_to_ssa, lir::interp::LirInterpreter};

	use super::sexpr::SExpr;

	pub struct TestState<'a> {
		wasm_file: WasmFile<'a>,
	
		interp: SsaInterpreter
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
				SExpr::Node { name, .. } if name == "assert_malformed" => {
					println!("Ignoring AssertMalformed");
				}
				SExpr::Node { name, .. } if name == "assert_exhaustion" => {
					println!("Ignoring AssertExhaustion");
				}
				_ => todo!("{:?}", arg),
			}
		}
	}

	pub fn load_state(wasm_data: &[u8]) -> TestState {
		let wasm_file = WasmFile::from(wasm_data);

		let program = wasm_to_ssa(&wasm_file);

		let interp = SsaInterpreter::new(program);

		//let program = lir_emitter::convert(program);

		TestState { wasm_file, interp }
	}

}
*/

/*
pub mod wasm_suite_prelude {
	use wasm_runner::{wasm_file::WasmFile, ssa::{interp::{SsaInterpreter, TypedValue}, lir_emitter}, validator::wasm_to_ssa, lir::interp::LirInterpreter};

	use super::sexpr::SExpr;

	pub struct TestState<'a> {
		wasm_file: WasmFile<'a>,
	
		interp: LirInterpreter 
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

				test_state.interp.call(func_idx, &func_params);

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
				SExpr::Node { name, .. } if name == "assert_malformed" => {
					println!("Ignoring AssertMalformed");
				}
				SExpr::Node { name, .. } if name == "assert_exhaustion" => {
					println!("Ignoring AssertExhaustion");
				}
				_ => todo!("{:?}", arg),
			}
		}
	}

	pub fn load_state(wasm_data: &[u8]) -> TestState {
		let wasm_file = WasmFile::from(wasm_data);

		let program = wasm_to_ssa(&wasm_file);

		let program = lir_emitter::convert(program);

		let interp = LirInterpreter::new(program);

		//let program = lir_emitter::convert(program);

		TestState { wasm_file, interp }
	}
}
*/


pub mod wasm_suite_prelude {
	use command_parser::CommandParse;
use datapack_common::functions::command_components::{FunctionIdent, ScoreHolder, Objective};
use datapack_vm::Interpreter;
	use wasm_runner::{wasm_file::WasmFile, ssa::{interp::{SsaInterpreter, TypedValue}, lir_emitter, BlockId}, validator::wasm_to_ssa, lir::{interp::LirInterpreter, Register}, pack_emitter::{self, get_mc_id}};
use wasmparser::Type;

	use super::sexpr::SExpr;

	pub struct TestState<'a> {
		wasm_file: WasmFile<'a>,
	
		interp: Interpreter,
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

				for (param_idx, param) in func_params.into_iter().enumerate() {
					match param {
						TypedValue::I32(v) => {
							let param_pair = Register::param_lo(param_idx as u32).to_string();
							let (holder, obj) = param_pair.split_once(' ').unwrap();
							let holder = ScoreHolder::new(holder.to_owned()).unwrap();
							let obj = Objective::new(obj.to_owned()).unwrap();
							test_state.interp.scoreboard.set(&holder, &obj, v);
						}
						TypedValue::I64(v) => {
							let v_lo = v as i32;
							let v_hi = (v >> 32) as i32;

							let param_pair_lo = Register::param_lo(param_idx as u32).to_string();
							let (holder_lo, obj_lo) = param_pair_lo.split_once(' ').unwrap();
							let holder_lo = ScoreHolder::new(holder_lo.to_owned()).unwrap();
							let obj_lo = Objective::new(obj_lo.to_owned()).unwrap();
							test_state.interp.scoreboard.set(&holder_lo, &obj_lo, v_lo);

							let param_pair_hi = Register::param_hi(param_idx as u32).to_string();
							let (holder_hi, obj_hi) = param_pair_hi.split_once(' ').unwrap();
							let holder_hi = ScoreHolder::new(holder_hi.to_owned()).unwrap();
							let obj_hi = Objective::new(obj_hi.to_owned()).unwrap();
							test_state.interp.scoreboard.set(&holder_hi, &obj_hi, v_hi);
						}
					}
				}

				let func_idx = test_state.wasm_file.find_func(func_name).unwrap();
				let return_tys = &test_state.wasm_file.func_type(func_idx).returns;
				let func_name = get_mc_id(BlockId { func: func_idx, block: 0 });
				let (_, func_name) = FunctionIdent::parse_from_command(&func_name).unwrap();

				let interp_idx = test_state.interp.get_func_idx(&func_name);
				test_state.interp.set_pos(interp_idx);

				test_state.interp.run_to_end().unwrap();

				return_tys.iter().enumerate().map(|(idx, ty)| {
					match ty {
						Type::I32 => {
							let param_pair = Register::return_lo(idx as u32).to_string();
							let (holder, obj) = param_pair.split_once(' ').unwrap();
							let holder = ScoreHolder::new(holder.to_owned()).unwrap();
							let obj = Objective::new(obj.to_owned()).unwrap();
							let v = test_state.interp.scoreboard.get(&holder, &obj).unwrap();

							TypedValue::I32(v)
						}
						Type::I64 => {
							let param_pair_lo = Register::return_lo(idx as u32).to_string();
							let (holder_lo, obj_lo) = param_pair_lo.split_once(' ').unwrap();
							let holder_lo = ScoreHolder::new(holder_lo.to_owned()).unwrap();
							let obj_lo = Objective::new(obj_lo.to_owned()).unwrap();
							let v_lo = test_state.interp.scoreboard.get(&holder_lo, &obj_lo).unwrap();

							let param_pair_hi = Register::return_hi(idx as u32).to_string();
							let (holder_hi, obj_hi) = param_pair_hi.split_once(' ').unwrap();
							let holder_hi = ScoreHolder::new(holder_hi.to_owned()).unwrap();
							let obj_hi = Objective::new(obj_hi.to_owned()).unwrap();
							let v_hi = test_state.interp.scoreboard.get(&holder_hi, &obj_hi).unwrap();

							let v = (v_lo as u32 as i64) | ((v_hi as i64) << 32);
							TypedValue::I64(v)
						}
						_ => todo!(),
					}
				}).collect()
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
				SExpr::Node { name, .. } if name == "assert_malformed" => {
					println!("Ignoring AssertMalformed");
				}
				SExpr::Node { name, .. } if name == "assert_exhaustion" => {
					println!("Ignoring AssertExhaustion");
				}
				_ => todo!("{:?}", arg),
			}
		}
	}

	pub fn load_state(wasm_data: &[u8]) -> TestState {
		let wasm_file = WasmFile::from(wasm_data);

		let program = wasm_to_ssa(&wasm_file);

		let program = lir_emitter::convert(program);

		let program = pack_emitter::emit_program(&program);

		let mut interp = Interpreter::new(program, 0);

		let (_, func_name) = FunctionIdent::parse_from_command("wasmrunner:init").unwrap();

		let interp_idx = interp.get_func_idx(&func_name);
		interp.set_pos(interp_idx);

		interp.run_to_end().unwrap();

		TestState { wasm_file, interp }
	}
}