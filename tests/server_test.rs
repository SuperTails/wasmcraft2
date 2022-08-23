#[path="sexpr.rs"]
mod sexpr;

use rcon::{Connection, AsyncStdStream};
use wasmcraft::{ssa::{interp::TypedValue, lir_emitter, BlockId}, lir::Register, wasm_file::WasmFile, validator::wasm_to_ssa, pack_emitter::{self, get_mc_id}, CompileContext};
use wasmparser::ValType;

type Server = Connection<AsyncStdStream>;

async fn connect_to_server() -> Server {
	let address = "localhost:25575";
	<Connection<AsyncStdStream>>::builder()
	    .enable_minecraft_quirks(true)
	    .connect(address, "test")
	    .await.unwrap()
	
	//let resp = conn.cmd("scoreboard players set %param%0%lo reg 123").await.unwrap();
	//println!("resp: {}", resp);
}

async fn scoreboard_set_reg(server: &mut Server, reg: Register, val: i32) {
	let pair = reg.to_string();
	let (holder, obj) = pair.split_once(' ').unwrap();
	scoreboard_set(server, holder, obj, val).await;
}

async fn scoreboard_set(server: &mut Server, holder: &str, obj: &str, val: i32) {
	let cmd = format!("scoreboard players set {holder} {obj} {val}");
	let resp = server.cmd(&cmd).await.unwrap();
	let ex = format!("Set [{obj}] for {holder} to {val}");
	assert_eq!(resp, ex);
}

async fn scoreboard_get_reg(server: &mut Server, reg: Register) -> i32 {
	let pair = reg.to_string();
	let (holder, obj) = pair.split_once(' ').unwrap();
	scoreboard_get(server, holder, obj).await
}

async fn scoreboard_get(server: &mut Server, holder: &str, obj: &str) -> i32 {
	let cmd = format!("scoreboard players get {holder} {obj}");
	let resp = server.cmd(&cmd).await.unwrap();

	let ex_prefix = format!("{holder} has ");
	let ex_suffix = format!(" [{obj}]");

	let resp = resp.strip_prefix(&ex_prefix).unwrap().strip_suffix(&ex_suffix).unwrap();
	resp.parse().unwrap()
}

async fn set_params(server: &mut Server, func_params: &[TypedValue]) {
	for (param_idx, param) in func_params.iter().enumerate() {
		match *param {
			TypedValue::I32(v) => {
				let reg = Register::param_lo(param_idx as u32);
				scoreboard_set_reg(server, reg, v).await;
			}
			TypedValue::I64(v) => {
				let v_lo = v as i32;
				let v_hi = (v >> 32) as i32;

				let param_pair_lo = Register::param_lo(param_idx as u32);
				scoreboard_set_reg(server, param_pair_lo, v_lo).await;

				let param_pair_hi = Register::param_hi(param_idx as u32);
				scoreboard_set_reg(server, param_pair_hi, v_hi).await;
			}
		}
	}
}

async fn get_returns(server: &mut Server, return_tys: &[ValType]) -> Vec<TypedValue> {
	let mut result = Vec::new();

	for (idx, ty) in return_tys.iter().enumerate() {
		match ty {
			ValType::I32 => {
				let param_pair = Register::return_lo(idx as u32);
				let val = scoreboard_get_reg(server, param_pair).await;

				result.push(TypedValue::I32(val));
			}
			ValType::I64 => {
				let param_pair_lo = Register::return_lo(idx as u32);
				let v_lo = scoreboard_get_reg(server, param_pair_lo).await;

				let param_pair_hi = Register::return_hi(idx as u32);
				let v_hi = scoreboard_get_reg(server, param_pair_hi).await;

				let v = (v_lo as u32 as i64) | ((v_hi as i64) << 32);
				result.push(TypedValue::I64(v));
			}
			_ => todo!(),
		}
	}

	result
}

pub fn load_state(wasm_data: &[u8]) -> TestState {
	async_std::task::block_on(load_state_async(wasm_data))
}

async fn load_state_async(wasm_data: &[u8]) -> TestState {
	let ctx = CompileContext::new_from_opt(1);

	let wasm_file = ctx.compute_wasm_file(wasm_data);
	let program = ctx.compute_ssa(&wasm_file);
	let program = ctx.compute_lir(program);
	let program = ctx.compute_datapack(&program);

	pack_emitter::persist_program(std::path::Path::new("./tests/server_test/world/datapacks/out"), &program);

	let mut server = connect_to_server().await;

	let resp = server.cmd("reload").await.unwrap();
	println!("{:?}", resp);
	let resp = server.cmd("gamerule maxCommandChainLength 10000000").await.unwrap();
	println!("{:?}", resp);
	let resp = server.cmd("function wasmrunner:init").await.unwrap();
	println!("{:?}", resp);

	TestState { server, wasm_file }

}

use sexpr::SExpr;

pub struct TestState<'a> {
	wasm_file: WasmFile<'a>,
	server: Server,
}

async fn eval(expr: &SExpr, test_state: Option<&mut TestState<'_>>) -> Vec<TypedValue> {
	match expr {
		SExpr::Node { name, params } if name == "invoke" => {
			let test_state = test_state.unwrap();
			
			let func_name = &params[0];
			let func_params2 = &params[1..];

			let func_name = if let SExpr::String(s) = func_name {
				&**s
			} else {
				panic!()
			};

			let mut func_params = Vec::new();
			for p in func_params2.iter() {
				func_params.push(eval_single(p, Some(test_state)).await);
			}

			set_params(&mut test_state.server, &func_params).await;

			let func_idx = test_state.wasm_file.find_func(func_name).unwrap();
			let return_tys = &test_state.wasm_file.func_type(func_idx).returns;
			let func_name = get_mc_id(BlockId { func: func_idx, block: 0 });

			println!("Calling func {func_name} with params {:?}", func_params);

			let resp = test_state.server.cmd(&format!("function {func_name}")).await.unwrap();
			println!("TODO: {resp}");

			get_returns(&mut test_state.server, return_tys).await
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

/*async fn eval_single<'a>(expr: &'a SExpr, test_state: Option<&'a mut TestState<'a>>) -> TypedValue {
	let values = eval(expr, test_state).await;

	assert_eq!(values.len(), 1);

	values.into_iter().next().unwrap()
}*/

async fn eval_single(expr: &SExpr, _test_state: Option<&mut TestState<'_>>) -> TypedValue {
	match expr {
		SExpr::Node { name, params } if name == "i32.const" => {
			if let [SExpr::Int(i)] = &params[..] {
				TypedValue::I32((*i) as i32)
			} else {
				panic!()
			}
		}
		SExpr::Node { name, params } if name == "i64.const" => {
			if let [SExpr::Int(i)] = &params[..] {
				TypedValue::I64(*i)
			} else {
				panic!()
			}
		}
		_ => todo!("{:?}", expr)
	}
}

impl<'a> TestState<'a> {
	async fn eval(&mut self, expr: &SExpr) -> Vec<TypedValue> {
		eval(expr, Some(self)).await
	}

	async fn eval_single(&mut self, expr: &SExpr) -> TypedValue {
		eval_single(expr, Some(self)).await
	}

	pub fn run_check(&mut self, arg: &str) {
		async_std::task::block_on(self.run_check_async(arg))
	}

	async fn run_check_async(&mut self, arg: &str) {
		println!("\n{}", arg);

		let arg: SExpr = arg.parse().unwrap();

		match arg {
			SExpr::AssertReturn(expr, expected) => {
				let actual = self.eval(&expr).await;

				let mut expected2 = Vec::new();
				for s in expected.into_iter() {
					expected2.push(self.eval_single(&s).await);
				}

				assert_eq!(actual, expected2);
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