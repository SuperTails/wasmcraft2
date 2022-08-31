use std::collections::HashMap;

use wasmparser::{ValType, MemoryImmediate};

use crate::{ssa::{TypedSsaVar, const_prop::state_matches}, block_id_map::LocalBlockMap};

use super::{BlockId, SsaBasicBlock, SsaVar, SsaProgram, Memory, Table, SsaVarOrConst, const_prop::StaticState};

#[derive(Debug)]
pub struct Pc {
	block: BlockId,
	instr: usize,
}

#[derive(Debug)]
pub struct CallFrame {
	pc: Pc,
	locals: Vec<TypedValue>,
	var_context: VarContext,
	return_vars: Option<Vec<TypedSsaVar>>,
}

impl CallFrame {
	pub fn new(block: BlockId, local_tys: &[ValType], return_vars: Option<Vec<TypedSsaVar>>) -> Self {
		// TODO: Are locals zero-initialized???
		let locals = local_tys.iter().map(|ty| match ty {
			ValType::I32 => TypedValue::I32(0),
			ValType::I64 => TypedValue::I64(0),
			_ => panic!(),
		}).collect();

		Self {
			pc: Pc { block, instr: 0 },
			locals,
			var_context: VarContext::default(),
			return_vars,
		}
	}
}

#[derive(Debug)]
pub struct CallStack(Vec<CallFrame>);

impl CallStack {
	pub fn last(&self) -> Option<&CallFrame> {
		self.0.last()
	}

	pub fn pop(&mut self) -> CallFrame {
		self.0.pop().unwrap()
	}

	pub fn last_mut(&mut self) -> Option<&mut CallFrame> {
		self.0.last_mut()
	}

	pub fn is_empty(&self) -> bool {
		self.0.is_empty()
	}

	pub fn incr(&mut self, program: &HashMap<BlockId, SsaBasicBlock>) {
		let last = self.last_mut().unwrap();
		let block = program.get(&last.pc.block).unwrap();
		if last.pc.instr == block.body.len() {
			self.pop();
		} else {
			last.pc.instr += 1;
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypedValue {
	I32(i32),
	I64(i64),
}

impl TypedValue {
	pub fn ty(&self) -> ValType {
		match self {
			Self::I32(_) => ValType::I32,
			Self::I64(_) => ValType::I64,
		}
	}

	pub fn into_i32(self) -> Option<i32> {
		if let TypedValue::I32(v) = self {
			Some(v)
		} else {
			None
		}
	}

	pub fn into_i64(self) -> Option<i64> {
		if let TypedValue::I64(v) = self {
			Some(v)
		} else {
			None
		}
	}
}

impl From<i32> for TypedValue {
	fn from(v: i32) -> Self {
		TypedValue::I32(v)
	}
}

impl From<i64> for TypedValue {
	fn from(v: i64) -> Self {
		TypedValue::I64(v)
	}
}

#[derive(Default, Debug)]
pub struct VarContext(HashMap<SsaVar, TypedValue>);

impl VarContext {
	pub fn insert(&mut self, var: SsaVar, value: TypedValue) {
		if let Some(existing) = self.0.get_mut(&var) {
			assert_eq!(existing.ty(), value.ty());

			*existing = value;
		} else {
			self.0.insert(var, value);
		}
	}

	pub fn get(&self, var: SsaVar) -> Option<TypedValue> {
		self.0.get(&var).copied()
	}

	pub fn get_typed(&self, var: TypedSsaVar) -> Option<TypedValue> {
		if let Some(val) = self.get(var.into_untyped()) {
			assert_eq!(val.ty(), var.ty());
			Some(val)
		} else {
			None
		}
	}	
}

trait Var {
	fn eval(&self, var_context: &VarContext) -> Option<TypedValue>;
}

impl Var for TypedSsaVar {
	fn eval(&self, var_context: &VarContext) -> Option<TypedValue> {
		var_context.get_typed(*self)
	}
}

impl Var for SsaVarOrConst {
	fn eval(&self, var_context: &VarContext) -> Option<TypedValue> {
		match self {
			SsaVarOrConst::Var(v) => v.eval(var_context),
			SsaVarOrConst::Const(c) => Some(*c),
		}
	}
}

pub struct SsaInterpreter {
	local_types: HashMap<usize, Vec<ValType>>,
	globals: Vec<TypedValue>,
	memory: Vec<Memory>,
	tables: Vec<Table>,
	program: HashMap<BlockId, SsaBasicBlock>,
	constants: LocalBlockMap<StaticState>,
	call_stack: CallStack,
	steps: u64,
}

impl SsaInterpreter {
	pub fn new(program: SsaProgram) -> Self {
		let constants = program.code.iter().flat_map(super::const_prop::get_func_constants).collect();
		
		//let constants = HashMap::new();
		
		let code = program.code.into_iter().flat_map(|f| f.iter().map(|(i, b)| (i, (*b).clone())).collect::<Vec<_>>()).collect::<HashMap<_, _>>();

		Self {
			local_types: program.local_types,
			globals: program.globals,
			memory: program.memory,
			tables: program.tables,
			program: code,
			call_stack: CallStack(Vec::new()),
			constants,
			steps: 0,
		}
	}

	pub fn call(&mut self, func: usize, params: Vec<TypedValue>) {
		assert!(self.call_stack.is_empty());

		let locals = self.local_types.get(&func).unwrap();

		let block = BlockId { func, block: 0 };

		let mut frame = CallFrame::new(block, locals, None);
		assert!(frame.locals.len() >= params.len());
		for (local, param) in frame.locals.iter_mut().zip(params) {
			assert_eq!(local.ty(), param.ty());
			*local = param;
		}
		self.call_stack = CallStack(vec![frame]);
	}

	pub fn run_until_halted(&mut self) -> Vec<TypedValue> {
		loop {
			if let Some(result) = self.step() {
				assert!(self.is_halted());
				return result;
			}
		}
	}

	pub fn step(&mut self) -> Option<Vec<TypedValue>> {
		//println!("{:?}", self.call_stack);

		let frame = self.call_stack.last_mut().expect("stepped while halted");

		let block = self.program.get(&frame.pc.block).unwrap();

		self.steps += 1;

		if self.steps % 100_000 == 0 {
			println!("Steps: {}", self.steps);
		}

		if frame.pc.instr == block.body.len() {
			//println!("{:?}", block.term);

			match &block.term {
				super::SsaTerminator::Unreachable => unreachable!(),
				super::SsaTerminator::Jump(jump) |
				super::SsaTerminator::ScheduleJump(jump, _) => {
					let target_block = self.program.get(&jump.label).unwrap();
					assert_eq!(target_block.params.len(), jump.params.len());
					for (&dst, &src) in target_block.params.iter().zip(jump.params.iter()) {
						assert_eq!(dst.ty(), src.ty());
						
						let val = frame.var_context.get_typed(src).unwrap();
						frame.var_context.insert(dst.into_untyped(), val);
					}

					frame.pc = Pc { block: jump.label, instr: 0 };

					None
				}
				super::SsaTerminator::BranchIf { cond, true_target, false_target } => {
					let cond = frame.var_context.get_typed(*cond).unwrap();
					let cond = cond.into_i32().unwrap();

					let target = if cond != 0 { true_target } else { false_target };

					let target_block = self.program.get(&target.label).unwrap();
					/*println!("cond was {}", if cond != 0 { "true" } else { "false" });
					println!("target params: {:?}", target_block.params);*/
					assert_eq!(target_block.params.len(), target.params.len());
					for (&dst, &src) in target_block.params.iter().zip(target.params.iter()) {
						assert_eq!(dst.ty(), src.ty());

						let val = frame.var_context.get_typed(src).unwrap();
						frame.var_context.insert(dst.into_untyped(), val);
					}

					frame.pc = Pc { block: target.label, instr: 0 };

					None
				}
				super::SsaTerminator::BranchTable { cond, default, arms } => {
					let cond = frame.var_context.get_typed(*cond).unwrap();
					let cond = cond.into_i32().unwrap() as usize;

					let target = if arms.len() <= cond {
						default
					} else {
						&arms[cond]
					};

					let target_block = self.program.get(&target.label).unwrap();
					assert_eq!(target_block.params.len(), target.params.len());
					for (&dst, &src) in target_block.params.iter().zip(target.params.iter()) {
						assert_eq!(dst.ty(), src.ty());

						let val = frame.var_context.get_typed(src).unwrap();
						frame.var_context.insert(dst.into_untyped(), val);
					}

					frame.pc = Pc { block: target.label, instr: 0 };

					None
				}
				super::SsaTerminator::Return(values) => {
					let return_vals = values.iter().map(|v| frame.var_context.get_typed(*v).unwrap()).collect::<Vec<_>>();

					let return_vars = frame.return_vars.clone();

					self.call_stack.incr(&self.program);

					if let Some(return_vars) = return_vars {
						assert!(!self.call_stack.is_empty());
						assert_eq!(return_vars.len(), return_vals.len());

						let frame = self.call_stack.last_mut().unwrap();

						for (var, val) in return_vars.iter().zip(return_vals) {
							assert_eq!(var.ty(), val.ty());
							frame.var_context.insert(var.into_untyped(), val);
						}

						None
					} else {
						assert!(self.call_stack.is_empty());

						Some(return_vals)
					}
				},
			}
		} else {
			let mut incr_pc = true;

			if /*frame.pc.instr == 0*/ true {
				let consts = self.constants.get(frame.pc.block).unwrap_or_else(|| panic!("{:?}", frame.pc));

				//println!("{:?}", &block.body[frame.pc.instr]);

				for (var, value) in frame.var_context.0.iter() {
					let var = TypedSsaVar(var.0, value.ty());

					if let Some(cst) = consts.get(&var) {
						if !state_matches(*cst, *value) {
							println!("const: {:X?}", cst);
							println!("value: {:X?}", value);
							println!("variable: {:?}", var);
							println!("pc: {:?}", frame.pc);
							panic!();
						}
					}
				}

			}

			fn do_compare_op(dst: TypedSsaVar, lhs: SsaVarOrConst, rhs: SsaVarOrConst, var_context: &mut VarContext, f: impl FnOnce(i32, i32) -> bool, g: impl FnOnce(i64, i64) -> bool) {
				let l = lhs.eval(var_context).expect("lhs was uninit");
				let r = rhs.eval(var_context).expect("rhs was uninit");

				let result = match (l, r) {
					(TypedValue::I32(l), TypedValue::I32(r)) => {
						TypedValue::I32(f(l, r) as i32)
					}
					(TypedValue::I64(l), TypedValue::I64(r)) => {
						TypedValue::I32(g(l, r) as i32)
					}
					_ => panic!()
				};

				assert_eq!(dst.ty(), result.ty());

				var_context.insert(dst.into_untyped(), result);
			}

			fn do_binop<L, R>(dst: TypedSsaVar, lhs: L, rhs: R, var_context: &mut VarContext, f: impl FnOnce(i32, i32) -> i32, g: impl FnOnce(i64, i64) -> i64)
				where
					L: Var,
					R: Var,
			{
				let l = lhs.eval(var_context).expect("lhs was uninit");
				let r = rhs.eval(var_context).expect("rhs was uninit");

				let result = match (l, r) {
					(TypedValue::I32(l), TypedValue::I32(r)) => {
						TypedValue::I32(f(l, r))					
					}
					(TypedValue::I64(l), TypedValue::I64(r)) => {
						TypedValue::I64(g(l, r))
					}
					_ => panic!(),
				};

				assert_eq!(dst.ty(), result.ty());

				var_context.insert(dst.into_untyped(), result);
			}

			fn do_unaryop(dst: TypedSsaVar, src: TypedSsaVar, var_context: &mut VarContext, f: impl FnOnce(i32) -> i32, g: impl FnOnce(i64) -> i64) {
				let s = var_context.get_typed(src).expect("src was uninit");

				let result = match s {
					TypedValue::I32(s) => TypedValue::I32(f(s)),
					TypedValue::I64(s) => TypedValue::I64(g(s)),
				};

				assert_eq!(dst.ty(), result.ty());

				var_context.insert(dst.into_untyped(), result);
			}

			fn do_extend_op(dst: TypedSsaVar, src: TypedSsaVar, var_context: &mut VarContext, f: impl FnOnce(i64) -> i32, g: impl FnOnce(i64) -> i64) {
				let s = var_context.get_typed(src).expect("src was uninit");

				let s = match s {
					TypedValue::I32(s) => s as i64,
					TypedValue::I64(s) => s,
				};

				let result = match dst.ty() {
					ValType::I32 => TypedValue::I32(f(s)),
					ValType::I64 => TypedValue::I64(g(s)),
					_ => panic!(),
				};

				var_context.insert(dst.into_untyped(), result);
			}

			fn do_i32_cvtop(dst: TypedSsaVar, src: TypedSsaVar, var_context: &mut VarContext, f: impl FnOnce(i32) -> i32, g: impl FnOnce(i32) -> i64) {
				let s = var_context.get_typed(src).expect("src was uninit");

				let result = match (dst.ty(), s) {
					(ValType::I32, TypedValue::I32(s)) => TypedValue::I32(f(s)),
					(ValType::I64, TypedValue::I32(s)) => TypedValue::I64(g(s)),
					_ => panic!(),
				};

				var_context.insert(dst.into_untyped(), result);
			}

			fn do_store_op(memarg: MemoryImmediate, src: TypedSsaVar, addr: SsaVarOrConst, size: usize, var_context: &mut VarContext, memory: &mut [Memory]) {
				let offset = addr.eval(var_context).unwrap();
				let offset = offset.into_i32().unwrap();
				let addr = memarg.offset as usize + offset as usize;

				let src = var_context.get_typed(src).unwrap();

				let memory = &mut memory[memarg.memory as usize];

				match src {
					TypedValue::I32(s) => memory.store(addr, &s.to_le_bytes()[..size]),
					TypedValue::I64(s) => memory.store(addr, &s.to_le_bytes()[..size]),
				}
			}

			fn do_load_op(memarg: MemoryImmediate, dst: TypedSsaVar, addr: SsaVarOrConst, f: impl FnOnce(i64) -> i32, g: impl FnOnce(i64) -> i64, size: usize, var_context: &mut VarContext, memory: &mut [Memory]) {
				let offset = addr.eval(var_context).unwrap();
				let offset = offset.into_i32().unwrap();
				let addr = memarg.offset as usize + offset as usize;

				let memory = &memory[memarg.memory as usize];

				let mut buf = [0; 8];
				buf[..size].copy_from_slice(memory.load(addr, size));
				let buf = i64::from_le_bytes(buf);

				let result = match dst.ty() {
					ValType::I32 => TypedValue::I32(f(buf)),
					ValType::I64 => TypedValue::I64(g(buf)),
					_ => panic!(),
				};

				var_context.insert(dst.into_untyped(), result);
			}

			match &block.body[frame.pc.instr] {
				&super::SsaInstr::I32Set(dst, val) => {
					assert_eq!(dst.ty(), ValType::I32);
					frame.var_context.insert(dst.into_untyped(), val.into());
				}
				&super::SsaInstr::I64Set(dst, val) => {
					assert_eq!(dst.ty(), ValType::I64);
					frame.var_context.insert(dst.into_untyped(), val.into());
				}
				&super::SsaInstr::Assign(dst, src) => {
					assert_eq!(dst.ty(), src.ty());
					let val = src.eval(&frame.var_context).unwrap();
					frame.var_context.insert(dst.into_untyped(), val);
				}

				&super::SsaInstr::Add(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, i32::wrapping_add, i64::wrapping_add),
				&super::SsaInstr::Sub(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, i32::wrapping_sub, i64::wrapping_sub),
				&super::SsaInstr::Mul(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, i32::wrapping_mul, i64::wrapping_mul),
				// TODO: Test behavior with negative numbers
				&super::SsaInstr::DivS(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, |a, b| a / b, |a, b| a / b),
				&super::SsaInstr::DivU(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, |a, b| (a as u32 / b as u32) as i32, |a, b| (a as u64 / b as u64) as i64),
				&super::SsaInstr::RemS(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, i32::wrapping_rem, i64::wrapping_rem),
				&super::SsaInstr::RemU(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, |a, b| (a as u32 % b as u32) as i32, |a, b| (a as u64 % b as u64) as i64),
				&super::SsaInstr::Shl(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, |a, b| a << b.rem_euclid(32), |a, b| a << b.rem_euclid(64)),
				&super::SsaInstr::ShrS(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, |a, b| a >> b.rem_euclid(32), |a, b| a >> b.rem_euclid(64)),
				&super::SsaInstr::ShrU(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, |a, b| ((a as u32) >> (b as u32).rem_euclid(32)) as i32, |a, b| ((a as u64) >> (b as u64).rem_euclid(64)) as i64),
				&super::SsaInstr::Rotl(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, |a, b| a.rotate_left(b.rem_euclid(32) as u32), |a, b| a.rotate_left(b.rem_euclid(64) as u32)),
				&super::SsaInstr::Rotr(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, |a, b| a.rotate_right(b.rem_euclid(32) as u32), |a, b| a.rotate_right(b.rem_euclid(64) as u32)),
				&super::SsaInstr::Xor(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, |a, b| a ^ b, |a, b| a ^ b),
				&super::SsaInstr::And(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, |a, b| a & b, |a, b| a & b),
				&super::SsaInstr::Or(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut frame.var_context, |a, b| a | b, |a, b| a | b),

				&super::SsaInstr::GtS(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| a > b, |a, b| a > b),
				&super::SsaInstr::GtU(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| (a as u32) > (b as u32), |a, b| (a as u64) > (b as u64)),
				&super::SsaInstr::GeS(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| a >= b, |a, b| a >= b),
				&super::SsaInstr::GeU(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| (a as u32) >= (b as u32), |a, b| (a as u64) >= (b as u64)),
				&super::SsaInstr::LtS(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| a < b, |a, b| a < b),
				&super::SsaInstr::LtU(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| (a as u32) < (b as u32), |a, b| (a as u64) < (b as u64)),
				&super::SsaInstr::LeS(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| a <= b, |a, b| a <= b),
				&super::SsaInstr::LeU(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| (a as u32) <= (b as u32), |a, b| (a as u64) <= (b as u64)),
				&super::SsaInstr::Eq(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| a == b, |a, b| a == b),
				&super::SsaInstr::Ne(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| a != b, |a, b| a != b),

				&super::SsaInstr::Popcnt(dst, src) => do_unaryop(dst, src, &mut frame.var_context, |a| a.count_ones() as i32, |a| a.count_ones() as i64),
				// TODO: Determine behavior when it is entirely zeros
				&super::SsaInstr::Clz(dst, src) => do_unaryop(dst, src, &mut frame.var_context, |a| a.leading_zeros() as i32, |a| a.leading_zeros() as i64),
				&super::SsaInstr::Ctz(dst, src) => do_unaryop(dst, src, &mut frame.var_context, |a| a.trailing_zeros() as i32, |a| a.trailing_zeros() as i64),

				&super::SsaInstr::Eqz(dst, src) => {
					assert_eq!(dst.ty(), ValType::I32);

					match frame.var_context.get_typed(src).unwrap() {
						TypedValue::I32(s) => {
							frame.var_context.insert(dst.into_untyped(), TypedValue::I32((s == 0) as i32));
						}
						TypedValue::I64(s) => {
							frame.var_context.insert(dst.into_untyped(), TypedValue::I32((s == 0) as i32));
						}
					}
				}

				&super::SsaInstr::Load64(memarg, dst, addr) => do_load_op(memarg, dst, addr, |_| panic!(), |a| a, 8, &mut frame.var_context, &mut self.memory),
				&super::SsaInstr::Load32S(memarg, dst, addr) => do_load_op(memarg, dst, addr, |a| a as i32, |a| a as i32 as i64, 4, &mut frame.var_context, &mut self.memory),
				&super::SsaInstr::Load32U(memarg, dst, addr) => do_load_op(memarg, dst, addr, |a| a as u32 as i32, |a| a as u32 as i64, 4, &mut frame.var_context, &mut self.memory),
				&super::SsaInstr::Load16S(memarg, dst, addr) => do_load_op(memarg, dst, addr, |a| a as i16 as i32, |a| a as i16 as i64, 2, &mut frame.var_context, &mut self.memory),
				&super::SsaInstr::Load16U(memarg, dst, addr) => do_load_op(memarg, dst, addr, |a| a as u16 as i32, |a| a as u16 as i64, 2, &mut frame.var_context, &mut self.memory),
				&super::SsaInstr::Load8S(memarg, dst, addr) => do_load_op(memarg, dst, addr, |a| a as i8 as i32, |a| a as i8 as i64, 1, &mut frame.var_context, &mut self.memory),
				&super::SsaInstr::Load8U(memarg, dst, addr) => do_load_op(memarg, dst, addr, |a| a as u8 as i32, |a| a as u8 as i64, 1, &mut frame.var_context, &mut self.memory),

				&super::SsaInstr::Store64(memarg, src, addr) => do_store_op(memarg, src, addr, 8, &mut frame.var_context, &mut self.memory),
				&super::SsaInstr::Store32(memarg, src, addr) => do_store_op(memarg, src, addr, 4, &mut frame.var_context, &mut self.memory),
				&super::SsaInstr::Store16(memarg, src, addr) => do_store_op(memarg, src, addr, 2, &mut frame.var_context, &mut self.memory),
				&super::SsaInstr::Store8(memarg, src, addr) => do_store_op(memarg, src, addr, 1, &mut frame.var_context, &mut self.memory),

				&super::SsaInstr::GlobalSet(dst, src) => {
					let src = frame.var_context.get_typed(src).unwrap();
					let dst = &mut self.globals[dst as usize];
					assert_eq!(dst.ty(), src.ty());
					*dst = src;
				}
				&super::SsaInstr::GlobalGet(dst, src) => {
					let src = self.globals[src as usize];
					assert_eq!(dst.ty(), src.ty());
					frame.var_context.insert(dst.into_untyped(), src);
				}

				&super::SsaInstr::LocalSet(dst, src) => {
					let src = frame.var_context.get_typed(src).unwrap();
					let dst = &mut frame.locals[dst as usize];
					assert_eq!(dst.ty(), src.ty());
					*dst = src;
				}
				&super::SsaInstr::LocalGet(dst, src) => {
					let src = &frame.locals[src as usize];
					assert_eq!(dst.ty(), src.ty());
					frame.var_context.insert(dst.into_untyped(), *src);
				}

				&super::SsaInstr::Extend8S(dst, src) => do_extend_op(dst, src, &mut frame.var_context, |a| a as i8 as i32, |a| a as i8 as i64),
				&super::SsaInstr::Extend16S(dst, src) => do_extend_op(dst, src, &mut frame.var_context, |a| a as i16 as i32, |a| a as i16 as i64),
				&super::SsaInstr::Extend32S(dst, src) => do_extend_op(dst, src, &mut frame.var_context, |_| panic!(), |a| a as i32 as i64),
				&super::SsaInstr::Extend32U(dst, src) => do_extend_op(dst, src, &mut frame.var_context, |_| panic!(), |a| a as u32 as i64),

				&super::SsaInstr::Wrap(dst, src) => {
					let src = frame.var_context.get_typed(src).unwrap();
					if let TypedValue::I64(src) = src {
						assert_eq!(dst.ty(), ValType::I32);
						frame.var_context.insert(dst.into_untyped(), TypedValue::I32(src as i32));
					} else {
						panic!()
					};
				}

				&super::SsaInstr::Select { dst, true_var, false_var, cond } => {
					let true_val = true_var.eval(&frame.var_context).unwrap();
					let false_val = false_var.eval(&frame.var_context).unwrap();
					assert_eq!(true_val.ty(), false_val.ty());

					let cond = frame.var_context.get_typed(cond).unwrap();
					let cond = cond.into_i32().unwrap();

					let val = if cond != 0 {
						true_val
					} else {
						false_val
					};

					assert_eq!(dst.ty(), val.ty());
					frame.var_context.insert(dst.into_untyped(), val);
				}
				&super::SsaInstr::ParamGet(dst, src) => {
					frame.var_context.insert(dst.into_untyped(), frame.locals[src as usize]);
				},
				super::SsaInstr::Call { function_index, params, returns } => {
					incr_pc = false;

					let block = BlockId { func: *function_index as usize, block: 0 };

					let local_tys = self.local_types.get(&(*function_index as usize)).unwrap();

					let mut new_frame = CallFrame::new(block, local_tys, Some(returns.clone()));

					assert!(new_frame.locals.len() >= params.len());

					for (local, param) in new_frame.locals.iter_mut().zip(params.iter()) {
						assert_eq!(local.ty(), param.ty());
						let param = frame.var_context.get(param.into_untyped()).unwrap();
						assert_eq!(local.ty(), param.ty());
						*local = param;
					}

					self.call_stack.incr(&self.program);

					self.call_stack.0.push(new_frame);
				}
				super::SsaInstr::CallIndirect { table_index, table_entry, params, returns } => {
					incr_pc = false;

					let table_entry = frame.var_context.get_typed(*table_entry).unwrap();
					let table_entry = table_entry.into_i32().unwrap() as usize;

					let function_index = self.tables[*table_index as usize].elements[table_entry].unwrap();

					let local_tys = self.local_types.get(&(function_index)).unwrap();

					let block = BlockId { func: function_index, block: 0 };

					let mut new_frame = CallFrame::new(block, local_tys, Some(returns.clone()));

					assert!(new_frame.locals.len() >= params.len());

					for (local, param) in new_frame.locals.iter_mut().zip(params.iter()) {
						assert_eq!(local.ty(), param.ty());
						let param = frame.var_context.get(param.into_untyped()).unwrap();
						assert_eq!(local.ty(), param.ty());
						*local = param;
					}

					self.call_stack.incr(&self.program);

					self.call_stack.0.push(new_frame);

				}

				super::SsaInstr::Memset { dest, value, length, result } => {
					let dest = dest.eval(&frame.var_context).unwrap().into_i32().unwrap();
					let value = value.eval(&frame.var_context).unwrap().into_i32().unwrap();
					let length = length.eval(&frame.var_context).unwrap().into_i32().unwrap();

					if length < 0 {
						panic!();
					}

					for i in 0..length {
						self.memory[0].data[dest as usize + i as usize] = value as u8;
					}

					frame.var_context.insert(result.into_untyped(), dest.into());
				}

				super::SsaInstr::TurtleSetX(_) |
				super::SsaInstr::TurtleSetY(_) |
				super::SsaInstr::TurtleSetZ(_) |
				super::SsaInstr::TurtleSetBlock(_) |
				super::SsaInstr::TurtleFillBlock { .. } |
				super::SsaInstr::TurtleCopyRegion { .. } |
				super::SsaInstr::TurtlePasteRegionMasked { .. } |
				super::SsaInstr::TurtleCopy |
				super::SsaInstr::TurtlePaste => {}

				super::SsaInstr::PrintInt(v) => {
					let value = v.eval(&frame.var_context).unwrap().into_i32().unwrap();
					println!("{:?}", value);
				}
				super::SsaInstr::PutChar(_) => todo!(),

				super::SsaInstr::TurtleGetBlock(dst) => {
					frame.var_context.insert(dst.into_untyped(), 0.into());
				}


				/*super::SsaInstr::TurtleSetX(_) |
				super::SsaInstr::TurtleSetY(_) |
				super::SsaInstr::TurtleSetZ(_) |
				super::SsaInstr::TurtleSetBlock(_) |
				super::SsaInstr::TurtleGetBlock(_) |
				super::SsaInstr::TurtleCopy |
				super::SsaInstr::TurtlePaste |
				super::SsaInstr::PrintInt(_) => panic!("trying to use minecraft IO in SSA interpreter")*/
			}

			if incr_pc {
				self.call_stack.incr(&self.program);
			}

			None
		}
	}

	pub fn is_halted(&self) -> bool {
		self.call_stack.is_empty()
	}
}