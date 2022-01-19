use std::collections::HashMap;

use wasmparser::Type;

use crate::ssa::TypedSsaVar;

use super::{BlockId, SsaBasicBlock, SsaVar};

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
	pub fn new(block: BlockId, local_tys: &[Type], return_vars: Option<Vec<TypedSsaVar>>) -> Self {
		// TODO: Are locals zero-initialized???
		let locals = local_tys.iter().map(|ty| match ty {
			Type::I32 => TypedValue::I32(0),
			Type::I64 => TypedValue::I64(0),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedValue {
	I32(i32),
	I64(i64),
}

impl TypedValue {
	pub fn ty(&self) -> Type {
		match self {
			Self::I32(_) => Type::I32,
			Self::I64(_) => Type::I64,
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

pub struct SsaInterpreter {
	local_types: HashMap<usize, Vec<Type>>,
	program: HashMap<BlockId, SsaBasicBlock>,
	call_stack: CallStack,
}

impl SsaInterpreter {
	pub fn new(local_types: HashMap<usize, Vec<Type>>, program: HashMap<BlockId, SsaBasicBlock>) -> Self {
		Self {
			local_types,
			program,
			call_stack: CallStack(Vec::new()),
		}
	}

	pub fn call(&mut self, func: usize, params: Vec<TypedValue>) {
		assert!(self.call_stack.is_empty());

		let locals = self.local_types.get(&func).unwrap();

		let block = BlockId { func, block: 0 };

		let mut frame = CallFrame::new(block, locals, None);
		frame.locals = params;
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
		println!("{:?}", self.call_stack);

		let frame = self.call_stack.last_mut().expect("stepped while halted");

		let block = self.program.get(&frame.pc.block).unwrap();

		if frame.pc.instr == block.body.len() {
			match &block.term {
				super::SsaTerminator::Unreachable => unreachable!(),
				super::SsaTerminator::Jump(jump) => {
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
				super::SsaTerminator::BranchIf { cond, true_target, false_target } => todo!(),
				super::SsaTerminator::BranchTable { cond, default, arms } => todo!(),
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

			fn do_compare_op(dst: TypedSsaVar, lhs: TypedSsaVar, rhs: TypedSsaVar, var_context: &mut VarContext, f: impl FnOnce(i32, i32) -> bool, g: impl FnOnce(i64, i64) -> bool) {
				let l = var_context.get_typed(lhs).expect("lhs was uninit");
				let r = var_context.get_typed(rhs).expect("rhs was uninit");

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

			fn do_binop(dst: TypedSsaVar, lhs: TypedSsaVar, rhs: TypedSsaVar, var_context: &mut VarContext, f: impl FnOnce(i32, i32) -> i32, g: impl FnOnce(i64, i64) -> i64) {
				let l = var_context.get_typed(lhs).expect("lhs was uninit");
				let r = var_context.get_typed(rhs).expect("rhs was uninit");

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

			fn do_i32_cvtop(dst: TypedSsaVar, src: TypedSsaVar, var_context: &mut VarContext, f: impl FnOnce(i32) -> i32, g: impl FnOnce(i32) -> i64) {
				let s = var_context.get_typed(src).expect("src was uninit");

				let result = match (dst.ty(), s) {
					(Type::I32, TypedValue::I32(s)) => TypedValue::I32(f(s)),
					(Type::I64, TypedValue::I32(s)) => TypedValue::I64(g(s)),
					_ => panic!(),
				};

				var_context.insert(dst.into_untyped(), result);
			}

			match &block.body[frame.pc.instr] {
				&super::SsaInstr::I32Set(dst, val) => {
					assert_eq!(dst.ty(), Type::I32);
					frame.var_context.insert(dst.into_untyped(), val.into());
				}
				&super::SsaInstr::I64Set(dst, val) => {
					assert_eq!(dst.ty(), Type::I64);
					frame.var_context.insert(dst.into_untyped(), val.into());
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
				&super::SsaInstr::GeU(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| (a as u32) >= (b as u32), |a, b| (a as u32) >= (b as u32)),
				&super::SsaInstr::LtS(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| a < b, |a, b| a < b),
				&super::SsaInstr::LtU(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| (a as u32) < (b as u32), |a, b| (a as u32) < (b as u32)),
				&super::SsaInstr::LeS(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| a <= b, |a, b| a <= b),
				&super::SsaInstr::LeU(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| (a as u32) <= (b as u32), |a, b| (a as u64) <= (b as u64)),
				&super::SsaInstr::Eq(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| a == b, |a, b| a == b),
				&super::SsaInstr::Ne(dst, lhs, rhs) => do_compare_op(dst, lhs, rhs, &mut frame.var_context, |a, b| a != b, |a, b| a != b),

				&super::SsaInstr::Popcnt(dst, src) => do_unaryop(dst, src, &mut frame.var_context, |a| a.count_ones() as i32, |a| a.count_ones() as i64),
				// TODO: Determine behavior when it is entirely zeros
				&super::SsaInstr::Clz(dst, src) => do_unaryop(dst, src, &mut frame.var_context, |a| a.leading_zeros() as i32, |a| a.leading_zeros() as i64),
				&super::SsaInstr::Ctz(dst, src) => do_unaryop(dst, src, &mut frame.var_context, |a| a.trailing_zeros() as i32, |a| a.trailing_zeros() as i64),

				&super::SsaInstr::Eqz(dst, src) => do_unaryop(dst, src, &mut frame.var_context, |a| (a == 0) as i32, |a| (a == 0) as i64),

				super::SsaInstr::Load(_, _, _) => todo!(),
				super::SsaInstr::Load32S(_, _, _) => todo!(),
				super::SsaInstr::Load32U(_, _, _) => todo!(),
				super::SsaInstr::Load16S(_, _, _) => todo!(),
				super::SsaInstr::Load16U(_, _, _) => todo!(),
				super::SsaInstr::Load8S(_, _, _) => todo!(),
				super::SsaInstr::Load8U(_, _, _) => todo!(),
				super::SsaInstr::Store(_, _, _) => todo!(),
				super::SsaInstr::Store32(_, _, _) => todo!(),
				super::SsaInstr::Store16(_, _, _) => todo!(),
				super::SsaInstr::Store8(_, _, _) => todo!(),

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

				&super::SsaInstr::Extend8S(dst, src) => do_i32_cvtop(dst, src, &mut frame.var_context, |a| a as i8 as i32, |a| a as i8 as i64),
				&super::SsaInstr::Extend16S(dst, src) => do_i32_cvtop(dst, src, &mut frame.var_context, |a| a as i16 as i32, |a| a as i16 as i64),
				&super::SsaInstr::Extend32S(dst, src) => do_i32_cvtop(dst, src, &mut frame.var_context, |_| panic!(), |a| a as i64),

				super::SsaInstr::Select { dst, true_var, false_var, cond } => todo!(),
				super::SsaInstr::Call { function_index, params, returns } => todo!(),
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