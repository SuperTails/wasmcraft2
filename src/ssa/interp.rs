use std::collections::HashMap;

use wasmparser::Type;

use crate::ssa::TypedSsaVar;

use super::{BlockId, SsaBasicBlock, SsaVar};

pub struct Pc {
	block: BlockId,
	instr: usize,
}

pub struct CallStack(Vec<Pc>);

impl CallStack {
	pub fn last(&self) -> Option<&Pc> {
		self.0.last()
	}

	pub fn last_mut(&mut self) -> Option<&mut Pc> {
		self.0.last_mut()
	}

	pub fn is_empty(&self) -> bool {
		self.0.is_empty()
	}
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Default)]
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
	program: HashMap<BlockId, SsaBasicBlock>,
	call_stack: CallStack,
	var_context: VarContext,
}

impl SsaInterpreter {
	pub fn new(program: HashMap<BlockId, SsaBasicBlock>) -> Self {
		Self {
			program,
			call_stack: CallStack(Vec::new()),
			var_context: VarContext::default(),
		}
	}

	pub fn step(&mut self) {
		let pc = self.call_stack.last_mut().expect("stepped while halted");

		let block = self.program.get(&pc.block).unwrap();

		let mut incr_pc = true;
		
		if pc.instr == block.body.len() {
			match &block.term {
				super::SsaTerminator::Unreachable => unreachable!(),
				super::SsaTerminator::Jump(jump) => todo!(),
				super::SsaTerminator::BranchIf { cond, true_target, false_target } => todo!(),
				super::SsaTerminator::BranchTable { cond, default, arms } => todo!(),
				super::SsaTerminator::Return => todo!(),
			}
		} else {
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

			match &block.body[pc.instr] {
				&super::SsaInstr::I32Set(dst, val) => {
					assert_eq!(dst.ty(), Type::I32);
					self.var_context.insert(dst.into_untyped(), val.into());
				}
				&super::SsaInstr::I64Set(dst, val) => {
					assert_eq!(dst.ty(), Type::I64);
					self.var_context.insert(dst.into_untyped(), val.into());
				}

				&super::SsaInstr::Add(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| a + b, |a, b| a + b),
				&super::SsaInstr::Sub(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| a - b, |a, b| a - b),
				&super::SsaInstr::Mul(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| a * b, |a, b| a * b),
				// TODO: Test behavior with negative numbers
				&super::SsaInstr::DivS(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b|  a / b, |a, b| a / b),
				&super::SsaInstr::DivU(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| (a as u32 / b as u32) as i32, |a, b| (a as u64 / b as u64) as i64),
				&super::SsaInstr::RemS(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| a % b, |a, b | a % b),
				&super::SsaInstr::RemU(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| (a as u32 / b as u32) as i32, |a, b| (a as u64 / b as u64) as i64),
				// TODO: Test behavior when out of range
				&super::SsaInstr::Shl(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| a << b, |a, b| a << b),
				&super::SsaInstr::ShrS(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| a >> b, |a, b| a >> b),
				&super::SsaInstr::ShrU(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| ((a as u32) >> (b as u32)) as i32, |a, b| ((a as u64) >> (b as u64)) as i64),
				&super::SsaInstr::Xor(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| a ^ b, |a, b| a ^ b),
				&super::SsaInstr::And(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| a & b, |a, b| a & b),
				&super::SsaInstr::Or(dst, lhs, rhs) => do_binop(dst, lhs, rhs, &mut self.var_context, |a, b| a | b, |a, b| a | b),

				&super::SsaInstr::GtS(dst, lhs, rhs) => todo!(),
				&super::SsaInstr::GtU(dst, lhs, rhs) => todo!(),
				&super::SsaInstr::GeS(dst, lhs, rhs) => todo!(),
				&super::SsaInstr::GeU(dst, lhs, rhs) => todo!(),
				&super::SsaInstr::LtS(dst, lhs, rhs) => todo!(),
				super::SsaInstr::LtU(_, _, _) => todo!(),
				super::SsaInstr::LeS(_, _, _) => todo!(),
				super::SsaInstr::LeU(_, _, _) => todo!(),
				super::SsaInstr::Eq(_, _, _) => todo!(),
				super::SsaInstr::Ne(_, _, _) => todo!(),

				super::SsaInstr::Eqz(_, _) => todo!(),

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
				super::SsaInstr::LocalSet(_, _) => todo!(),
				super::SsaInstr::LocalGet(_, _) => todo!(),
				super::SsaInstr::Select { dst, true_var, false_var, cond } => todo!(),
				super::SsaInstr::Call { function_index, params, returns } => todo!(),
			}
		}
	}

	pub fn is_halted(&self) -> bool {
		self.call_stack.is_empty()
	}
}