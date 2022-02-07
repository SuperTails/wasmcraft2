use std::collections::HashMap;

use wasmparser::Type;

use crate::ssa::{interp::TypedValue, BlockId, Memory};

use super::{Register, LirBasicBlock, DoubleRegister};

pub struct Pc {
	block: BlockId,
	instr: usize,
}

pub struct CallStack(Vec<Pc>);

pub struct LocalStack(Vec<Vec<TypedValue>>);

pub struct RegContext(HashMap<Register, i32>);

impl RegContext {
	pub fn set(&mut self, reg: Register, val: i32) {
		self.0.insert(reg, val);
	}

	pub fn set_64(&mut self, reg: DoubleRegister, val: i64) {
		self.set(reg.lo(), val as i32);
		self.set(reg.hi(), (val >> 32) as i32);
	}

	pub fn get(&self, reg: Register) -> i32 {
		*self.0.get(&reg).unwrap()
	}

	pub fn get_i64(&self, reg: DoubleRegister) -> i64 {
		let lo = self.get(reg.lo());
		let hi = self.get(reg.hi());

		((hi as i64) << 32) | (lo as u32 as i64)
	}
}

pub struct LirInterpreter {
	local_types: HashMap<usize, Vec<Type>>,
	globals: Vec<TypedValue>,
	memory: Vec<Memory>,
	program: HashMap<BlockId, LirBasicBlock>,
}

