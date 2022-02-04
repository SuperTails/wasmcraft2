use std::collections::HashMap;

use wasmparser::Type;

use crate::ssa::{interp::TypedValue, BlockId, Memory};

use super::{Register, LirBasicBlock};

pub struct Pc {
	block: BlockId,
	instr: usize,
}

pub struct CallStack(Vec<Pc>);

pub struct LocalStack(Vec<Vec<TypedValue>>);

pub struct RegContext(HashMap<Register, i32>);

pub struct LirInterpreter {
	local_types: HashMap<usize, Vec<Type>>,
	globals: Vec<TypedValue>,
	memory: Vec<Memory>,
	program: HashMap<BlockId, LirBasicBlock>,
}

