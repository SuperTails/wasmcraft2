pub mod interp;
pub mod lir_emitter;
pub mod liveness;
pub mod call_graph;

use std::collections::HashMap;

use wasmparser::{Type, MemoryImmediate};

use self::interp::TypedValue;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BlockId {
	pub func: usize,
	pub block: usize
}

pub struct SsaBasicBlock {
	pub params: Vec<TypedSsaVar>,
	pub body: Vec<SsaInstr>,
	pub term: SsaTerminator,
}

impl Default for SsaBasicBlock {
	fn default() -> Self {
		SsaBasicBlock {
			params: Vec::new(), body: Default::default(), term: SsaTerminator::Unreachable,
		}
	}
}

#[derive(Default)]
pub struct SsaVarAlloc(u32);

impl SsaVarAlloc {
	pub fn new() -> Self {
		Default::default()
	}

	fn next_id(&mut self) -> u32 {
		let id = self.0;
		self.0 += 1;
		id
	}

	pub fn new_typed(&mut self, ty: Type) -> TypedSsaVar {
		TypedSsaVar(self.next_id(), ty)
	}

	pub fn new_i32(&mut self) -> TypedSsaVar {
		self.new_typed(Type::I32)
	}

	pub fn new_i64(&mut self) -> TypedSsaVar {
		self.new_typed(Type::I64)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SsaVar(u32);

impl SsaVar {
	pub fn into_typed(self, ty: Type) -> TypedSsaVar {
		TypedSsaVar(self.0, ty)
	}
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypedSsaVar(u32, Type);

impl TypedSsaVar {
	pub fn ty(self) -> Type {
		self.1
	}

	pub fn into_untyped(self) -> SsaVar {
		SsaVar(self.0)
	}

	pub fn unwrap_i32(self) -> SsaVar {
		assert_eq!(self.1, Type::I32);
		SsaVar(self.0)
	}

	pub fn unwrap_i64(self) -> SsaVar {
		assert_eq!(self.1, Type::I64);
		SsaVar(self.0)
	}
}

#[derive(Debug)]
pub enum SsaInstr {
	I32Set(TypedSsaVar, i32),
	I64Set(TypedSsaVar, i64),

	// binop instructions: dst, lhs, rhs

	Add(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Sub(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Mul(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	DivS(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	DivU(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	RemS(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	RemU(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Shl(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	ShrS(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	ShrU(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Rotl(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Rotr(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Xor(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	And(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Or(TypedSsaVar, TypedSsaVar, TypedSsaVar),

	// comp instructions: dst, lhs, rhs

	GtS(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	GtU(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	GeS(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	GeU(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	LtS(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	LtU(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	LeS(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	LeU(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Eq(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Ne(TypedSsaVar, TypedSsaVar, TypedSsaVar),

	// unary instructions: dst, src

	Popcnt(TypedSsaVar, TypedSsaVar),
	Clz(TypedSsaVar, TypedSsaVar),
	Ctz(TypedSsaVar, TypedSsaVar),

	// test instructions: dst, src

	Eqz(TypedSsaVar, TypedSsaVar),

	// memory instructions
	// loads: dst, addr
	// stores: src, addr

	Load64(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load32S(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load32U(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load16S(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load16U(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load8S(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load8U(MemoryImmediate, TypedSsaVar, TypedSsaVar),

	Store64(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Store32(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Store16(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Store8(MemoryImmediate, TypedSsaVar, TypedSsaVar),

	// variable instructions

	GlobalSet(u32, TypedSsaVar),
	GlobalGet(TypedSsaVar, u32),

	LocalSet(u32, TypedSsaVar),
	LocalGet(TypedSsaVar, u32),

	// conversion ops: dst, src

	Extend8S(TypedSsaVar, TypedSsaVar),
	//Extend8U(TypedSsaVar, TypedSsaVar),
	Extend16S(TypedSsaVar, TypedSsaVar),
	//Extend16U(TypedSsaVar, TypedSsaVar),
	Extend32S(TypedSsaVar, TypedSsaVar),
	Extend32U(TypedSsaVar, TypedSsaVar),

	Wrap(TypedSsaVar, TypedSsaVar),

	// misc instructions

	Select {
		dst: TypedSsaVar,
		true_var: TypedSsaVar,
		false_var: TypedSsaVar,
		cond: TypedSsaVar,
	},

	Call {
		function_index: u32,
		params: Vec<TypedSsaVar>,
		returns: Vec<TypedSsaVar>,
	},
	CallIndirect {
		table_index: u32,
		table_entry: TypedSsaVar,
		params: Vec<TypedSsaVar>,
		returns: Vec<TypedSsaVar>,
	}
}

impl SsaInstr {
	pub fn uses(&self) -> Vec<TypedSsaVar> {
		match self {
			SsaInstr::I32Set(_, _) => vec![],
			SsaInstr::I64Set(_, _) => vec![],

			SsaInstr::Add(_, lhs, rhs) |
			SsaInstr::Sub(_, lhs, rhs) |
			SsaInstr::Mul(_, lhs, rhs) |
			SsaInstr::DivS(_, lhs, rhs) |
			SsaInstr::DivU(_, lhs, rhs) |
			SsaInstr::RemS(_, lhs, rhs) |
			SsaInstr::RemU(_, lhs, rhs) |
			SsaInstr::Shl(_, lhs, rhs) |
			SsaInstr::ShrS(_, lhs, rhs) |
			SsaInstr::ShrU(_, lhs, rhs) |
			SsaInstr::Rotl(_, lhs, rhs) |
			SsaInstr::Rotr(_, lhs, rhs) |
			SsaInstr::Xor(_, lhs, rhs) |
			SsaInstr::And(_, lhs, rhs) |
			SsaInstr::Or(_, lhs, rhs) |
			SsaInstr::GtS(_, lhs, rhs) |
			SsaInstr::GtU(_, lhs, rhs) |
			SsaInstr::GeS(_, lhs, rhs) |
			SsaInstr::GeU(_, lhs, rhs) |
			SsaInstr::LtS(_, lhs, rhs) |
			SsaInstr::LtU(_, lhs, rhs) |
			SsaInstr::LeS(_, lhs, rhs) |
			SsaInstr::LeU(_, lhs, rhs) |
			SsaInstr::Eq(_, lhs, rhs) |
			SsaInstr::Ne(_, lhs, rhs) => vec![*lhs, *rhs],

			SsaInstr::Popcnt(_, src) |
			SsaInstr::Clz(_, src) |
			SsaInstr::Ctz(_, src) => vec![*src],

			SsaInstr::Eqz(_, src) => vec![*src],

			SsaInstr::Load64(_, _, addr) | 
			SsaInstr::Load32S(_, _, addr) |
			SsaInstr::Load32U(_, _, addr) |
			SsaInstr::Load16S(_, _, addr) |
			SsaInstr::Load16U(_, _, addr) |
			SsaInstr::Load8S(_, _, addr) |
			SsaInstr::Load8U(_, _, addr) => vec![*addr],

			SsaInstr::Store64(_, src, addr) |
			SsaInstr::Store32(_, src, addr) | 
			SsaInstr::Store16(_, src, addr) |
			SsaInstr::Store8(_, src, addr) => vec![*src, *addr],

			SsaInstr::GlobalSet(_, src) => vec![*src],
			SsaInstr::GlobalGet(_, _) => vec![],

			SsaInstr::LocalSet(_, src) => vec![*src],
			SsaInstr::LocalGet(_, _) => vec![], 

			SsaInstr::Extend8S(_, src) |
			SsaInstr::Extend16S(_, src) |
			SsaInstr::Extend32S(_, src) |
			SsaInstr::Extend32U(_, src) |
			SsaInstr::Wrap(_, src) => vec![*src],

			SsaInstr::Select { dst: _, true_var, false_var, cond } => vec![*true_var, *false_var, *cond],
			SsaInstr::Call { function_index: _, params, returns: _ } => params.clone(),
			SsaInstr::CallIndirect { table_index: _, table_entry, params, returns: _ } => {
				params.iter().copied().chain(Some(*table_entry)).collect()
			}
		}
	}

	pub fn defs(&self) -> Vec<TypedSsaVar> {
		match self {
			SsaInstr::I32Set(dst, _) => vec![*dst],
			SsaInstr::I64Set(dst, _) => vec![*dst],

			SsaInstr::Add(dst, _, _) |
			SsaInstr::Sub(dst, _, _) |
			SsaInstr::Mul(dst, _, _) |
			SsaInstr::DivS(dst, _, _) |
			SsaInstr::DivU(dst, _, _) |
			SsaInstr::RemS(dst, _, _) |
			SsaInstr::RemU(dst, _, _) |
			SsaInstr::Shl(dst, _, _) |
			SsaInstr::ShrS(dst, _, _) |
			SsaInstr::ShrU(dst, _, _) |
			SsaInstr::Rotl(dst, _, _) |
			SsaInstr::Rotr(dst, _, _) |
			SsaInstr::Xor(dst, _, _) |
			SsaInstr::And(dst, _, _) |
			SsaInstr::Or(dst, _, _) |
			SsaInstr::GtS(dst, _, _) |
			SsaInstr::GtU(dst, _, _) |
			SsaInstr::GeS(dst, _, _) |
			SsaInstr::GeU(dst, _, _) |
			SsaInstr::LtS(dst, _, _) |
			SsaInstr::LtU(dst, _, _) |
			SsaInstr::LeS(dst, _, _) |
			SsaInstr::LeU(dst, _, _) |
			SsaInstr::Eq(dst, _, _) |
			SsaInstr::Ne(dst, _, _) => vec![*dst],

			SsaInstr::Popcnt(dst, _) |
			SsaInstr::Clz(dst, _) |
			SsaInstr::Ctz(dst, _) => vec![*dst],

			SsaInstr::Eqz(dst, _) => vec![*dst],

			SsaInstr::Load64(_, dst, _) |
			SsaInstr::Load32S(_, dst, _) |
			SsaInstr::Load32U(_, dst, _) |
			SsaInstr::Load16S(_, dst, _) |
			SsaInstr::Load16U(_, dst, _) |
			SsaInstr::Load8S(_, dst, _) |
			SsaInstr::Load8U(_, dst, _) => vec![*dst],

			SsaInstr::Store64(_, _, _) |
			SsaInstr::Store32(_, _, _) |
			SsaInstr::Store16(_, _, _) |
			SsaInstr::Store8(_, _, _) => vec![],

			SsaInstr::GlobalSet(_, _) => vec![],
			SsaInstr::GlobalGet(dst, _) => vec![*dst],

			SsaInstr::LocalSet(_, _) => vec![],
			SsaInstr::LocalGet(dst, _) => vec![*dst], 

			SsaInstr::Extend8S(dst, _) |
			SsaInstr::Extend16S(dst, _) |
			SsaInstr::Extend32S(dst, _) |
			SsaInstr::Extend32U(dst, _) |
			SsaInstr::Wrap(dst, _) => vec![*dst],

			SsaInstr::Select { dst, true_var: _, false_var: _, cond: _ } => vec![*dst],
			SsaInstr::Call { function_index: _, params: _, returns } => returns.clone(),
			SsaInstr::CallIndirect { returns, .. } => returns.clone(),
		}
	}
}

#[derive(Debug)]
pub struct JumpTarget {
	pub label: BlockId,
	pub params: Vec<TypedSsaVar>,
}

#[derive(Debug)]
pub enum SsaTerminator {
	Unreachable,
	Jump(JumpTarget),
	BranchIf { cond: TypedSsaVar, true_target: JumpTarget, false_target: JumpTarget },
	BranchTable { cond: TypedSsaVar, default: JumpTarget, arms: Vec<JumpTarget> },
	Return(Vec<TypedSsaVar>),
}

impl SsaTerminator {
	pub fn uses(&self) -> Vec<TypedSsaVar> {
		match self {
			SsaTerminator::Unreachable => vec![],
			SsaTerminator::Jump(target) => {
				target.params.clone()
			}
			SsaTerminator::BranchIf { cond, true_target, false_target } => {
				let mut result = vec![*cond];
				result.extend(true_target.params.iter());
				result.extend(false_target.params.iter());
				result
			}
			SsaTerminator::BranchTable { cond, default, arms } => {
				let mut result = vec![*cond];
				result.extend(default.params.iter());
				for arm in arms.iter() {
					result.extend(arm.params.iter());
				}
				result
			}
			SsaTerminator::Return(vars) => vars.clone(),
		}
	}
}

pub struct SsaFunction {
	pub code: Vec<(BlockId, SsaBasicBlock)>,
	pub params: Box<[Type]>,
	pub returns: Box<[Type]>,
}

impl SsaFunction {
	pub fn iter<'a>(&'a self) -> impl Iterator<Item=(BlockId, &'a SsaBasicBlock)> + 'a {
		self.code.iter().map(|(i, b)| (*i, b))
	}

	pub fn get(&self, block_id: BlockId) -> &SsaBasicBlock {
		&self.code.iter().find(|(id, _)| *id == block_id).unwrap().1
	}

	pub fn func_id(&self) -> u32 {
		self.code[0].0.func as u32
	}
}

pub struct SsaProgram {
	pub local_types: HashMap<usize, Vec<Type>>,
	pub globals: Vec<TypedValue>,
	pub memory: Vec<Memory>,
	pub tables: Vec<Table>,
	pub code: Vec<SsaFunction>,
}

pub struct Memory {
	pub data: Vec<u8>,
	pub maximum: Option<usize>,
}

impl Memory {
	pub fn new(initial: usize, maximum: Option<usize>) -> Memory {
		Memory {
			data: vec![0; 65536 * initial],
			maximum,
		}
	}

	pub fn store(&mut self, addr: usize, bytes: &[u8]) {
		let dest = &mut self.data[addr..][..bytes.len()];
		dest.copy_from_slice(bytes);
	}

	pub fn load(&self, addr: usize, len: usize) -> &[u8] {
		&self.data[addr..][..len]
	}
}

#[derive(Debug)]
pub struct Table {
	pub max: Option<usize>,
	pub elements: Vec<Option<usize>>,
}