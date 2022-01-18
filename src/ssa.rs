use wasmparser::{Type, MemoryImmediate};

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


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypedSsaVar(u32, Type);

impl TypedSsaVar {
	pub fn ty(self) -> Type {
		self.1
	}
}

#[derive(Debug)]
pub enum SsaInstr {
	I32Set(TypedSsaVar, i32),

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

	// test instructions: dst, src

	Eqz(TypedSsaVar, TypedSsaVar),

	// memory instructions
	// loads: dst, addr
	// stores: src, addr

	Load(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load32S(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load32U(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load16S(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load16U(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load8S(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Load8U(MemoryImmediate, TypedSsaVar, TypedSsaVar),

	Store(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Store32(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Store16(MemoryImmediate, TypedSsaVar, TypedSsaVar),
	Store8(MemoryImmediate, TypedSsaVar, TypedSsaVar),

	// variable instructions

	LocalSet(u32, TypedSsaVar),
	LocalGet(TypedSsaVar, u32),

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
	Return,
}

