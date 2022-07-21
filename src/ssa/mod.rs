pub mod interp;
pub mod lir_emitter;
pub mod liveness;
pub mod call_graph;
pub mod const_prop;
pub mod dce;
pub mod reg_alloc;

use std::{collections::{HashMap, HashSet}, fmt};

use wasmparser::{Type, MemoryImmediate, ValType};

use self::interp::TypedValue;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BlockId {
	pub func: usize,
	pub block: usize
}

#[derive(Clone)]
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

	pub fn new_typed(&mut self, ty: ValType) -> TypedSsaVar {
		TypedSsaVar(self.next_id(), ty)
	}

	pub fn new_i32(&mut self) -> TypedSsaVar {
		self.new_typed(ValType::I32)
	}

	pub fn new_i64(&mut self) -> TypedSsaVar {
		self.new_typed(ValType::I64)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SsaVar(u32);

impl SsaVar {
	pub fn into_typed(self, ty: ValType) -> TypedSsaVar {
		TypedSsaVar(self.0, ty)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SsaVarOrConst {
	Var(TypedSsaVar),
	Const(TypedValue),
}

impl SsaVarOrConst {
	pub fn ty(&self) -> ValType {
		match self {
			SsaVarOrConst::Var(v) => v.ty(),
			SsaVarOrConst::Const(c) => c.ty(),
		}
	}

	pub fn get_var(self) -> Option<TypedSsaVar> {
		if let Self::Var(v) = self {
			Some(v)
		} else {
			None
		}
	}

	pub fn get_var_mut(&mut self) -> Option<&mut TypedSsaVar> {
		if let Self::Var(v) = self {
			Some(v)
		} else {
			None
		}
	}
}

impl From<TypedSsaVar> for SsaVarOrConst {
	fn from(v: TypedSsaVar) -> Self {
		Self::Var(v)
	}
}

impl From<TypedValue> for SsaVarOrConst {
	fn from(c: TypedValue) -> Self {
		Self::Const(c)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypedSsaVar(u32, ValType);

impl TypedSsaVar {
	pub fn ty(self) -> ValType {
		self.1
	}

	pub fn into_untyped(self) -> SsaVar {
		SsaVar(self.0)
	}

	pub fn unwrap_i32(self) -> SsaVar {
		assert_eq!(self.1, ValType::I32);
		SsaVar(self.0)
	}

	pub fn unwrap_i64(self) -> SsaVar {
		assert_eq!(self.1, ValType::I64);
		SsaVar(self.0)
	}
}

fn get_ty_index(ty: ValType) -> u32 {
	match ty {
		ValType::I32 => 0,
		ValType::I64 => 1,
		ValType::F32 => 2,
		ValType::F64 => 3,
		ValType::V128 => 4,
		ValType::FuncRef => 5,
		ValType::ExternRef => 6,
	}
}

impl PartialOrd for TypedSsaVar {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		(self.0, get_ty_index(self.1)).partial_cmp(&(other.0, get_ty_index(self.1)))
    }
}

impl Ord for TypedSsaVar {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.partial_cmp(other).unwrap()
	}
}

#[derive(Debug, Clone)]
pub enum SsaInstr {
	I32Set(TypedSsaVar, i32),
	I64Set(TypedSsaVar, i64),

	Assign(TypedSsaVar, SsaVarOrConst),

	// binop instructions: dst, lhs, rhs

	Add(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	Sub(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	Mul(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	DivS(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	DivU(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	RemS(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	RemU(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	Shl(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	ShrS(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	ShrU(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	Rotl(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Rotr(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Xor(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	And(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	Or(TypedSsaVar, TypedSsaVar, SsaVarOrConst),

	// comp instructions: dst, lhs, rhs

	GtS(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	GtU(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	GeS(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	GeU(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	LtS(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	LtU(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	LeS(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	LeU(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	Eq(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	Ne(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),

	// unary instructions: dst, src

	Popcnt(TypedSsaVar, TypedSsaVar),
	Clz(TypedSsaVar, TypedSsaVar),
	Ctz(TypedSsaVar, TypedSsaVar),

	// test instructions: dst, src

	Eqz(TypedSsaVar, TypedSsaVar),

	// memory instructions
	// loads: dst, addr
	// stores: src, addr

	Load64(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load32S(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load32U(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load16S(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load16U(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load8S(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load8U(MemoryImmediate, TypedSsaVar, SsaVarOrConst),

	Store64(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Store32(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Store16(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Store8(MemoryImmediate, TypedSsaVar, SsaVarOrConst),

	// variable instructions

	GlobalSet(u32, TypedSsaVar),
	GlobalGet(TypedSsaVar, u32),

	LocalSet(u32, TypedSsaVar),
	LocalGet(TypedSsaVar, u32),

	ParamGet(TypedSsaVar, u32),

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
		true_var: SsaVarOrConst,
		false_var: SsaVarOrConst,
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
	},

	// Optimized C stdlib calls

	Memset { dest: TypedSsaVar, value: TypedSsaVar, length: TypedSsaVar, result: TypedSsaVar },

	// Minecraft IO instructions

	TurtleSetX(SsaVarOrConst),
	TurtleSetY(SsaVarOrConst),
	TurtleSetZ(SsaVarOrConst),
	TurtleSetBlock(TypedSsaVar),
	TurtleGetBlock(TypedSsaVar),
	TurtleCopy,
	TurtlePaste,
	PrintInt(TypedSsaVar),
}

impl SsaInstr {
	pub fn uses(&self) -> Vec<TypedSsaVar> {
		match self {
			SsaInstr::I32Set(_, _) => vec![],
			SsaInstr::I64Set(_, _) => vec![],

			SsaInstr::Assign(_, SsaVarOrConst::Var(src)) => vec![*src],
			SsaInstr::Assign(_, SsaVarOrConst::Const(_)) => vec![],

			SsaInstr::Add(_, lhs, rhs) |
			SsaInstr::Sub(_, lhs, rhs) => lhs.get_var().into_iter().chain(rhs.get_var()).collect(),

			SsaInstr::Mul(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::DivS(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::DivU(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::RemS(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::RemU(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::Shl(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::ShrS(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::ShrU(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::Rotl(_, lhs, rhs) |
			SsaInstr::Rotr(_, lhs, rhs) |
			SsaInstr::And(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::Xor(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::Or(_, lhs, SsaVarOrConst::Var(rhs)) => vec![*lhs, *rhs],

			SsaInstr::GtS(_, lhs, rhs) |
			SsaInstr::GtU(_, lhs, rhs) |
			SsaInstr::GeS(_, lhs, rhs) |
			SsaInstr::GeU(_, lhs, rhs) |
			SsaInstr::LtS(_, lhs, rhs) |
			SsaInstr::LtU(_, lhs, rhs) |
			SsaInstr::LeS(_, lhs, rhs) |
			SsaInstr::LeU(_, lhs, rhs) |
			SsaInstr::Eq(_, lhs, rhs) |
			SsaInstr::Ne(_, lhs, rhs) => lhs.get_var().into_iter().chain(rhs.get_var()).collect(),

			SsaInstr::Mul(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::And(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::Xor(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::Or(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::Shl(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::ShrS(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::ShrU(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::RemS(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::RemU(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::DivS(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::DivU(_, lhs, SsaVarOrConst::Const(_)) => vec![*lhs],


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
			SsaInstr::Load8U(_, _, addr) => addr.get_var().into_iter().collect(),

			SsaInstr::Store64(_, src, addr) |
			SsaInstr::Store32(_, src, addr) | 
			SsaInstr::Store16(_, src, addr) |
			SsaInstr::Store8(_, src, addr) => Some(*src).into_iter().chain(addr.get_var()).collect(),

			SsaInstr::GlobalSet(_, src) => vec![*src],
			SsaInstr::GlobalGet(_, _) => vec![],

			SsaInstr::LocalSet(_, src) => vec![*src],
			SsaInstr::LocalGet(_, _) => vec![], 

			SsaInstr::ParamGet(_, _) => vec![],

			SsaInstr::Extend8S(_, src) |
			SsaInstr::Extend16S(_, src) |
			SsaInstr::Extend32S(_, src) |
			SsaInstr::Extend32U(_, src) |
			SsaInstr::Wrap(_, src) => vec![*src],

			SsaInstr::Select { dst: _, true_var, false_var, cond } => {
				true_var.get_var().into_iter().chain(false_var.get_var()).chain(vec![*cond]).collect()
			}
			SsaInstr::Call { function_index: _, params, returns: _ } => params.clone(),
			SsaInstr::CallIndirect { table_index: _, table_entry, params, returns: _ } => {
				params.iter().copied().chain(Some(*table_entry)).collect()
			},

			SsaInstr::Memset { dest, value, length, result: _, } => {
				vec![*dest, *value, *length]
			}

			SsaInstr::TurtleSetX(SsaVarOrConst::Var(v)) |
			SsaInstr::TurtleSetY(SsaVarOrConst::Var(v)) |
			SsaInstr::TurtleSetZ(SsaVarOrConst::Var(v)) => vec![*v],

			SsaInstr::TurtleSetX(SsaVarOrConst::Const(_)) |
			SsaInstr::TurtleSetY(SsaVarOrConst::Const(_)) |
			SsaInstr::TurtleSetZ(SsaVarOrConst::Const(_)) => Vec::new(),

			SsaInstr::TurtleSetBlock(b) => vec![*b],
			SsaInstr::TurtleGetBlock(_) => Vec::new(),
			SsaInstr::TurtleCopy => Vec::new(),
			SsaInstr::TurtlePaste => Vec::new(),
			SsaInstr::PrintInt(i) => vec![*i],
		}
	}

	pub fn defs(&self) -> Vec<TypedSsaVar> {
		match self {
			SsaInstr::I32Set(dst, _) => vec![*dst],
			SsaInstr::I64Set(dst, _) => vec![*dst],

			SsaInstr::Assign(dst, _) => vec![*dst],

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

			SsaInstr::ParamGet(dst, _) => vec![*dst],

			SsaInstr::Extend8S(dst, _) |
			SsaInstr::Extend16S(dst, _) |
			SsaInstr::Extend32S(dst, _) |
			SsaInstr::Extend32U(dst, _) |
			SsaInstr::Wrap(dst, _) => vec![*dst],

			SsaInstr::Select { dst, true_var: _, false_var: _, cond: _ } => vec![*dst],
			SsaInstr::Call { function_index: _, params: _, returns } => returns.clone(),
			SsaInstr::CallIndirect { returns, .. } => returns.clone(),

			SsaInstr::Memset { dest: _, value: _, length: _, result, } => {
				vec![*result]
			}

			SsaInstr::TurtleSetX(_) => Vec::new(),
			SsaInstr::TurtleSetY(_) => Vec::new(),
			SsaInstr::TurtleSetZ(_) => Vec::new(),
			SsaInstr::TurtleSetBlock(_) => Vec::new(),
			SsaInstr::TurtleGetBlock(b) => vec![*b],
			SsaInstr::TurtleCopy => Vec::new(),
			SsaInstr::TurtlePaste => Vec::new(),
			SsaInstr::PrintInt(_) => Vec::new(),
		}
	}

	pub fn has_side_effects(&self) -> bool {
		match self {
			SsaInstr::I32Set(_, _) |
			SsaInstr::I64Set(_, _) |
			SsaInstr::Assign(_, _) |
			SsaInstr::Add(_, _, _) |
			SsaInstr::Sub(_, _, _) |
			SsaInstr::Mul(_, _, _) |
			SsaInstr::DivS(_, _, _) |
			SsaInstr::DivU(_, _, _) |
			SsaInstr::RemS(_, _, _) |
			SsaInstr::RemU(_, _, _) |
			SsaInstr::Shl(_, _, _) |
			SsaInstr::ShrS(_, _, _) |
			SsaInstr::ShrU(_, _, _) |
			SsaInstr::Rotl(_, _, _) |
			SsaInstr::Rotr(_, _, _) |
			SsaInstr::Xor(_, _, _) |
			SsaInstr::And(_, _, _) |
			SsaInstr::Or(_, _, _) |
			SsaInstr::GtS(_, _, _) |
			SsaInstr::GtU(_, _, _) |
			SsaInstr::GeS(_, _, _) |
			SsaInstr::GeU(_, _, _) |
			SsaInstr::LtS(_, _, _) |
			SsaInstr::LtU(_, _, _) |
			SsaInstr::LeS(_, _, _) |
			SsaInstr::LeU(_, _, _) |
			SsaInstr::Eq(_, _, _) |
			SsaInstr::Ne(_, _, _) |
			SsaInstr::Popcnt(_, _) |
			SsaInstr::Clz(_, _) |
			SsaInstr::Ctz(_, _) |
			SsaInstr::Eqz(_, _) => false,

			SsaInstr::Load64(_, _, _) |
			SsaInstr::Load32S(_, _, _) |
			SsaInstr::Load32U(_, _, _) |
			SsaInstr::Load16S(_, _, _) |
			SsaInstr::Load16U(_, _, _) |
			SsaInstr::Load8S(_, _, _) |
			SsaInstr::Load8U(_, _, _) |
			SsaInstr::Store64(_, _, _) |
			SsaInstr::Store32(_, _, _) |
			SsaInstr::Store16(_, _, _) |
			SsaInstr::Store8(_, _, _) |
			SsaInstr::GlobalSet(_, _) |
			SsaInstr::GlobalGet(_, _) |
			SsaInstr::LocalSet(_, _) |
			SsaInstr::LocalGet(_, _) |
			SsaInstr::ParamGet(_, _) => true,

			SsaInstr::Extend8S(_, _) |
			SsaInstr::Extend16S(_, _) |
			SsaInstr::Extend32S(_, _) |
			SsaInstr::Extend32U(_, _) |
			SsaInstr::Wrap(_, _) |
			SsaInstr::Select { .. } => false,

			SsaInstr::Call { .. } |
			SsaInstr::CallIndirect { .. } |
			SsaInstr::Memset { .. } |
			SsaInstr::TurtleSetX(_) |
			SsaInstr::TurtleSetY(_) |
			SsaInstr::TurtleSetZ(_) |
			SsaInstr::TurtleSetBlock(_) |
			SsaInstr::TurtleGetBlock(_) |
			SsaInstr::TurtleCopy |
			SsaInstr::TurtlePaste |
			SsaInstr::PrintInt(_) => true,
		}

	}

	pub fn constable_vars(&mut self) -> Vec<&mut SsaVarOrConst> {
		match self {
			SsaInstr::Add(_, l, r) => vec![l, r],
			SsaInstr::Sub(_, l, r) => vec![l, r],

			SsaInstr::Assign(_, r) => vec![r],

			SsaInstr::Shl(_, _, r) |
			SsaInstr::ShrS(_, _, r) |
			SsaInstr::ShrU(_, _, r) |
			SsaInstr::And(_, _, r) |
			SsaInstr::Xor(_, _, r) |
			SsaInstr::Or(_, _, r) |
			SsaInstr::Mul(_, _, r) | 
			SsaInstr::RemS(_, _, r) | 
			SsaInstr::RemU(_, _, r) | 
			SsaInstr::DivS(_, _, r) | 
			SsaInstr::DivU(_, _, r) => vec![r],

			SsaInstr::GtS(_, lhs, rhs) |
			SsaInstr::GtU(_, lhs, rhs) |
			SsaInstr::GeS(_, lhs, rhs) |
			SsaInstr::GeU(_, lhs, rhs) |
			SsaInstr::LtS(_, lhs, rhs) |
			SsaInstr::LtU(_, lhs, rhs) |
			SsaInstr::LeS(_, lhs, rhs) |
			SsaInstr::LeU(_, lhs, rhs) |
			SsaInstr::Eq(_, lhs, rhs) |
			SsaInstr::Ne(_, lhs, rhs) => vec![lhs, rhs],

			SsaInstr::Load64(_, _, addr) | 
			SsaInstr::Load32S(_, _, addr) |
			SsaInstr::Load32U(_, _, addr) |
			SsaInstr::Load16S(_, _, addr) |
			SsaInstr::Load16U(_, _, addr) |
			SsaInstr::Load8S(_, _, addr) |
			SsaInstr::Load8U(_, _, addr) => vec![addr],

			SsaInstr::Store64(_, _, addr) |
			SsaInstr::Store32(_, _, addr) |
			SsaInstr::Store16(_, _, addr) |
			SsaInstr::Store8(_, _, addr) => vec![addr],

			SsaInstr::Select { dst: _, true_var, false_var, cond: _ } => vec![true_var, false_var],

			SsaInstr::TurtleSetX(v) |
			SsaInstr::TurtleSetY(v) |
			SsaInstr::TurtleSetZ(v) => vec![v],

			_ => Vec::new(),
		}
	}

	pub fn coalescable_vars(&self) -> Vec<(&TypedSsaVar, &TypedSsaVar)> {
		match self {
			SsaInstr::Add(dst, SsaVarOrConst::Var(lhs), SsaVarOrConst::Var(rhs)) if dst.ty() == ValType::I32 => vec![(dst, lhs), (dst, rhs)],
			SsaInstr::Add(dst, SsaVarOrConst::Var(lhs), _) if dst.ty() == ValType::I32 => vec![(dst, lhs)],
			SsaInstr::Add(dst, _, SsaVarOrConst::Var(rhs)) if dst.ty() == ValType::I32 => vec![(dst, rhs)],

			SsaInstr::Assign(dst, SsaVarOrConst::Var(src)) => vec![(dst, src)],

			SsaInstr::Mul(dst, lhs, _rhs) if dst.ty() == ValType::I32 => vec![(dst, lhs), /*(dst, rhs)*/],
			SsaInstr::ShrS(dst, lhs, _) if dst.ty() == ValType::I32 => vec![(dst, lhs)],
			SsaInstr::ShrU(dst, lhs, _) if dst.ty() == ValType::I32 => vec![(dst, lhs)],
			SsaInstr::Shl(dst, lhs, _) if dst.ty() == ValType::I32 => vec![(dst, lhs)],
			SsaInstr::Xor(dst, lhs, _) => vec![(dst, lhs)],

			SsaInstr::And(dst, lhs, SsaVarOrConst::Const(c))
				if dst.ty() == ValType::I32 && is_simple_and_mask(c.into_i32().unwrap()) => vec![(dst, lhs)],

			_ => Vec::new(), 
		}
	}
}

fn is_simple_and_mask(i: i32) -> bool {
	i == 0 || i.leading_zeros() + i.trailing_ones() == 32
}

#[derive(Debug, Clone)]
pub struct JumpTarget {
	pub label: BlockId,
	pub params: Vec<TypedSsaVar>,
}

#[derive(Debug, Clone)]
pub enum SsaTerminator {
	Unreachable,
	ScheduleJump(JumpTarget, u32),
	Jump(JumpTarget),
	BranchIf { cond: TypedSsaVar, true_target: JumpTarget, false_target: JumpTarget },
	BranchTable { cond: TypedSsaVar, default: JumpTarget, arms: Vec<JumpTarget> },
	Return(Vec<TypedSsaVar>),
}

impl SsaTerminator {
	/// Returns a set of the variables that are used as parameters when jumping to the given successor block.
	pub fn params_on_edge(&self, succ_id: BlockId) -> HashSet<TypedSsaVar> {
		match self {
			SsaTerminator::Unreachable | SsaTerminator::Return(_) => panic!("cannot jump to block {succ_id:?}"),
			SsaTerminator::ScheduleJump(target, _) |
			SsaTerminator::Jump(target) => {
				assert_eq!(target.label, succ_id);
				target.params.iter().copied().collect()
			}
			SsaTerminator::BranchIf { cond: _, true_target, false_target } => {
				let mut result = HashSet::new();
				let mut is_ok = false;
				if true_target.label == succ_id {
					result.extend(true_target.params.iter().copied());
					is_ok = true;
				}
				if false_target.label == succ_id {
					result.extend(false_target.params.iter().copied());
					is_ok = true;
				}
				assert!(is_ok, "cannot jump to block {succ_id:?}");
				result
			}
			SsaTerminator::BranchTable { cond: _, default, arms } => {
				let mut result = HashSet::new();
				let mut is_ok = false;
				if default.label == succ_id {
					result.extend(default.params.iter().copied());
					is_ok = true;
				}
				for arm in arms.iter() {
					if arm.label == succ_id {
						result.extend(arm.params.iter().copied());
						is_ok = true;
					}
				}
				assert!(is_ok, "cannot jump to block {succ_id:?}");
				result
			}
		}
	}

	pub fn uses(&self) -> Vec<TypedSsaVar> {
		match self {
			SsaTerminator::Unreachable => vec![],
			SsaTerminator::ScheduleJump(target, _) => {
				target.params.clone()
			}
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

	pub fn defs(&self, parent: &SsaFunction) -> Vec<TypedSsaVar> {
		let mut result = Vec::new();

		for succ in self.successors() {
			let block = parent.get(succ);
			result.extend(block.params.iter().copied());
		}

		result

	}

	pub fn successors(&self) -> Vec<BlockId> {
		match self {
			SsaTerminator::Unreachable => Vec::new(),
			SsaTerminator::Jump(t) => vec![t.label],
			SsaTerminator::ScheduleJump(t, _) => vec![t.label],
			SsaTerminator::BranchIf { cond: _, true_target, false_target } => {
				vec![true_target.label, false_target.label]
			}
			SsaTerminator::BranchTable { cond: _, default, arms } => {
				let mut result = vec![default.label];
				result.extend(arms.iter().map(|t| t.label));
				result
			}
			SsaTerminator::Return(_) => Vec::new(),
		}
	}
}

pub struct SsaFunction {
	pub func_id: usize,
	/// A sparse array, where each basic block's ID is represented as its index in the list.
	pub code: Vec<Option<SsaBasicBlock>>,
	pub params: Box<[ValType]>,
	pub returns: Box<[ValType]>,
}

impl SsaFunction {
	pub fn new<C>(blocks: C, params: Box<[ValType]>, returns: Box<[ValType]>) -> Self
		where C: IntoIterator<Item=(BlockId, SsaBasicBlock)>
	{
		let mut code = Vec::new();

		let mut func_id = None;

		for (block_id, block) in blocks.into_iter() {
			if let Some(func_id) = func_id {
				assert_eq!(func_id, block_id.func);
			} else {
				func_id = Some(block_id.func);
			}

			if block_id.block >= code.len() {
				code.resize_with(block_id.block + 1, || None);
			}

			assert!(code[block_id.block].is_none());
			code[block_id.block] = Some(block);
		}

		let func_id = func_id.unwrap();

		SsaFunction { func_id, code, params, returns }
	}

	pub fn iter<'a>(&'a self) -> impl Iterator<Item=(BlockId, &'a SsaBasicBlock)> + 'a {
		self.code.iter().enumerate().filter_map(|(idx, block)| {
			let block_id = BlockId { func: self.func_id, block: idx };
			block.as_ref().map(|block| (block_id, block))
		})
	}

	pub fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item=(BlockId, &'a mut SsaBasicBlock)> + 'a {
		self.code.iter_mut().enumerate().filter_map(|(idx, block)| {
			let block_id = BlockId { func: self.func_id, block: idx };
			block.as_mut().map(|block| (block_id, block))
		})
	}

	pub fn get(&self, block_id: BlockId) -> &SsaBasicBlock {
		assert_eq!(block_id.func, self.func_id);
		self.code[block_id.block].as_ref().unwrap()
	}

	pub fn get_mut(&mut self, block_id: BlockId) -> &mut SsaBasicBlock {
		assert_eq!(block_id.func, self.func_id);
		self.code[block_id.block].as_mut().unwrap()
	}

	pub fn func_id(&self) -> u32 {
		self.func_id as u32
	}

	pub fn entry_point_id(&self) -> BlockId {
		let func = self.func_id() as usize;
		BlockId { func, block: 0 }
	}

	pub fn coalescable_term_vars(&self, source_id: BlockId) -> Vec<(TypedSsaVar, TypedSsaVar)> {
		let mut result = Vec::new();
		let source = self.get(source_id);
		match &source.term {
			SsaTerminator::Unreachable => {},
			SsaTerminator::ScheduleJump(t, _) => { assert!(t.params.is_empty()); },
			SsaTerminator::Jump(t) => {
				let target_params = &self.get(t.label).params;
				result.extend(target_params.iter().copied().zip(t.params.iter().copied()));
			},
			SsaTerminator::BranchIf { cond: _, true_target, false_target } => {
				let t_params = &self.get(true_target.label).params;
				let f_params = &self.get(false_target.label).params;
				result.extend(t_params.iter().copied().zip(true_target.params.iter().copied()));
				result.extend(f_params.iter().copied().zip(false_target.params.iter().copied()));
			},
			SsaTerminator::BranchTable { cond: _, default, arms } => {
				let d_params = &self.get(default.label).params;
				result.extend(d_params.iter().copied().zip(default.params.iter().copied()));

				for arm in arms.iter() {
					let a_params = &self.get(arm.label).params;
					result.extend(a_params.iter().copied().zip(arm.params.iter().copied()));
				}
			},
			SsaTerminator::Return(_) => {},
		}
		result
	}
}

pub struct SsaProgram {
	pub local_types: HashMap<usize, Vec<ValType>>,
	pub globals: Vec<TypedValue>,
	pub memory: Vec<Memory>,
	pub tables: Vec<Table>,
	pub code: Vec<SsaFunction>,
	pub exports: HashMap<String, BlockId>,
}

impl SsaProgram {
	pub fn get_func(&self, func_id: u32) -> &SsaFunction {
		self.code.iter().find(|f| f.func_id() == func_id).unwrap()
	}
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