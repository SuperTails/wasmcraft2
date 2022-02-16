pub mod interp;

use std::{ops::RangeInclusive, fmt, collections::HashSet};

use wasmparser::Type;

use crate::ssa::{BlockId, Memory, interp::TypedValue, Table};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Half {
	Hi,
	Lo,
}

impl fmt::Display for Half {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Half::Lo => write!(f, "lo"),
			Half::Hi => write!(f, "hi"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DoubleRegister {
	/// function id, register id
	Work(u32, u32),
	Temp(u32),
	Return(u32),
	Param(u32),
	Const(i32),
	Global(u32),
	CondTaken,
}

impl DoubleRegister {
	pub fn lo(self) -> Register {
		Register { double: self, half: Half::Lo }
	}

	pub fn hi(self) -> Register {
		Register { double: self, half: Half::Hi }
	}

	pub fn split_lo_hi(self) -> (Register, Register) {
		(self.lo(), self.hi())
	}

	pub fn return_reg(id: u32) -> DoubleRegister {
		DoubleRegister::Return(id)
	}

	pub fn temp(id: u32) -> DoubleRegister {
		DoubleRegister::Temp(id)
	}

	pub fn param(id: u32) -> DoubleRegister {
		DoubleRegister::Param(id)
	}
	
	pub fn global(id: u32) -> DoubleRegister {
		DoubleRegister::Global(id)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Register {
	double: DoubleRegister,
	half: Half,
}

impl Register {
	pub fn work_lo(func: u32, id: u32) -> Register {
		DoubleRegister::Work(func, id).lo()
	}

	pub fn work_hi(func: u32, id: u32) -> Register {
		DoubleRegister::Work(func, id).hi()
	}

	pub fn return_lo(id: u32) -> Register {
		DoubleRegister::return_reg(id).lo()
	}

	pub fn return_hi(id: u32) -> Register {
		DoubleRegister::return_reg(id).hi()
	}

	pub fn temp_lo(id: u32) -> Register {
		DoubleRegister::temp(id).lo()
	}

	pub fn temp_hi(id: u32) -> Register {
		DoubleRegister::temp(id).hi()
	}

	pub fn param_lo(id: u32) -> Register {
		DoubleRegister::param(id).lo()
	}

	pub fn param_hi(id: u32) -> Register {
		DoubleRegister::param(id).hi()
	}

	pub fn global_lo(id: u32) -> Register {
		DoubleRegister::global(id).lo()
	}

	pub fn global_hi(id: u32) -> Register {
		DoubleRegister::global(id).hi()
	}

	pub fn cond_taken() -> Register {
		DoubleRegister::CondTaken.lo()
	}

	pub fn const_val(v: i32) -> Register {
		DoubleRegister::Const(v).lo()
	}
}

const OBJECTIVE_NAME: &str = "reg";

impl fmt::Display for Register {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let half = self.half;
		match self.double {
			DoubleRegister::Work(func, reg) => write!(f, "%work%{func}%{reg}%{half}")?,
			DoubleRegister::Temp(reg) => write!(f, "%temp%{reg}%{half}")?,
			DoubleRegister::Return(reg) => write!(f, "%return%{reg}%{half}")?,
			DoubleRegister::Param(reg) => write!(f, "%param%{reg}%{half}")?,
			DoubleRegister::Global(reg) => write!(f, "%global%{reg}%{half}")?,
			DoubleRegister::Const(val) => write!(f, "%const%{val}")?,
			DoubleRegister::CondTaken => write!(f, "%condtaken")?,
		}

		write!(f, " {OBJECTIVE_NAME}")
	}
}

/// by default:
/// 
/// work%0%lo

#[derive(Clone, Debug)]
pub enum Condition {
	Matches(Register, RangeInclusive<i32>),
	NotMatches(Register, RangeInclusive<i32>),
}

impl Condition {
	pub fn eq_zero(reg: Register) -> Self {
		Condition::Matches(reg, 0..=0)
	}

	pub fn eq_const(reg: Register, val: i32) -> Self {
		Condition::Matches(reg, val..=val)
	}

	pub fn neq_zero(reg: Register) -> Self {
		Condition::NotMatches(reg, 0..=0)
	}
}

#[derive(Debug, Clone)]
pub enum LirInstr {
	Assign(Register, Register),
	Set(Register, i32),

	Add(Register, Register),
	Sub(Register, Register),
	Mul(Register, Register),
	DivS(Register, Register, Register),
	DivU(Register, Register, Register),
	RemS(Register, Register, Register),
	RemU(Register, Register, Register),

	MulTo64(DoubleRegister, Register, Register),

	Add64(DoubleRegister, DoubleRegister, DoubleRegister),
	Sub64(DoubleRegister, DoubleRegister, DoubleRegister),
	DivS64(DoubleRegister, DoubleRegister, DoubleRegister),
	DivU64(DoubleRegister, DoubleRegister, DoubleRegister),
	RemS64(DoubleRegister, DoubleRegister, DoubleRegister),
	RemU64(DoubleRegister, DoubleRegister, DoubleRegister),

	Shl(Register, Register, Register),
	ShrS(Register, Register, Register),
	ShrU(Register, Register, Register),
	Rotl(Register, Register, Register),
	Rotr(Register, Register, Register),

	Shl64(DoubleRegister, DoubleRegister, DoubleRegister),
	ShrS64(DoubleRegister, DoubleRegister, DoubleRegister),
	ShrU64(DoubleRegister, DoubleRegister, DoubleRegister),
	Rotl64(DoubleRegister, DoubleRegister, DoubleRegister),
	Rotr64(DoubleRegister, DoubleRegister, DoubleRegister),

	Xor(Register, Register, Register),
	And(Register, Register, Register),
	Or(Register, Register, Register),

	PopcntAdd(Register, Register),
	Ctz(Register, Register),
	Clz(Register, Register),

	Ctz64(DoubleRegister, DoubleRegister),
	Clz64(DoubleRegister, DoubleRegister),

	Eqz(Register, Register),
	Eqz64(Register, DoubleRegister),

	GtS(Register, Register, Register),
	GtU(Register, Register, Register),
	GeS(Register, Register, Register),
	GeU(Register, Register, Register),
	LtS(Register, Register, Register),
	LtU(Register, Register, Register),
	LeS(Register, Register, Register),
	LeU(Register, Register, Register),
	Eq(Register, Register, Register),
	Ne(Register, Register, Register),

	GtS64(Register, DoubleRegister, DoubleRegister),
	GtU64(Register, DoubleRegister, DoubleRegister),
	GeS64(Register, DoubleRegister, DoubleRegister),
	GeU64(Register, DoubleRegister, DoubleRegister),
	LtS64(Register, DoubleRegister, DoubleRegister),
	LtU64(Register, DoubleRegister, DoubleRegister),
	LeS64(Register, DoubleRegister, DoubleRegister),
	LeU64(Register, DoubleRegister, DoubleRegister),
	Eq64(Register, DoubleRegister, DoubleRegister),
	Ne64(Register, DoubleRegister, DoubleRegister),


	/// arg, bits
	Trunc(Register, u32),

	SignExtend8(Register),
	SignExtend16(Register),
	SignExtend32(DoubleRegister),

	LocalSet(u32, Half, Register),
	LocalGet(Register, u32, Half),

	GlobalSet(u32, Half, Register),
	GlobalGet(Register, u32, Half),

	// src, addr
	Store32(Register, Register),
	Store16(Register, Register),
	Store8(Register, Register),

	// dst, addr
	Load32(Register, Register),
	Load16(Register, Register),
	Load8(Register, Register),

	/// arg, old width (assumes high bits are zero)
	SignExtend(Register, u32),

	Select { dst: Register, true_reg: Register, false_reg: Register, cond: Register },

	Call { func: u32 },
	CallIndirect { table_index: u32, table_entry: Register },

	Push(Vec<Register>),
	Pop(Vec<Register>),

	IfCond { cond: Condition, instr: Box<LirInstr> },

	PushLocalFrame(Vec<Type>),
	PopLocalFrame(Vec<Type>),

	TurtleSetX(Register),
	TurtleSetY(Register),
	TurtleSetZ(Register),
	TurtleSetBlock(Register),
}

impl LirInstr {
	pub fn if_cond(self, cond: Condition) -> LirInstr {
		LirInstr::IfCond { cond, instr: Box::new(self) }
	}

	pub fn unless_cond_taken(self) -> LirInstr {
		self.if_cond(Condition::eq_zero(Register::cond_taken()))
	}
}

#[derive(Debug)]
pub enum LirTerminator {
	Jump(BlockId),
	JumpIf { true_label: BlockId, false_label: BlockId, cond: Register },
	JumpTable { arms: Vec<BlockId>, default: BlockId, cond: Register },
	Return,
}

pub struct LirBasicBlock {
	pub body: Vec<LirInstr>,
	pub term: LirTerminator,
}

pub struct LirFunction {
	pub code: Vec<(BlockId, LirBasicBlock)>,
	pub returns: Box<[Type]>,
}

impl LirFunction {
	pub fn func_id(&self) -> usize {
		self.code[0].0.func
	}
}

pub struct LirProgram {
	pub globals: Vec<TypedValue>,
	pub memory: Vec<Memory>,
	pub tables: Vec<Table>,
	pub code: Vec<LirFunction>,
	pub constants: HashSet<i32>,
}