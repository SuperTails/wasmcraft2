pub mod interp;

use std::ops::RangeInclusive;

use crate::ssa::BlockId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Half {
	Hi,
	Lo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DoubleRegister {
	Work(u32),
	Return(u32),
	Param(u32),
	Temp(u32),
	CondTaken,
	Const(i32),
}

impl DoubleRegister {
	pub fn lo(self) -> Register {
		Register { double: self, half: Half::Lo }
	}

	pub fn hi(self) -> Register {
		Register { double: self, half: Half::Hi }
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Register {
	double: DoubleRegister,
	half: Half,
}

impl Register {
	pub fn work_lo(id: u32) -> Register {
		DoubleRegister::Work(id).lo()
	}

	pub fn work_hi(id: u32) -> Register {
		DoubleRegister::Work(id).hi()
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

	pub fn cond_taken() -> Register {
		DoubleRegister::CondTaken.lo()
	}

	pub fn const_val(v: i32) -> Register {
		DoubleRegister::Const(v).lo()
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

#[derive(Debug)]
pub enum LirInstr {
	Assign(Register, Register),
	Set(Register, i32),

	Add(Register, Register),
	Sub(Register, Register),
	Mul(Register, Register),
	DivS(Register, Register),
	DivU(Register, Register),
	RemS(Register, Register),
	RemU(Register, Register),

	Add64(DoubleRegister, DoubleRegister, DoubleRegister),
	Sub64(DoubleRegister, DoubleRegister, DoubleRegister),
	Mul64(DoubleRegister, DoubleRegister, DoubleRegister),

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
	Load(Register, Register),

	/// arg, old width (assumes high bits are zero)
	SignExtend(Register, u32),

	Select { dst: Register, true_reg: Register, false_reg: Register, cond: Register },

	Call { func: u32 },
	CallIndirect { table_index: u32, table_entry: Register },

	IfCond { cond: Condition, instr: Box<LirInstr> }
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
	JumpIf { true_label: BlockId, false_label: BlockId },
	JumpTable { arms: Vec<BlockId>, default: BlockId },
	Return,
}

pub struct LirBasicBlock {
	pub body: Vec<LirInstr>,
	pub term: LirTerminator,
}

pub struct LirFunction(pub Vec<(BlockId, LirBasicBlock)>);

pub struct LirProgram {
	pub code: Vec<LirFunction>,
}