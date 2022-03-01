use std::collections::HashSet;

use wasmparser::{Type, MemoryImmediate};

use crate::{lir::{Register, LirInstr, DoubleRegister, LirBasicBlock, LirProgram, LirFunction, LirTerminator, Condition, Half}, ssa::{TypedSsaVar, SsaVarOrConst}, jump_mode, JumpMode};

use super::{SsaProgram, SsaFunction, SsaBasicBlock, BlockId, reg_alloc::*, liveness::{LivenessInfo, SimpleLivenessInfo}, call_graph::CallGraph};


struct LirFuncBuilder {
	used_ids: HashSet<BlockId>,
	body: Vec<(BlockId, LirBasicBlock)>
}

impl LirFuncBuilder {
	pub fn new(ssa_func: &SsaFunction) -> Self {
		let used_ids = ssa_func.iter().map(|(id, _)| id).collect();

		LirFuncBuilder {
			used_ids,
			body: Vec::new(),
		}
	}

	pub fn push(&mut self, block_id: BlockId, body: Vec<LirInstr>, term: LirTerminator) {
		assert!(!self.body.iter().any(|(id, _)| *id == block_id));

		let block = LirBasicBlock { body, term };

		self.body.push((block_id, block));
	}

	pub fn func(&self) -> usize {
		let func = self.used_ids.iter().next().unwrap().func;

		assert!(self.used_ids.iter().all(|id| id.func == func));

		func
	}

	pub fn alloc_block_id(&mut self) -> BlockId {
		let mut i = 0;
		loop {
			if self.used_ids.iter().all(|id| id.block != i) {
				let block_id = BlockId { func: self.func(), block: i };
				self.used_ids.insert(block_id);
				return block_id;
			}

			i += 1;
		}
	}
}

fn lower_block<L>(parent: &SsaProgram, parent_func: &SsaFunction, mut block_id: BlockId, ssa_block: &SsaBasicBlock, ra: &mut FullRegAlloc, li: &L, call_graph: &CallGraph, builder: &mut LirFuncBuilder)
	where L: LivenessInfo
{
	fn do_binop<'a, F, G, L, R>(dst: TypedSsaVar, lhs: L, rhs: R, block: &'a mut Vec<LirInstr>, ra: &mut FullRegAlloc, f: F, g: G)
		where
			F: FnOnce(Register, Register, Register, &'a mut Vec<LirInstr>),
			G: FnOnce(DoubleRegister, DoubleRegister, DoubleRegister, &'a mut Vec<LirInstr>),
			L: Into<SsaVarOrConst>,
			R: Into<SsaVarOrConst>,
	{
		let lhs = lhs.into();
		let rhs = rhs.into();

		assert_eq!(dst.ty(), lhs.ty());
		assert_eq!(lhs.ty(), rhs.ty());

		match dst.ty() {
			Type::I32 => f(ra.get(dst.into_untyped()), map_ra_i32(lhs, ra), map_ra_i32(rhs, ra), block),
			Type::I64 => g(ra.get_double(dst.into_untyped()), map_ra_i64(lhs, ra), map_ra_i64(rhs, ra), block),
			t => todo!("{:?}", t),
		}
	}

	fn do_shiftop<F, G, R>(dst: TypedSsaVar, lhs: TypedSsaVar, rhs: R, block: &mut Vec<LirInstr>, ra: &mut FullRegAlloc, f: F, g: G)
		where
			F: FnOnce(Register, Register, Register) -> LirInstr,
			G: FnOnce(DoubleRegister, DoubleRegister, DoubleRegister) -> LirInstr,
			R: Into<SsaVarOrConst>,
	{
		let rhs = rhs.into();

		assert_eq!(dst.ty(), lhs.ty());
		assert_eq!(lhs.ty(), rhs.ty());

		match dst.ty() {
			Type::I32 => block.push(f(ra.get(dst.into_untyped()), ra.get(lhs.into_untyped()), map_ra_i32(rhs, ra))),
			Type::I64 => block.push(g(ra.get_double(dst.into_untyped()), ra.get_double(lhs.into_untyped()), map_ra_i64(rhs, ra))),
			t => todo!("{:?}", t),
		}
	}

	fn do_bitwiseop<F, R>(dst: TypedSsaVar, lhs: TypedSsaVar, rhs: R, block: &mut Vec<LirInstr>, ra: &mut FullRegAlloc, f: F)
		where
			F: Fn(Register, Register, Register) -> LirInstr,
			R: Into<SsaVarOrConst>,
	{
		let rhs = rhs.into();

		assert_eq!(dst.ty(), lhs.ty());
		assert_eq!(lhs.ty(), rhs.ty());

		match dst.ty() {
			Type::I32 => block.push(f(ra.get(dst.into_untyped()), ra.get(lhs.into_untyped()), map_ra_i32(rhs, ra))),
			Type::I64 => {
				let dst = ra.get_double(dst.into_untyped());
				let lhs = ra.get_double(lhs.into_untyped());
				let rhs = map_ra_i64(rhs, ra);
				block.push(f(dst.lo(), lhs.lo(), rhs.lo()));
				block.push(f(dst.hi(), lhs.hi(), rhs.hi()));
			}
			_ => todo!(),
		}
	}

	fn map_ra_i32(r: SsaVarOrConst, ra: &mut FullRegAlloc) -> Register {
		match r {
			SsaVarOrConst::Var(v) => ra.get(v.unwrap_i32()),
			SsaVarOrConst::Const(c) => {
				ra.get_const(c.into_i32().unwrap())
			}
			_ => panic!()
		}
	}

	fn map_ra_i64(r: SsaVarOrConst, ra: &mut FullRegAlloc) -> DoubleRegister {
		match r {
			SsaVarOrConst::Var(v) => ra.get_double(v.unwrap_i64()),
			SsaVarOrConst::Const(c) => {
				let c = c.into_i64().unwrap();
				ra.const_pool.insert(c as i32);
				ra.const_pool.insert((c >> 32) as i32);
				DoubleRegister::const_val(c)
			}
		}
	}

	fn do_compareop<F, G>(dst: TypedSsaVar, lhs: TypedSsaVar, rhs: TypedSsaVar, block: &mut Vec<LirInstr>, ra: &mut FullRegAlloc, f: F, g: G)
		where
			F: FnOnce(Register, Register, Register) -> LirInstr,
			G: FnOnce(Register, DoubleRegister, DoubleRegister) -> LirInstr,
	{
		assert_eq!(dst.ty(), Type::I32);
		let dst = ra.get(dst.into_untyped());

		assert_eq!(lhs.ty(), rhs.ty());
		match lhs.ty() {
			Type::I32 => {
				let lhs = ra.get(lhs.into_untyped());
				let rhs = ra.get(rhs.into_untyped());
				block.push(f(dst, lhs, rhs))
			}
			Type::I64 => {
				let lhs = ra.get_double(lhs.into_untyped());
				let rhs = ra.get_double(rhs.into_untyped());
				block.push(g(dst, lhs, rhs));
			}
			_ => todo!(),
		}
	}

	fn do_unaryop<'a, F, G>(dst: TypedSsaVar, src: TypedSsaVar, block: &'a mut Vec<LirInstr>, ra: &mut FullRegAlloc, f: F, g: G)
		where
			F: FnOnce(Register, Register) -> LirInstr,
			G: FnOnce(DoubleRegister, DoubleRegister) -> LirInstr,
	{
		assert_eq!(dst.ty(), src.ty());

		match dst.ty() {
			Type::I32 => block.push(f(ra.get(dst.into_untyped()), ra.get(src.into_untyped()))),
			Type::I64 => block.push(g(ra.get_double(dst.into_untyped()), ra.get_double(src.into_untyped()))),
			t => todo!("{:?}", t)
		}
	}

	fn do_store<F>(mem: &MemoryImmediate, src: TypedSsaVar, addr: SsaVarOrConst, block: &mut Vec<LirInstr>, ra: &mut FullRegAlloc, f: F)
		where
			F: FnOnce(Register, Register) -> LirInstr
	{
		assert_eq!(mem.memory, 0);

		assert_eq!(src.ty(), Type::I32);
		let src = ra.get(src.into_untyped());

		let addr = map_ra_i32(addr, ra);

		if let Some(c) = addr.get_const() {
			let addr = c + mem.offset as i32;
			block.push(f(src, ra.get_const(addr)));
		} else {
			// TODO: Coalescing?
			let temp = Register::temp_lo(0);

			block.push(LirInstr::Assign(temp, addr));
			block.push(LirInstr::Add(temp, ra.get_const(mem.offset as i32)));
			block.push(f(src, temp));
		}
	}

	fn do_load_trunc(mem: &MemoryImmediate, dst: TypedSsaVar, addr: SsaVarOrConst, bits: u32, signed: bool, block: &mut Vec<LirInstr>, ra: &mut FullRegAlloc)
	{
		assert_eq!(mem.memory, 0);

		let dst_lo = match dst.ty() {
			Type::I32 => ra.get(dst.into_untyped()), 
			Type::I64 => ra.get_double(dst.into_untyped()).lo(),
			_ => todo!(),
		};

		let addr = map_ra_i32(addr, ra);

		let addr_reg = if let Some(addr_c) = addr.get_const() {
			ra.get_const(addr_c + mem.offset as i32)
		} else {
			// TODO: Coalescing?
			let temp = Register::temp_lo(0);

			block.push(LirInstr::Assign(temp, addr));
			block.push(LirInstr::Add(temp, ra.get_const(mem.offset as i32)));

			temp
		};

		match bits {
			32 => {
				block.push(LirInstr::Load32(dst_lo, addr_reg));
			}
			16 => {
				block.push(LirInstr::Load16(dst_lo, addr_reg));
				if signed {
					block.push(LirInstr::SignExtend16(dst_lo));
				}
			}
			8 => {
				block.push(LirInstr::Load8(dst_lo, addr_reg));
				if signed {
					block.push(LirInstr::SignExtend8(dst_lo));
				}
			}
			_ => panic!()
		}

		if dst.ty() == Type::I64 {
			let dst = ra.get_double(dst.into_untyped());
			block.push(LirInstr::SignExtend32(dst));
		}
	}

	fn do_signext32<F>(dst: TypedSsaVar, src: TypedSsaVar, block: &mut Vec<LirInstr>, ra: &mut FullRegAlloc, f: F)
		where
			F: FnOnce(Register) -> LirInstr
	{
		let src_lo;
		match src.ty() {
			Type::I32 => {
				src_lo = ra.get(src.into_untyped());
			}
			Type::I64 => {
				src_lo = ra.get_double(src.into_untyped()).lo();
			} 
			_ => todo!()
		}

		match dst.ty() {
			Type::I32 => {
				let dst = ra.get(dst.into_untyped());
				block.push(LirInstr::Assign(dst, src_lo));
				block.push(f(dst));
			}
			Type::I64 => {
				let dst = ra.get_double(dst.into_untyped());
				block.push(LirInstr::Assign(dst.lo(), src_lo));
				block.push(f(dst.lo()));
				block.push(LirInstr::SignExtend32(dst));
			}
			_ => todo!()
		}
	}

	let mut new_block_id = block_id;

	let mut block = Vec::new();

	for (instr_idx, instr) in ssa_block.body.iter().enumerate() {
		match instr {
			&super::SsaInstr::I32Set(var, val) => {
				let reg = ra.get(var.unwrap_i32());
				block.push(LirInstr::Set(reg, val));
			}
			&super::SsaInstr::I64Set(var, val) => {
				let reg = ra.get_double(var.unwrap_i64());
				block.push(LirInstr::Set(reg.lo(), val as i32));
				block.push(LirInstr::Set(reg.hi(), (val >> 32) as i32));
			}

			&super::SsaInstr::Assign(lhs, rhs) => {
				assert_eq!(lhs.ty(), rhs.ty());
				match lhs.ty() {
					Type::I32 => {
						let lhs = ra.get(lhs.into_untyped());
						let rhs = ra.get(rhs.into_untyped());
						block.push(LirInstr::Assign(lhs, rhs));
					}
					Type::I64 => {
						let lhs = ra.get_double(lhs.into_untyped());
						let rhs = ra.get_double(rhs.into_untyped());
						block.push(LirInstr::Assign(lhs.lo(), rhs.lo()));
						block.push(LirInstr::Assign(lhs.hi(), rhs.hi()));
					}
					_ => todo!(),
				}
			}

			&super::SsaInstr::Add(dst, lhs, rhs) => {
				let i32_add = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					if dst != lhs {
						block.push(LirInstr::Assign(dst, lhs));
					}

					block.push(LirInstr::Add(dst, rhs));
				};

				let i64_add = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					block.push(LirInstr::Add64(dst, lhs, rhs));
				};

				do_binop(dst, lhs, rhs, &mut block, ra, i32_add, i64_add);
			}
			&super::SsaInstr::Sub(dst, lhs, rhs) => {
				let i32_sub = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					if dst != lhs {
						block.push(LirInstr::Assign(dst, lhs));
					}

					block.push(LirInstr::Sub(dst, rhs));
				};

				let i64_sub = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					block.push(LirInstr::Sub64(dst, lhs, rhs));
				};

				do_binop(dst, lhs, rhs, &mut block, ra, i32_sub, i64_sub);
			}
			&super::SsaInstr::Mul(dst, lhs, rhs) => {
				let i32_mul = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					if dst != lhs {
						block.push(LirInstr::Assign(dst, lhs));
					}

					block.push(LirInstr::Mul(dst, rhs));
				};

				let t = ra.get_temp();

				let i64_mul = |dst, lhs: DoubleRegister, rhs: DoubleRegister, block: &mut Vec<LirInstr>| {
					block.push(LirInstr::MulTo64(dst, lhs.lo(), rhs.lo()));

					block.push(LirInstr::Assign(t, lhs.lo()));
					block.push(LirInstr::Mul(t, rhs.hi()));
					block.push(LirInstr::Add(dst.hi(), t));

					block.push(LirInstr::Assign(t, lhs.hi()));
					block.push(LirInstr::Mul(t, rhs.lo()));
					block.push(LirInstr::Add(dst.hi(), t));
				};

				do_binop(dst, lhs, rhs, &mut block, ra, i32_mul, i64_mul);
			}
			&super::SsaInstr::DivS(dst, lhs, rhs) => {
				let i32_divs = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					block.push(LirInstr::DivS(dst, lhs, rhs));
				};

				let i64_divs = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					block.push(LirInstr::DivS64(dst, lhs, rhs));
				};

				do_binop(dst, lhs, rhs, &mut block, ra, i32_divs, i64_divs);
			}
			&super::SsaInstr::DivU(dst, lhs, rhs) => {
				let i32_divu = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					block.push(LirInstr::DivU(dst, lhs, rhs));
				};

				let i64_divu = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					block.push(LirInstr::DivU64(dst, lhs, rhs));
				};

				do_binop(dst, lhs, rhs, &mut block, ra, i32_divu, i64_divu);

			}
			&super::SsaInstr::RemS(dst, lhs, rhs) => {
				let i32_rems = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					block.push(LirInstr::RemS(dst, lhs, rhs));
				};

				let i64_rems = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					block.push(LirInstr::RemS64(dst, lhs, rhs));
				};

				do_binop(dst, lhs, rhs, &mut block, ra, i32_rems, i64_rems);

			}
			&super::SsaInstr::RemU(dst, lhs, rhs) => {
				let i32_remu = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					block.push(LirInstr::RemU(dst, lhs, rhs));
				};

				let i64_remu = |dst, lhs, rhs, block: &mut Vec<LirInstr>| {
					block.push(LirInstr::RemU64(dst, lhs, rhs));
				};

				do_binop(dst, lhs, rhs, &mut block, ra, i32_remu, i64_remu);
			}

			super::SsaInstr::Shl(dst, lhs, rhs) => do_shiftop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::Shl, LirInstr::Shl64),
			super::SsaInstr::ShrS(dst, lhs, rhs) => do_shiftop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::ShrS, LirInstr::ShrS64),
			super::SsaInstr::ShrU(dst, lhs, rhs) => do_shiftop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::ShrU, LirInstr::ShrU64),
			super::SsaInstr::Rotl(dst, lhs, rhs) => do_shiftop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::Rotl, LirInstr::Rotl64),
			super::SsaInstr::Rotr(dst, lhs, rhs) => do_shiftop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::Rotr, LirInstr::Rotr64),

			super::SsaInstr::Xor(dst, lhs, rhs) => do_bitwiseop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::Xor),
			super::SsaInstr::And(dst, lhs, rhs) => do_bitwiseop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::And),
			super::SsaInstr::Or(dst, lhs, rhs) => do_bitwiseop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::Or),

			super::SsaInstr::GtS(dst, lhs, rhs) => do_compareop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::GtS, LirInstr::GtS64),
			super::SsaInstr::GtU(dst, lhs, rhs) => do_compareop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::GtU, LirInstr::GtU64),
			super::SsaInstr::GeS(dst, lhs, rhs) => do_compareop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::GeS, LirInstr::GeS64),
			super::SsaInstr::GeU(dst, lhs, rhs) => do_compareop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::GeU, LirInstr::GeU64),
			super::SsaInstr::LtS(dst, lhs, rhs) => do_compareop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::LtS, LirInstr::LtS64),
			super::SsaInstr::LtU(dst, lhs, rhs) => do_compareop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::LtU, LirInstr::LtU64),
			super::SsaInstr::LeS(dst, lhs, rhs) => do_compareop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::LeS, LirInstr::LeS64),
			super::SsaInstr::LeU(dst, lhs, rhs) => do_compareop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::LeU, LirInstr::LeU64),
			super::SsaInstr::Eq(dst, lhs, rhs) => do_compareop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::Eq, LirInstr::Eq64),
			super::SsaInstr::Ne(dst, lhs, rhs) => do_compareop(*dst, *lhs, *rhs, &mut block, ra, LirInstr::Ne, LirInstr::Ne64),

			super::SsaInstr::Popcnt(dst, src) => {
				assert_eq!(dst.ty(), src.ty());
				match dst.ty() {
					Type::I32 => {
						let dst = ra.get(dst.into_untyped());
						let src = ra.get(src.into_untyped());
						block.push(LirInstr::Set(dst, 0));
						block.push(LirInstr::PopcntAdd(dst, src));
					}
					Type::I64 => {
						let dst = ra.get_double(dst.into_untyped());
						let src = ra.get_double(src.into_untyped());
						block.push(LirInstr::Set(dst.lo(), 0));
						block.push(LirInstr::Set(dst.hi(), 0));
						block.push(LirInstr::PopcntAdd(dst.lo(), src.lo()));
						block.push(LirInstr::PopcntAdd(dst.lo(), src.hi()));
					}
					_ => todo!()
				}
			}
			super::SsaInstr::Clz(dst, src) => do_unaryop(*dst, *src, &mut block, ra, LirInstr::Clz, LirInstr::Clz64),
			super::SsaInstr::Ctz(dst, src) => do_unaryop(*dst, *src, &mut block, ra, LirInstr::Ctz, LirInstr::Ctz64),

			super::SsaInstr::Eqz(dst, src) => {
				assert_eq!(dst.ty(), Type::I32);
				let dst = ra.get(dst.into_untyped());

				match src.ty() {
					Type::I32 => {
						let src = ra.get(src.into_untyped());
						block.push(LirInstr::Eqz(dst, src));
					}
					Type::I64 => {
						let src = ra.get_double(src.into_untyped());
						block.push(LirInstr::Eqz64(dst, src));
					}
					_ => todo!(),
				}
			}

			super::SsaInstr::Load64(mem, dst, addr) => {
				assert_eq!(mem.memory, 0);

				assert_eq!(dst.ty(), Type::I64);
				let dst = ra.get_double(dst.into_untyped());

				let addr = map_ra_i32(*addr, ra);

				if let Some(addr) = addr.get_const() {
					let addr_lo = ra.get_const(addr + mem.offset as i32);
					let addr_hi = ra.get_const(addr + mem.offset as i32 + 4);
					
					block.push(LirInstr::Load32(dst.lo(), addr_lo));
					block.push(LirInstr::Load32(dst.hi(), addr_hi));
				} else {
					// TODO: Coalescing?
					let temp = Register::temp_lo(0);

					block.push(LirInstr::Assign(temp, addr));
					block.push(LirInstr::Add(temp, ra.get_const(mem.offset as i32)));
					block.push(LirInstr::Load32(dst.lo(), temp));

					// TODO: Coalescing?
					let temp = Register::temp_lo(0);

					block.push(LirInstr::Assign(temp, addr));
					block.push(LirInstr::Add(temp, ra.get_const(mem.offset as i32 + 4)));
					block.push(LirInstr::Load32(dst.hi(), temp));
				}
			}
			super::SsaInstr::Load32S(mem, dst, addr) => do_load_trunc(mem, *dst, *addr, 32, true, &mut block, ra),
			super::SsaInstr::Load32U(mem, dst, addr) => do_load_trunc(mem, *dst, *addr, 32, false, &mut block, ra),
			super::SsaInstr::Load16S(mem, dst, addr) => do_load_trunc(mem, *dst, *addr, 16, true, &mut block, ra),
			super::SsaInstr::Load16U(mem, dst, addr) => do_load_trunc(mem, *dst, *addr, 16, false, &mut block, ra),
			super::SsaInstr::Load8S(mem, dst, addr) => do_load_trunc(mem, *dst, *addr, 8, true, &mut block, ra),
			super::SsaInstr::Load8U(mem, dst, addr) => do_load_trunc(mem, *dst, *addr, 8, false, &mut block, ra),

			super::SsaInstr::Store64(mem, src, addr) => {
				assert_eq!(mem.memory, 0);

				assert_eq!(src.ty(), Type::I64);
				let src = ra.get_double(src.into_untyped());

				assert_eq!(addr.ty(), Type::I32);
				let addr = map_ra_i32(*addr, ra);

				if let Some(c) = addr.get_const() {
					let addr_lo = c + mem.offset as i32;
					let addr_hi = c + mem.offset as i32 + 4;
					
					block.push(LirInstr::Store32(src.lo(), ra.get_const(addr_lo)));
					block.push(LirInstr::Store32(src.hi(), ra.get_const(addr_hi)));
				} else {
					// TODO: Coalescing?
					let temp = Register::temp_lo(0);

					block.push(LirInstr::Assign(temp, addr));
					block.push(LirInstr::Add(temp, ra.get_const(mem.offset as i32)));
					block.push(LirInstr::Store32(src.lo(), temp));

					block.push(LirInstr::Assign(temp, addr));
					block.push(LirInstr::Add(temp, ra.get_const(mem.offset as i32 + 4)));
					block.push(LirInstr::Store32(src.hi(), temp));
				}
			}
			super::SsaInstr::Store32(mem, src, addr) => do_store(mem, *src, *addr, &mut block, ra, LirInstr::Store32),
			super::SsaInstr::Store16(mem, src, addr) => do_store(mem, *src, *addr, &mut block, ra, LirInstr::Store16),
			super::SsaInstr::Store8(mem, src, addr) => do_store(mem, *src, *addr, &mut block, ra, LirInstr::Store8),

			super::SsaInstr::GlobalSet(dst, src) => {
				match src.ty() {
					Type::I32 => {
						let reg = ra.get(src.into_untyped());
						block.push(LirInstr::GlobalSet(*dst, Half::Lo, reg));
					}
					_ => todo!()
				}
			}
			super::SsaInstr::GlobalGet(dst, src) => {
				match dst.ty() {
					Type::I32 => {
						let reg = ra.get(dst.into_untyped());
						block.push(LirInstr::GlobalGet(reg, *src, Half::Lo));
					}
					_ => todo!(),
				}
			}

			super::SsaInstr::LocalSet(dst, src) => {
				match src.ty() {
					Type::I32 => {
						let reg = ra.get(src.into_untyped());
						block.push(LirInstr::LocalSet(*dst, Half::Lo, reg));
					}
					Type::I64 => {
						let reg = ra.get_double(src.into_untyped());
						block.push(LirInstr::LocalSet(*dst, Half::Lo, reg.lo()));
						block.push(LirInstr::LocalSet(*dst, Half::Hi, reg.hi()));
					}
					_ => todo!()
				}
			}
			super::SsaInstr::LocalGet(dst, src) => {
				match dst.ty() {
					Type::I32 => {
						let reg = ra.get(dst.into_untyped());
						block.push(LirInstr::LocalGet(reg, *src, Half::Lo));
					}
					Type::I64 => {
						let reg = ra.get_double(dst.into_untyped());
						block.push(LirInstr::LocalGet(reg.lo(), *src, Half::Lo));
						block.push(LirInstr::LocalGet(reg.hi(), *src, Half::Hi));
					}
					_ => todo!(),
				}
			}
			super::SsaInstr::ParamGet(dst, src) => {
				match dst.ty() {
					Type::I32 => {
						let dst = ra.get(dst.into_untyped());
						let src = Register::param_lo(*src);
						block.push(LirInstr::Assign(dst, src));
					}
					Type::I64 => {
						let dst = ra.get_double(dst.into_untyped());
						let src = DoubleRegister::param(*src);
						block.push(LirInstr::Assign(dst.lo(), src.lo()));
						block.push(LirInstr::Assign(dst.hi(), src.hi()));
					}
					_ => todo!(),
				}
			}

			&super::SsaInstr::Extend8S(dst, src) => do_signext32(dst, src, &mut block, ra, LirInstr::SignExtend8),
			&super::SsaInstr::Extend16S(dst, src) => do_signext32(dst, src, &mut block, ra, LirInstr::SignExtend16),
			&super::SsaInstr::Extend32S(dst, src) => {
				assert_eq!(dst.ty(), Type::I64);
				let dst = ra.get_double(dst.into_untyped());

				let src_lo;
				match src.ty() {
					Type::I32 => {
						src_lo = ra.get(src.into_untyped());
					}
					Type::I64 => {
						src_lo = ra.get_double(src.into_untyped()).lo();
					}
					_ => todo!()
				}

				block.push(LirInstr::Assign(dst.lo(), src_lo));
				block.push(LirInstr::SignExtend32(dst));
			}
			&super::SsaInstr::Extend32U(dst, src) => {
				assert_eq!(dst.ty(), Type::I64);
				let dst = ra.get_double(dst.into_untyped());

				let src_lo;
				match src.ty() {
					Type::I32 => {
						src_lo = ra.get(src.into_untyped());
					}
					Type::I64 => {
						src_lo = ra.get_double(src.into_untyped()).lo();
					}
					_ => todo!()
				}

				block.push(LirInstr::Assign(dst.lo(), src_lo));
				block.push(LirInstr::Set(dst.hi(), 0));
			}
			super::SsaInstr::Wrap(dst, src) => {
				assert_eq!(dst.ty(), Type::I32);
				assert_eq!(src.ty(), Type::I64);

				let dst = ra.get(dst.into_untyped());
				let src = ra.get_double(src.into_untyped());
				block.push(LirInstr::Assign(dst, src.lo()));
			}
			super::SsaInstr::Select { dst, true_var, false_var, cond } => {
				assert_eq!(cond.ty(), Type::I32);
				let cond = ra.get(cond.into_untyped());

				assert_eq!(dst.ty(), true_var.ty());
				assert_eq!(dst.ty(), false_var.ty());

				match dst.ty() {
					Type::I32 => {
						let dst = ra.get(dst.into_untyped());
						let true_reg = ra.get(true_var.into_untyped());
						let false_reg = ra.get(false_var.into_untyped());

						block.push(LirInstr::Select { dst, true_reg, false_reg, cond });
					}
					Type::I64 => {
						let dst = ra.get_double(dst.into_untyped());
						let true_reg = ra.get_double(true_var.into_untyped());
						let false_reg = ra.get_double(false_var.into_untyped());

						block.push(LirInstr::Select { dst: dst.lo(), true_reg: true_reg.lo(), false_reg: false_reg.lo(), cond });
						block.push(LirInstr::Select { dst: dst.hi(), true_reg: true_reg.hi(), false_reg: false_reg.hi(), cond });
					}
					_ => panic!(),
				}

			}
			super::SsaInstr::Call { function_index, params, returns } => {
				emit_copy_to_params(&mut block, params, ra);

				let mut to_save = li.live_out_body(block_id, instr_idx);
				for return_var in returns.iter() {
					to_save.remove(return_var);
				}
				let to_save = to_save.into_iter().collect::<Vec<_>>();

				let needs_save = call_graph.may_call(*function_index, block_id.func as u32);

				if needs_save {
					emit_save(&mut block, &to_save, ra);
				}

				match jump_mode() {
					JumpMode::Direct => {
						if call_graph.is_single_tick(*function_index) {
							block.push(LirInstr::Call { func: *function_index });
						} else {
							let next_block_id = builder.alloc_block_id();

							let entry_point = BlockId { func: *function_index as usize, block: 0 };

							block.push(LirInstr::PushReturnAddr(next_block_id));
							builder.push(block_id, block, LirTerminator::Jump(entry_point));

							block_id = next_block_id;
							block = Vec::new();
						}
					}
				}

				if needs_save {
					emit_restore(&mut block, &to_save, ra);
				}

				emit_copy_from_returns(&mut block, returns, ra);
			}
			super::SsaInstr::CallIndirect { table_index, table_entry, params, returns } => {
				emit_copy_to_params(&mut block, params, ra);

				let mut to_save = li.live_out_body(block_id, instr_idx);
				for return_var in returns.iter() {
					to_save.remove(return_var);
				}
				let to_save = to_save.into_iter().collect::<Vec<_>>();

				let needs_save = call_graph.table_may_call(*table_index, block_id.func as u32);

				if needs_save {
					emit_save(&mut block, &to_save, ra);
				}

				assert_eq!(table_entry.ty(), Type::I32);
				let table_entry = ra.get(table_entry.into_untyped());

				match jump_mode() {
					JumpMode::Direct => {
						if call_graph.table_is_single_tick(*table_index) {
							block.push(LirInstr::CallIndirect { table_index: *table_index, table_entry })
						} else {
							let next_block_id = builder.alloc_block_id();

							block.push(LirInstr::PushReturnAddr(next_block_id));

							let arms = parent.tables[*table_index as usize].elements.iter().map(|elem| {
								elem.map(|func_idx| {
									BlockId { func: func_idx, block: 0 }
								})
							}).collect();

							builder.push(block_id, block, LirTerminator::JumpTable { arms, default: None, cond: table_entry });


							block_id = next_block_id;
							block = Vec::new();
						}
					}
				}

				if needs_save {
					emit_restore(&mut block, &to_save, ra);
				}

				emit_copy_from_returns(&mut block, returns, ra);
			}

			&super::SsaInstr::TurtleSetX(v) => {
				let reg = ra.get(v.unwrap_i32());
				block.push(LirInstr::TurtleSetX(reg));
			}
			&super::SsaInstr::TurtleSetY(v) => {
				let reg = ra.get(v.unwrap_i32());
				block.push(LirInstr::TurtleSetY(reg));
			}
			&super::SsaInstr::TurtleSetZ(v) => {
				let reg = ra.get(v.unwrap_i32());
				block.push(LirInstr::TurtleSetZ(reg));
			}
			&super::SsaInstr::TurtleSetBlock(v) => {
				let reg = ra.get(v.unwrap_i32());
				block.push(LirInstr::TurtleSetBlock(reg));
			}
			&super::SsaInstr::TurtleGetBlock(v) => {
				let reg = ra.get(v.unwrap_i32());
				block.push(LirInstr::TurtleGetBlock(reg));
			}
			&super::SsaInstr::PrintInt(v) => {
				let reg = ra.get(v.unwrap_i32());
				block.push(LirInstr::PrintInt(reg));
			}
		}
	}

	match &ssa_block.term {
		crate::ssa::SsaTerminator::Unreachable => todo!(),
		crate::ssa::SsaTerminator::ScheduleJump(target, delay) => {
			assert!(target.params.is_empty());
			assert!(parent_func.get(target.label).params.is_empty());
			
			builder.push(block_id, block, LirTerminator::ScheduleJump(target.label, *delay));
		}
		crate::ssa::SsaTerminator::Jump(target) => {
			let out_params = &parent_func.get(target.label).params;
			emit_copy(&mut block, &target.params, out_params, ra, &[]);

			builder.push(block_id, block, LirTerminator::Jump(target.label));
		}
		crate::ssa::SsaTerminator::BranchIf { cond, true_target, false_target } => {
			if jump_mode() != JumpMode::Direct {
				todo!()
			}

			assert_eq!(cond.ty(), Type::I32);
			let cond = ra.get(cond.into_untyped());

			let true_out_params = &parent_func.get(true_target.label).params;
			let false_out_params = &parent_func.get(false_target.label).params;

			let true_conds = &[Condition::eq_zero(Register::cond_taken()), Condition::neq_zero(cond)];
			let false_conds = &[Condition::eq_zero(Register::cond_taken()), Condition::eq_zero(cond)];

			for param in true_out_params.iter().chain(false_out_params.iter()) {
				assert_ne!(cond, ra.get(param.into_untyped()));
			}

			// FIXME: Make sure cond is not overwritten!

			block.push(LirInstr::Set(Register::cond_taken(), 0));
			emit_copy(&mut block, &true_target.params, true_out_params, ra, true_conds);
			emit_copy(&mut block, &false_target.params, false_out_params, ra, false_conds);
			builder.push(block_id, block, LirTerminator::JumpIf { true_label: true_target.label, false_label: false_target.label, cond });
		}
		crate::ssa::SsaTerminator::BranchTable { cond, default, arms } => {
			if jump_mode() != JumpMode::Direct {
				todo!()
			}

			if arms.is_empty() {
				let out_params = &parent_func.get(default.label).params;
				emit_copy(&mut block, &default.params, out_params, ra, &[]);

				builder.push(block_id, block, LirTerminator::Jump(default.label));
			} else {
				assert_eq!(cond.ty(), Type::I32);
				let cond = ra.get(cond.into_untyped());

				let default_out_params = &parent_func.get(default.label).params;
				let other_out_params = arms.iter().map(|arm| &parent_func.get(arm.label).params).flatten();
				for out_param in other_out_params.chain(default_out_params.iter()) {
					assert_ne!(cond, ra.get(out_param.into_untyped()));
				}

				block.push(LirInstr::Set(Register::cond_taken(), 0));

				assert!(!arms.is_empty());
				let range = 0..=(arms.len() as i32 - 1);

				let default_conds = &[Condition::eq_zero(Register::cond_taken()), Condition::NotMatches(cond, range)];
				emit_copy(&mut block, &default.params, default_out_params, ra, default_conds);

				for (i, arm) in arms.iter().enumerate() {
					let out_params = &parent_func.get(arm.label).params;
					let conds = &[Condition::eq_zero(Register::cond_taken()), Condition::eq_const(cond, i as i32)];
					emit_copy(&mut block, &arm.params, out_params, ra, conds);
				}

				let arm_labels = arms.iter().map(|arm| Some(arm.label)).collect();

				builder.push(block_id, block, LirTerminator::JumpTable { default: Some(default.label), arms: arm_labels, cond });
			}
		}
		crate::ssa::SsaTerminator::Return(return_vars) => {
			for (idx, var) in return_vars.iter().enumerate() {
				match var.ty() {
					Type::I32 => {
						let src = ra.get(var.into_untyped());
						let dst = Register::return_lo(idx as u32);
						block.push(LirInstr::Assign(dst, src));
					}
					Type::I64 => {
						let src = ra.get_double(var.into_untyped());
						let dst = DoubleRegister::return_reg(idx as u32);
						block.push(LirInstr::Assign(dst.lo(), src.lo()));
						block.push(LirInstr::Assign(dst.hi(), src.hi()));
					}
					_ => panic!(),
				}
			}

			if call_graph.is_single_tick(parent_func.func_id()) {
				builder.push(block_id, block, LirTerminator::Return);
			} else {
				builder.push(block_id, block, LirTerminator::ReturnToSaved);
			}
		}
	}
}

fn emit_copy_to_params(block: &mut Vec<LirInstr>, vars: &[TypedSsaVar], ra: &mut FullRegAlloc) {
	for (id, var) in vars.iter().enumerate() {
		match var.ty() {
			Type::I32 => {
				let dst = Register::param_lo(id as u32);
				let src = ra.get(var.into_untyped());
				block.push(LirInstr::Assign(dst, src));
			}
			Type::I64 => {
				let dst = DoubleRegister::param(id as u32);
				let src = ra.get_double(var.into_untyped());
				block.push(LirInstr::Assign(dst.lo(), src.lo()));
				block.push(LirInstr::Assign(dst.hi(), src.hi()));
			}
			_ => panic!()
		}
	}
}

fn emit_copy_from_returns(block: &mut Vec<LirInstr>, vars: &[TypedSsaVar], ra: &mut FullRegAlloc) {
	for (id, var) in vars.iter().enumerate() {
		match var.ty() {
			Type::I32 => {
				let dst = ra.get(var.into_untyped());
				let src = Register::return_lo(id as u32);
				block.push(LirInstr::Assign(dst, src));
			}
			Type::I64 => {
				let dst = ra.get_double(var.into_untyped());
				let src = DoubleRegister::return_reg(id as u32);
				block.push(LirInstr::Assign(dst.lo(), src.lo()));
				block.push(LirInstr::Assign(dst.hi(), src.hi()));
			}
			_ => panic!(),
		}
	}
}

fn get_save_reg_list(to_save: &[TypedSsaVar], ra: &mut FullRegAlloc) -> Vec<Register> {
	to_save.iter().flat_map(|var| {
		match var.ty() {
			Type::I32 => {
				let reg = ra.get(var.into_untyped());
				[reg, reg].into_iter().take(1)
				
			}
			Type::I64 => {
				let reg = ra.get_double(var.into_untyped());
				[reg.lo(), reg.hi()].into_iter().take(2)
			}
			_ => todo!()
		}
	}).collect()
}

fn emit_save(block: &mut Vec<LirInstr>, to_save: &[TypedSsaVar], ra: &mut FullRegAlloc) {
	block.push(LirInstr::Push(get_save_reg_list(to_save, ra)));
}

fn emit_restore(block: &mut Vec<LirInstr>, to_restore: &[TypedSsaVar], ra: &mut FullRegAlloc) {
	block.push(LirInstr::Pop(get_save_reg_list(to_restore, ra)));
}

fn emit_copy(block: &mut Vec<LirInstr>, in_params: &[TypedSsaVar], out_params: &[TypedSsaVar], ra: &mut FullRegAlloc, conds: &[Condition]) {
	assert_eq!(in_params.len(), out_params.len());

	assert!(out_params.iter().zip(in_params.iter()).all(|(o, i)| o.ty() == i.ty()));

	let mut add_instr = |mut instr: LirInstr| {
		for cond in conds {
			instr = instr.if_cond(cond.clone());
		}

		block.push(instr);
	};

	let param_pairs = in_params.iter().zip(out_params.iter()).filter(|(i, o)| {
		match i.ty() {
			Type::I32 => ra.get(i.into_untyped()) != ra.get(o.into_untyped()),
			Type::I64 => ra.get_double(i.into_untyped()) != ra.get_double(o.into_untyped()),
			_ => todo!(),
		}
	});

	// TODO: Optimize

	for (idx, (in_param, _)) in param_pairs.clone().enumerate() {
		match in_param.ty() {
			Type::I32 => {
				let tmp = Register::temp_lo(idx as u32);
				let in_reg = ra.get(in_param.into_untyped());
				add_instr(LirInstr::Assign(tmp, in_reg));
			}
			Type::I64 => {
				let tmp = DoubleRegister::temp(idx as u32);
				let in_reg = ra.get_double(in_param.into_untyped());
				add_instr(LirInstr::Assign(tmp.lo(), in_reg.lo()));
				add_instr(LirInstr::Assign(tmp.hi(), in_reg.hi()));
			}
			_ => panic!(),
		}
	}

	for (idx, (_, out_param)) in param_pairs.enumerate() {
		match out_param.ty() {
			Type::I32 => {
				let tmp = Register::temp_lo(idx as u32);
				let out_reg = ra.get(out_param.into_untyped());
				add_instr(LirInstr::Assign(out_reg, tmp));
			}
			Type::I64 => {
				let tmp = DoubleRegister::temp(idx as u32);
				let out_reg = ra.get_double(out_param.into_untyped());
				add_instr(LirInstr::Assign(out_reg.lo(), tmp.lo()));
				add_instr(LirInstr::Assign(out_reg.hi(), tmp.hi()));
			}
			_ => todo!(),
		}
	}
}

const MANUALLY_ZERO_LOCALS: bool = false;

fn gen_prologue(ssa_func: &SsaFunction, ssa_program: &SsaProgram, ra: &mut FullRegAlloc) -> Vec<LirInstr> {
	let mut result = Vec::new();

	let locals = ssa_program.local_types.get(&(ssa_func.func_id() as usize)).unwrap();

	result.push(LirInstr::PushLocalFrame(locals.clone()));

	assert!(locals.len() >= ssa_func.params.len());

	for (idx, (local, param)) in locals.iter().zip(ssa_func.params.iter()).enumerate() {
		assert_eq!(*local, *param);
		match param {
			Type::I32 => {
				result.push(LirInstr::LocalSet(idx as u32, Half::Lo, Register::param_lo(idx as u32)));
			}
			Type::I64 => {
				result.push(LirInstr::LocalSet(idx as u32, Half::Lo, Register::param_lo(idx as u32)));
				result.push(LirInstr::LocalSet(idx as u32, Half::Hi, Register::param_hi(idx as u32)));
			}
			_ => todo!(),
		}
	}

	if MANUALLY_ZERO_LOCALS {
		for (idx, local) in locals.iter().enumerate().skip(ssa_func.params.len()) {
			match *local {
				Type::I32 => {
					result.push(LirInstr::LocalSet(idx as u32, Half::Lo, ra.get_const(0)));
				}
				Type::I64 => {
					result.push(LirInstr::LocalSet(idx as u32, Half::Lo, ra.get_const(0)));
					result.push(LirInstr::LocalSet(idx as u32, Half::Hi, ra.get_const(0)));
				}
				_ => todo!(),
			}
		}
	}

	result
}

fn lower(ssa_func: &SsaFunction, ssa_program: &SsaProgram, call_graph: &CallGraph, constant_pool: &mut HashSet<i32>) -> LirFunction {
	let mut reg_alloc = FullRegAlloc::analyze(&ssa_func);

	let mut builder = LirFuncBuilder::new(ssa_func);

	let liveness_info = SimpleLivenessInfo::analyze(ssa_func);

	for (block_id, block) in ssa_func.iter() {
		lower_block(ssa_program, ssa_func, block_id, block, &mut reg_alloc, &liveness_info, call_graph, &mut builder);
	}

	let locals = ssa_program.local_types.get(&(ssa_func.func_id() as usize)).unwrap();

	let start_block = &mut builder.body.iter_mut().find(|(block_id, _block)| block_id.block == 0).unwrap().1;
	let prologue = gen_prologue(ssa_func, ssa_program, &mut reg_alloc);
	start_block.body.splice(0..0, prologue);

	let end_block = &mut builder.body.iter_mut().find(|(block_id, _block)| block_id.block == 1).unwrap().1;
	end_block.body.push(LirInstr::PopLocalFrame(locals.clone()));

	let blocks = builder.body;

	constant_pool.extend(reg_alloc.const_pool);

	LirFunction { code: blocks, returns: ssa_func.returns.clone() }
}

pub fn convert(ssa_program: SsaProgram) -> LirProgram {
	let call_graph = CallGraph::new(&ssa_program);

	let mut constants = HashSet::new();

	let code = ssa_program.code.iter().map(|block| lower(block, &ssa_program, &call_graph, &mut constants)).collect::<Vec<_>>();

	for (func_id, func) in code.iter().enumerate() {
		println!("==== func {:?} ==== ", func_id);
		for (block_id, block) in func.code.iter() {
			println!("-- block {:?} --", block_id);
			for instr in block.body.iter() {
				println!("{:?}", instr);
			}
			println!("{:?}", block.term);
		}
	}

	LirProgram { code, memory: ssa_program.memory, tables: ssa_program.tables, globals: ssa_program.globals, constants, exports: ssa_program.exports }
}