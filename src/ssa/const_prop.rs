use std::collections::HashMap;

use wasmparser::Type;

use super::{SsaProgram, SsaBasicBlock, TypedSsaVar, interp::TypedValue, SsaInstr, SsaVarOrConst, BlockId};

pub fn do_const_prop(program: &mut SsaProgram) -> HashMap<TypedSsaVar, StaticValue> {
	let mut result = HashMap::new();
	for func in program.code.iter_mut() {
		for (id, block) in func.iter_mut() {
			result.extend(do_block_const_prop(id, block));
		}
	}
	result
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BitMask {
	/// A 1 in this mask means that the value will always have a 1 at that position
	pub set_bits: u64,
	/// A 1 in this mask means that the value will always have a 0 at that position.
	pub clr_bits: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StaticValue {
	Mask(BitMask),
	Constant(TypedValue),
}

impl From<i32> for StaticValue {
	fn from(v: i32) -> Self {
		let v: TypedValue = v.into();
		v.into()
	}
}

impl From<i64> for StaticValue {
	fn from(v: i64) -> Self {
		let v: TypedValue = v.into();
		v.into()
	}
}

impl From<TypedValue> for StaticValue {
	fn from(v: TypedValue) -> Self {
		StaticValue::Constant(v)
	}
}

impl From<BitMask> for StaticValue {
	fn from(v: BitMask) -> Self {
		StaticValue::Mask(v)
	}
}

pub fn do_block_const_prop(id: BlockId, block: &mut SsaBasicBlock) -> HashMap<TypedSsaVar, StaticValue> {
	let mut constants = HashMap::<TypedSsaVar, StaticValue>::new();

	for instr in block.body.iter_mut() {
		let args = instr.constable_vars();

		for arg in args {
			if let SsaVarOrConst::Var(v) = arg {
				if let Some(StaticValue::Constant(a)) = constants.get(v) {
					*arg = (*a).into();
				}
			}
		}

		let get_sv = |voc: SsaVarOrConst, constants: &HashMap<TypedSsaVar, StaticValue>| -> Option<StaticValue> {
			match voc {
				SsaVarOrConst::Var(v) => constants.get(&v).copied(),
				SsaVarOrConst::Const(c) => Some(StaticValue::Constant(c)),
			}
		};

		match *instr {
			SsaInstr::Load32U(_, dst, _) => {
				constants.insert(dst, BitMask { set_bits: 0, clr_bits: 0xFFFF_FFFF_0000_0000 }.into());
			}
			SsaInstr::Load16U(_, dst, _) => {
				constants.insert(dst, BitMask { set_bits: 0, clr_bits: 0xFFFF_FFFF_FFFF_0000 }.into());
			}
			SsaInstr::Load8U(_, dst, _) => {
				constants.insert(dst, BitMask { set_bits: 0, clr_bits: 0xFFFF_FFFF_FFFF_FF00 }.into());
			}
			SsaInstr::I32Set(dst, src) => {
				constants.insert(dst, TypedValue::I32(src).into());
			}
			SsaInstr::Assign(dst, src) => {
				if let Some(sv) = get_sv(src, &constants) {
					constants.insert(dst, sv);
				}
			}
			SsaInstr::Select { dst, true_var, false_var, cond: _ } => {
				let true_val = get_sv(true_var, &constants).unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());
				let false_val = get_sv(false_var, &constants).unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());

				match (true_val, false_val) {
					(StaticValue::Mask(msk), StaticValue::Constant(cst)) |
					(StaticValue::Constant(cst), StaticValue::Mask(msk)) => {
						let c = match cst {
							TypedValue::I32(c) => c as u32 as u64,
							TypedValue::I64(c) => c as u64,
						};

						constants.insert(dst, BitMask { set_bits: msk.set_bits & c, clr_bits: msk.clr_bits & !c }.into());
					}
					(StaticValue::Mask(msk1), StaticValue::Mask(msk2)) => {
						constants.insert(dst, BitMask { set_bits: msk1.set_bits & msk2.clr_bits, clr_bits: msk1.clr_bits & msk2.clr_bits }.into());
					}
					(StaticValue::Constant(cst1), StaticValue::Constant(cst2)) => {
						let c1 = match cst1 {
							TypedValue::I32(c) => c as u32 as u64,
							TypedValue::I64(c) => c as u64,
						};
						let c2 = match cst2 {
							TypedValue::I32(c) => c as u32 as u64,
							TypedValue::I64(c) => c as u64,
						};

						constants.insert(dst, BitMask { set_bits: c1 & c2, clr_bits: !c1 & !c2 }.into());
					}
				}
			}
			SsaInstr::And(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied();
				let rhs_val = get_sv(rhs, &constants);

				match (lhs_val, rhs_val) {
					(None, None) => {}
					(Some(StaticValue::Constant(c)), None) |
					(None, Some(StaticValue::Constant(c))) => {
						let c = match c {
							TypedValue::I32(c) => c as u32 as u64,
							TypedValue::I64(c) => c as u64,
						};

						constants.insert(dst, BitMask { set_bits: 0, clr_bits: !c }.into());
					}
					(Some(StaticValue::Mask(msk)), Some(StaticValue::Constant(cst))) |
					(Some(StaticValue::Constant(cst)), Some(StaticValue::Mask(msk))) => {
						let cst = match cst {
							TypedValue::I32(c) => c as u32 as u64,
							TypedValue::I64(c) => c as u64,
						};
						
						constants.insert(dst, BitMask { set_bits: cst & msk.set_bits, clr_bits: !cst | msk.clr_bits }.into());
					}
					(Some(StaticValue::Mask(msk)), None) |
					(None, Some(StaticValue::Mask(msk))) => {
						constants.insert(dst, BitMask { set_bits: 0, clr_bits: msk.clr_bits }.into());
					}
					(Some(StaticValue::Mask(l)), Some(StaticValue::Mask(r))) => {
						constants.insert(dst, BitMask { set_bits: l.set_bits & r.set_bits, clr_bits: l.clr_bits | r.clr_bits }.into());
					}
					(Some(StaticValue::Constant(l)), Some(StaticValue::Constant(r))) => {
						match (l, r) {
							(TypedValue::I32(l), TypedValue::I32(r)) => {
								*instr = SsaInstr::I32Set(dst, l & r);
								constants.insert(dst, (l & r).into());
							}
							(TypedValue::I64(l), TypedValue::I64(r)) => {
								*instr = SsaInstr::I64Set(dst, l & r);
								constants.insert(dst, (l & r).into());
							}
							_ => panic!(),
						};
					}
				}
			}
			SsaInstr::Or(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied();
				let rhs_val = get_sv(rhs, &constants);

				match (lhs_val, rhs_val) {
					(None, None) => {}
					(Some(StaticValue::Mask(v1)), Some(StaticValue::Mask(v2))) => {
						let v1_maybe_set = !v1.clr_bits;
						let v2_maybe_set = !v2.clr_bits;
						if v1_maybe_set & v2_maybe_set == 0 {
							*instr = SsaInstr::Add(dst, lhs.into(), rhs);
						}
						constants.insert(dst, BitMask { set_bits: v1.set_bits | v2.set_bits, clr_bits: v1.clr_bits & v2.clr_bits }.into());
					}
					(Some(StaticValue::Mask(msk)), None) |
					(None, Some(StaticValue::Mask(msk))) => {
						constants.insert(dst, BitMask { set_bits: msk.set_bits, clr_bits: 0 }.into());
					}
					(Some(StaticValue::Constant(cst)), None) |
					(None, Some(StaticValue::Constant(cst))) => {
						let c = match cst {
							TypedValue::I32(c) => c as u32 as u64,
							TypedValue::I64(c) => c as u64,
						};
						constants.insert(dst, BitMask { set_bits: c, clr_bits: 0 }.into());
					}
					(Some(StaticValue::Mask(msk)), Some(StaticValue::Constant(cst))) |
					(Some(StaticValue::Constant(cst)), Some(StaticValue::Mask(msk))) => {
						let c = match cst {
							TypedValue::I32(c) => c as u32 as u64,
							TypedValue::I64(c) => c as u64,
						};
						if !msk.clr_bits & c == 0 {
							*instr = SsaInstr::Add(dst, lhs.into(), rhs);
						}
						constants.insert(dst, BitMask { set_bits: msk.set_bits | c, clr_bits: msk.clr_bits & !c }.into());
					}
					_ => todo!("{:X?} {:X?}", lhs_val, rhs_val)
				}
			}
			SsaInstr::Shl(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied().unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());
				let rhs_val = get_sv(rhs, &constants).unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());

				match (lhs_val, rhs_val) {
					(_, StaticValue::Mask(_)) => {},
					(StaticValue::Mask(msk), StaticValue::Constant(cst)) => {
						let c = match cst {
							TypedValue::I32(c) => c as u32 as u64,
							TypedValue::I64(c) => c as u64,
						};

						let mut dst_mask = msk;
						dst_mask.set_bits <<= c;
						dst_mask.clr_bits <<= c;
						dst_mask.clr_bits |= (1 << c) - 1;
						constants.insert(dst, dst_mask.into());
					}
					(StaticValue::Constant(_), StaticValue::Constant(_)) => {
						todo!()
					}
				}
			}
			SsaInstr::ShrS(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied().unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());
				let rhs_val = get_sv(rhs, &constants).unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());

				match (lhs_val, rhs_val) {
					(StaticValue::Mask(msk), StaticValue::Constant(cst)) => {
						let is_ok_64 = lhs.ty() == Type::I64 && msk.clr_bits & (1 << 63) != 0;
						let is_ok_32 = lhs.ty() == Type::I32 && msk.clr_bits & (1 << 31) != 0;
						if is_ok_64 || is_ok_32 {
							let c = match cst {
								TypedValue::I32(c) => c as u32 as u64,
								TypedValue::I64(c) => c as u64,
							};

							let mut dst_mask = msk;
							dst_mask.set_bits >>= c;
							dst_mask.clr_bits >>= c;
							for i in 0..c {
								dst_mask.clr_bits |= 1 << (31 - i)
							}
							constants.insert(dst, dst_mask.into());
						}
					},
					(_, StaticValue::Mask(_)) => {}
					(StaticValue::Constant(_), StaticValue::Constant(_)) => todo!(),
				}
			}
			SsaInstr::ShrU(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied().unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());
				let rhs_val = get_sv(rhs, &constants).unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());

				match (lhs_val, rhs_val) {
					(StaticValue::Mask(msk), StaticValue::Mask(_)) => {
						let is_ok_64 = lhs.ty() == Type::I64 && msk.clr_bits & (1 << 63) != 0;
						let is_ok_32 = lhs.ty() == Type::I32 && msk.clr_bits & (1 << 31) != 0;

						if is_ok_64 || is_ok_32 {
							*instr = SsaInstr::ShrS(dst, lhs, rhs);
						}
					},
					(StaticValue::Constant(cst), StaticValue::Mask(_)) => {
						match cst {
							TypedValue::I32(c) => if c >= 0 { *instr = SsaInstr::ShrS(dst, lhs, rhs); },
							TypedValue::I64(c) => if c >= 0 { *instr = SsaInstr::ShrS(dst, lhs, rhs); },
						};
					}
					(StaticValue::Mask(msk), StaticValue::Constant(cst)) => {
						let c = match cst {
							TypedValue::I32(c) => c as u32 as u64,
							TypedValue::I64(c) => c as u64,
						};

						let mut dst_mask = msk;
						dst_mask.set_bits >>= c;
						dst_mask.clr_bits >>= c;
						for i in 0..c {
							dst_mask.clr_bits |= 1 << (31 - i)
						}
						constants.insert(dst, dst_mask.into());

						let is_ok_64 = lhs.ty() == Type::I64 && msk.clr_bits & (1 << 63) != 0;
						let is_ok_32 = lhs.ty() == Type::I32 && msk.clr_bits & (1 << 31) != 0;

						if is_ok_64 || is_ok_32 {
							*instr = SsaInstr::ShrS(dst, lhs, rhs);
						}
					}
					(StaticValue::Constant(_), StaticValue::Constant(_)) => {
						todo!()
					}
				}
			}
			SsaInstr::Add(dst, lhs, rhs) => {
				let lhs_val = get_sv(lhs, &constants);
				let rhs_val = get_sv(rhs, &constants);

				if let (Some(lhs_val), Some(rhs_val)) = (lhs_val, rhs_val) {
					match (lhs_val, rhs_val) {
						(StaticValue::Constant(l), StaticValue::Constant(r)) => {
							match (l, r) {
								(TypedValue::I32(l), TypedValue::I32(r)) => {
									let d = l.wrapping_add(r);
									constants.insert(dst, d.into());
									*instr = SsaInstr::I32Set(dst, d);
								}
								(TypedValue::I64(l), TypedValue::I64(r)) => {
									let d = l.wrapping_add(r);
									constants.insert(dst, d.into());
									*instr = SsaInstr::I64Set(dst, d);
								}
								_ => panic!(),
							}
						}
						(StaticValue::Constant(cst), StaticValue::Mask(msk)) |
						(StaticValue::Mask(msk), StaticValue::Constant(cst)) => {
							let c = match cst {
								TypedValue::I32(c) => c as u32 as u64,
								TypedValue::I64(c) => c as u64,
							};

							if !msk.clr_bits & c == 0 {
								constants.insert(dst, BitMask { set_bits: msk.set_bits | c, clr_bits: msk.clr_bits & !c }.into());
							} else {
								let ok_bits1 = msk.clr_bits.trailing_ones();
								let ok_bits2 = c.trailing_zeros();
								let ok_bits = ok_bits1.min(ok_bits2);
								let clr_bits = (1 << (ok_bits + 1)) - 1;
								constants.insert(dst, BitMask { set_bits: 0, clr_bits }.into());
							}
						}
						(StaticValue::Mask(msk1), StaticValue::Mask(msk2)) => {
							if !msk1.clr_bits & !msk2.clr_bits == 0 {
								constants.insert(dst, BitMask { set_bits: msk1.set_bits | msk2.set_bits, clr_bits: msk1.clr_bits & msk2.clr_bits }.into());
							} else {
								let ok_bits1 = msk1.clr_bits.trailing_ones();
								let ok_bits2 = msk2.clr_bits.trailing_ones();
								let ok_bits = ok_bits1.min(ok_bits2);
								if ok_bits >= 29 {
									todo!()
								}
								let clr_bits = (1 << (ok_bits + 1)) - 1;
								constants.insert(dst, BitMask { set_bits: 0, clr_bits }.into());
							}
						}
					}
				}
			}
			_ => {}
		}
	}

	constants
}