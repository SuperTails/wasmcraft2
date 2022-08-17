use std::{collections::HashMap, hash::Hash};

use wasmparser::ValType;

use crate::ssa::liveness::{NoopLivenessInfo, LivenessInfo};

use super::{SsaProgram, SsaBasicBlock, TypedSsaVar, interp::TypedValue, SsaInstr, SsaVarOrConst, BlockId, SsaFunction, liveness::{DomTree, PredInfo}};

pub fn do_const_prop(program: &mut SsaProgram) -> HashMap<TypedSsaVar, StaticValue> {
	let mut result = HashMap::new();
	for func in program.code.iter_mut() {
		for (id, block) in func.iter_mut() {
			result.extend(do_block_const_prop(id, block));
		}
	}
	result
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BitMask {
	/// A 1 in this mask means that the value will always have a 1 at that position
	pub set_bits: u64,
	/// A 1 in this mask means that the value will always have a 0 at that position.
	pub clr_bits: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StaticValue {
	Mask(BitMask),
	Constant(TypedValue),
}

impl Default for StaticValue {
	fn default() -> Self {
		StaticValue::unknown()
	}
}

impl StaticValue {
	pub const fn unknown() -> Self {
		StaticValue::Mask(BitMask { set_bits: 0, clr_bits: 0 })
	}

	pub fn into_mask(self) -> BitMask {
		match self {
			StaticValue::Mask(msk) => msk,
			StaticValue::Constant(TypedValue::I32(c)) => BitMask { set_bits: c as u32 as u64, clr_bits: !(c as u32 as u64) },
			StaticValue::Constant(TypedValue::I64(c)) => BitMask { set_bits: c as u64, clr_bits: c as u64 },
		}
	}

	pub fn add(self, other: Self) -> Self {
		match (self, other) {
			(StaticValue::Constant(l), StaticValue::Constant(r)) => {
				match (l, r) {
					(TypedValue::I32(l), TypedValue::I32(r)) => {
						l.wrapping_add(r).into()
					}
					(TypedValue::I64(l), TypedValue::I64(r)) => {
						l.wrapping_add(r).into()
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
					BitMask { set_bits: msk.set_bits | c, clr_bits: msk.clr_bits & !c }.into()
				} else {
					let ok_bits1 = msk.clr_bits.trailing_ones();
					let ok_bits2 = c.trailing_zeros();
					let ok_bits = ok_bits1.min(ok_bits2);
					let clr_bits = (1 << ok_bits) - 1;
					BitMask { set_bits: 0, clr_bits }.into()
				}
			}
			(StaticValue::Mask(msk1), StaticValue::Mask(msk2)) => {
				if !msk1.clr_bits & !msk2.clr_bits == 0 {
					BitMask { set_bits: msk1.set_bits | msk2.set_bits, clr_bits: msk1.clr_bits & msk2.clr_bits }.into()
				} else {
					let ok_bits1 = msk1.clr_bits.trailing_ones();
					let ok_bits2 = msk2.clr_bits.trailing_ones();
					let ok_bits = ok_bits1.min(ok_bits2);
					if ok_bits >= 29 {
						todo!()
					}
					let clr_bits = (1 << ok_bits) - 1;
					BitMask { set_bits: 0, clr_bits }.into()
				}
			}
		}
	}
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

// If a variable is not present, it has not gained a value yet (it is a bottom value).
pub type StaticState = HashMap<TypedSsaVar, StaticValue>;

fn print_static_state(state: &StaticState) {
	let mut vars = state.iter().map(|(var, val)| (*var, *val)).collect::<Vec<_>>();
	vars.sort_by_key(|(var, _)| var.0);

	print!("{{ ");
	for (var, val) in vars {
		print!("({} {:?}): ", var.0, var.1);
		match val {
			StaticValue::Mask(msk) => print!("{{ set: {:#X}, clr: {:#X} }}", msk.set_bits, msk.clr_bits),
			StaticValue::Constant(TypedValue::I32(c)) => {
				assert_eq!(var.ty(), ValType::I32);
				print!("{c}");
			},
			StaticValue::Constant(TypedValue::I64(c)) => {
				assert_eq!(var.ty(), ValType::I64);
				print!("{c}");
			}
		}
		print!(", ");
	}
	println!("}}");
}

fn merge_var(state: &mut StaticState, rhs_var: TypedSsaVar, rhs_value: StaticValue) {
	//let lhs_value = state.get(&rhs_var).copied().unwrap_or(StaticValue::Mask(BitMask { set_bits: 0, clr_bits: 0 }));

	if let Some(lhs_value) = state.get_mut(&rhs_var) {
		let new_lhs: StaticValue = match (*lhs_value, rhs_value) {
			(StaticValue::Mask(lhs_msk), StaticValue::Mask(rhs_msk)) => {
				let set_bits = lhs_msk.set_bits & rhs_msk.clr_bits;
				let clr_bits = lhs_msk.clr_bits & rhs_msk.clr_bits;
				BitMask { set_bits, clr_bits }.into()
			}
			(StaticValue::Constant(cst), StaticValue::Mask(msk)) |
			(StaticValue::Mask(msk), StaticValue::Constant(cst)) => {
				let c = match cst {
					TypedValue::I32(c) => c as u32 as u64,
					TypedValue::I64(c) => c as u64,
				};

				BitMask { set_bits: msk.set_bits & c, clr_bits: msk.clr_bits & !c }.into()
			}
			(StaticValue::Constant(c1), StaticValue::Constant(c2)) if c1 == c2 => c1.into(),
			(StaticValue::Constant(cst1), StaticValue::Constant(cst2)) => {
				let c1 = match cst1 {
					TypedValue::I32(c) => c as u32 as u64,
					TypedValue::I64(c) => c as u64,
				};
				let c2 = match cst2 {
					TypedValue::I32(c) => c as u32 as u64,
					TypedValue::I64(c) => c as u64,
				};

				BitMask { set_bits: c1 & c2, clr_bits: !c1 & !c2 }.into()
			}
		};

		*lhs_value = new_lhs;
	} else {
		//println!("{:?} has no value, setting const", rhs_var);

		state.insert(rhs_var, rhs_value);
	}
}

pub fn state_matches(state: StaticValue, value: TypedValue) -> bool {
	match state {
		StaticValue::Mask(msk) => {
			let c = match value {
				TypedValue::I32(v) => v as u32 as u64,
				TypedValue::I64(v) => v as u64,
			};

			(msk.set_bits & c == msk.set_bits) && (msk.clr_bits & !c == msk.clr_bits)
		}
		StaticValue::Constant(cst) => cst == value,
	}
}

fn merge_state(state: &mut StaticState, other: &StaticState) {
	for (rhs_var, rhs_value) in other.iter() {
		merge_var(state, *rhs_var, *rhs_value);
	}
}

fn print_state_diff(before: &StaticState, after: &StaticState) {
	let keys = before.keys().chain(after.keys()).collect::<std::collections::HashSet<_>>();
	for key in keys {
		match (before.get(key), after.get(key)) {
			(Some(b), Some(a)) => {
				if b != a {
					println!("changed {:?}: {:?} => {:?}", key, b, a);
				}
			}
			(None, Some(a)) => {
				println!("added {:?}: {:?}", key, a);
			}
			(Some(_), None) => {
				println!("removed {:?}", key);
			}
			(None, None) => unreachable!(),
		}
	}
}

pub fn get_func_constants(func: &SsaFunction) -> HashMap<BlockId, StaticState> {
	println!("started const prop for function {}", func.func_id);

	let pred_info = PredInfo::new(func);

	let dom_tree = DomTree::analyze(func);
	let mut reverse_postorder = dom_tree.get_postorder();
	reverse_postorder.reverse();

	let mut states = HashMap::<BlockId, StaticState>::new();

	let all_vars = NoopLivenessInfo::analyze(func).vars;

	{
		let mut start_state = StaticState::new();
		for var in all_vars.iter() {
			start_state.insert(*var, StaticValue::unknown());
		}
		states.insert(func.entry_point_id(), start_state);
	}

	let empty_state = StaticState::new();

	let mut changed = true;
	while changed {
		changed = false;

		//println!("\nNew iteration");

		for node in reverse_postorder.iter() {
			let mut entry_state = states.entry(*node).or_insert_with(StaticState::new).clone();

			let preds = pred_info.get_predecessors(*node);
			for pred in preds {
				let pred_block = func.get(*pred);

				let this_block = func.get(*node);

				let pred_state = states.get(pred).unwrap_or(&empty_state);

				for (var, val) in pred_state.iter() {
					merge_var(&mut entry_state, *var, *val);
				}

				/*match &pred_block.term {
					super::SsaTerminator::ScheduleJump(t, _) |
					super::SsaTerminator::Jump(t) => {
						for (dst, src) in this_block.params.iter().zip(t.params.iter()) {
							if let Some(src) = pred_state.get(src).copied() {
								merge_var(&mut entry_state, *dst, src);
							}
						}
					}
					super::SsaTerminator::BranchIf { cond: _, true_target, false_target } => {
						if true_target.label == *node {
							for (dst, src) in this_block.params.iter().zip(true_target.params.iter()) {
								if let Some(src) = pred_state.get(src).copied() {
									merge_var(&mut entry_state, *dst, src);
								}
							}
						}
						if false_target.label == *node {
							for (dst, src) in this_block.params.iter().zip(false_target.params.iter()) {
								if let Some(src) = pred_state.get(src).copied() {
									merge_var(&mut entry_state, *dst, src);
								}
							}
						}
					}
					super::SsaTerminator::BranchTable { cond: _, default, arms } => {
						if default.label == *node {
							for (dst, src) in this_block.params.iter().zip(default.params.iter()) {
								if let Some(src) = pred_state.get(src).copied() {
									merge_var(&mut entry_state, *dst, src);
								}
							}
						}

						for arm in arms.iter() {
							if arm.label == *node {
								for (dst, src) in this_block.params.iter().zip(arm.params.iter()) {
									if let Some(src) = pred_state.get(src).copied() {
										merge_var(&mut entry_state, *dst, src);
									}
								}
							}
						}
					},
					super::SsaTerminator::Return(_) |
					super::SsaTerminator::Unreachable => unreachable!(),
				}*/
			}

			let block = func.get(*node);

			do_block_const_prop_from(*node, block, &mut entry_state);
			let new_exit_state = entry_state;


			let old_exit_state = states.get(node).unwrap();
			if old_exit_state != &new_exit_state {
				changed = true;
				//println!("Block {:?} changed", node);
				//print_state_diff(old_exit_state, &new_exit_state);
				states.insert(*node, new_exit_state);
			} else {
				//println!("Block {:?} did not change", node);
			}
		}
	}

	/*let mut sorted = state.iter().map(|(id, st)| (*id, *st)).collect::<Vec<_>>();
	sorted.sort_by_key(|(bl, _)| bl.0);

	let mut h = hashers::fnv::FNV1aHasher32::default();
	sorted.hash(&mut h);
	let hash = h.finish();

	println!("ABC const prop, hash is {:?}", hash);

	println!("THE DATA:");
	for (id, thing) in sorted.iter() {
		println!("ABC = {:?} {:?}", id, thing);
	}*/

	states
}

pub fn do_func_const_prop(func: &mut SsaFunction) -> HashMap<BlockId, StaticState> {
	let states = get_func_constants(func);

	for (block_id, block) in func.iter_mut() {
		if let Some(state) = states.get(&block_id) {
			do_strength_reduction(block, state);
		}
	}

	println!("finished const prop for function {}", func.func_id);

	states
}

pub fn do_block_const_prop(id: BlockId, block: &mut SsaBasicBlock) -> StaticState {
	let mut constants = StaticState::new();

	do_block_const_prop_from(id, block, &mut constants);

	constants
}

pub fn do_block_const_prop_from(_id: BlockId, block: &SsaBasicBlock, constants: &mut StaticState) {
	let get_sv = |voc: SsaVarOrConst, constants: &HashMap<TypedSsaVar, StaticValue>| -> Option<StaticValue> {
		match voc {
			SsaVarOrConst::Var(v) => constants.get(&v).copied(),
			SsaVarOrConst::Const(c) => Some(StaticValue::Constant(c)),
		}
	};

	for instr in block.body.iter() {
		match *instr {
			SsaInstr::GlobalGet(dst, _) => {
				// TODO: FIXME: HACK:
				// THIS IS AN AWFUL HACK
				constants.insert(dst, BitMask { set_bits: 0, clr_bits: 0b1111 }.into());
			}
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
				constants.insert(dst, src.into());
			}
			SsaInstr::I64Set(dst, src) => {
				constants.insert(dst, src.into());
			}
			SsaInstr::Assign(dst, src) => {
				if let Some(sv) = get_sv(src, constants) {
					constants.insert(dst, sv);
				}
			}
			SsaInstr::Select { dst, true_var, false_var, cond: _ } => {
				let true_val = get_sv(true_var, constants).unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());
				let false_val = get_sv(false_var, constants).unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());

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
				let rhs_val = get_sv(rhs, constants);

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
								constants.insert(dst, (l & r).into());
							}
							(TypedValue::I64(l), TypedValue::I64(r)) => {
								constants.insert(dst, (l & r).into());
							}
							_ => panic!(),
						};
					}
				}
			}
			SsaInstr::Or(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied();
				let rhs_val = get_sv(rhs, constants);

				match (lhs_val, rhs_val) {
					(None, None) => {}
					(Some(StaticValue::Mask(v1)), Some(StaticValue::Mask(v2))) => {
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
						constants.insert(dst, BitMask { set_bits: msk.set_bits | c, clr_bits: msk.clr_bits & !c }.into());
					}
					(Some(StaticValue::Constant(cst1)), Some(StaticValue::Constant(cst2))) => {
						match (cst1, cst2) {
							(TypedValue::I32(c1), TypedValue::I32(c2)) => {
								constants.insert(dst, (c1 | c2).into());
							}
							(TypedValue::I64(c1), TypedValue::I64(c2)) => {
								constants.insert(dst, (c1 | c2).into());
							}
							_ => panic!(),
						}
					}
				}
			}
			SsaInstr::Shl(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied().unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());
				let rhs_val = get_sv(rhs, constants).unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());

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
					(StaticValue::Constant(cst1), StaticValue::Constant(cst2)) => {
						match (cst1, cst2) {
							(TypedValue::I32(c1), TypedValue::I32(c2)) => {
								constants.insert(dst, (c1 | c2).into());
							}
							(TypedValue::I64(c1), TypedValue::I64(c2)) => {
								constants.insert(dst, (c1 | c2).into());
							}
							_ => panic!(),
						}
					}
				}
			}
			SsaInstr::ShrS(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied().unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());
				let rhs_val = get_sv(rhs, constants).unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());

				match (lhs_val, rhs_val) {
					(StaticValue::Mask(msk), StaticValue::Constant(cst)) => {
						let is_ok_64 = lhs.ty() == ValType::I64 && msk.clr_bits & (1 << 63) != 0;
						let is_ok_32 = lhs.ty() == ValType::I32 && msk.clr_bits & (1 << 31) != 0;
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
					(StaticValue::Constant(cst1), StaticValue::Constant(cst2)) => {
						match (cst1, cst2) {
							(TypedValue::I32(c1), TypedValue::I32(c2)) => {
								constants.insert(dst, (c1 >> (c2 % 32)).into());
							}
							(TypedValue::I64(c1), TypedValue::I64(c2)) => {
								constants.insert(dst, (c1 >> (c2 % 64)).into());
							}
							_ => panic!(),
						}
					}
				}
			}
			SsaInstr::ShrU(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied().unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());
				let rhs_val = get_sv(rhs, constants).unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());

				match (lhs_val, rhs_val) {
					(_, StaticValue::Mask(_)) => {}
					(StaticValue::Mask(msk), StaticValue::Constant(cst)) => {
						let c = match cst {
							TypedValue::I32(c) => (c as u32 as u64) % 32,
							TypedValue::I64(c) => (c as u64) % 64,
						};

						let mut dst_mask = msk;
						dst_mask.set_bits >>= c;
						dst_mask.clr_bits >>= c;
						for i in 0..c {
							dst_mask.clr_bits |= 1 << (31 - i)
						}
						constants.insert(dst, dst_mask.into());
					}
					(StaticValue::Constant(cst1), StaticValue::Constant(cst2)) => {
						match (cst1, cst2) {
							(TypedValue::I32(lhs), TypedValue::I32(rhs)) => {
								let rhs = rhs % 32;
								constants.insert(dst, (((lhs as u32) >> rhs) as i32).into());
							}
							(TypedValue::I64(lhs), TypedValue::I64(rhs)) => {
								let rhs = rhs % 64;
								constants.insert(dst, (((lhs as u64) >> rhs) as i64).into());
							}
							_ => panic!(),
						}
					}
				}
			}
			SsaInstr::Add(dst, lhs, rhs) => {
				let lhs_val = get_sv(lhs, constants);
				let rhs_val = get_sv(rhs, constants);

				if let (Some(lhs_val), Some(rhs_val)) = (lhs_val, rhs_val) {
					match (lhs_val, rhs_val) {
						(StaticValue::Constant(l), StaticValue::Constant(r)) => {
							match (l, r) {
								(TypedValue::I32(l), TypedValue::I32(r)) => {
									let d = l.wrapping_add(r);
									constants.insert(dst, d.into());
								}
								(TypedValue::I64(l), TypedValue::I64(r)) => {
									let d = l.wrapping_add(r);
									constants.insert(dst, d.into());
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
								if ok_bits >= 30 {
									todo!()
								}
								let clr_bits = (1 << ok_bits) - 1;
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
								if ok_bits >= 30 {
									todo!()
								}
								let clr_bits = (1 << ok_bits) - 1;
								constants.insert(dst, BitMask { set_bits: 0, clr_bits }.into());
							}
						}
					}
				}
			}
			_ => {
				for dst in instr.defs() {
					constants.insert(dst, BitMask { set_bits: 0, clr_bits: 0 }.into());
				}
			}
		}
	}
}

pub fn do_strength_reduction(block: &mut SsaBasicBlock, constants: &StaticState) {
	let get_sv = |voc: SsaVarOrConst, constants: &HashMap<TypedSsaVar, StaticValue>| -> Option<StaticValue> {
		match voc {
			SsaVarOrConst::Var(v) => constants.get(&v).copied(),
			SsaVarOrConst::Const(c) => Some(StaticValue::Constant(c)),
		}
	};

	for instr in block.body.iter_mut() {
		let args = instr.constable_vars();

		for arg in args {
			if let SsaVarOrConst::Var(v) = arg {
				if let Some(StaticValue::Constant(a)) = constants.get(v) {
					*arg = (*a).into();
				}
			}
		}
		
		match *instr {
			SsaInstr::DivU(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied();
				let rhs_val = get_sv(rhs, constants);

				let mut ok = false;

				if let Some(StaticValue::Constant(TypedValue::I64(cst_rhs))) = rhs_val {
					if cst_rhs.count_ones() == 1 {
						let new_rhs = SsaVarOrConst::Const(TypedValue::I64(cst_rhs.trailing_zeros() as i64));

						*instr = SsaInstr::ShrU(dst, lhs, new_rhs);
						ok = true;
					}
				}

				if !ok {
					if let (Some(lhs_val), Some(rhs_val)) = (lhs_val, rhs_val) {
						let lhs_is_pos = match lhs_val {
							StaticValue::Mask(msk) if lhs.ty() == ValType::I32 => msk.clr_bits & (1 << 31) != 0,
							StaticValue::Mask(msk) if lhs.ty() == ValType::I64 => msk.clr_bits & (1 << 63) != 0,
							StaticValue::Constant(TypedValue::I32(c)) => c >= 0,
							StaticValue::Constant(TypedValue::I64(c)) => c >= 0,
							_ => panic!(),
						};

						let rhs_is_pos = match rhs_val {
							StaticValue::Mask(msk) if rhs.ty() == ValType::I32 => msk.clr_bits & (1 << 31) != 0,
							StaticValue::Mask(msk) if rhs.ty() == ValType::I64 => msk.clr_bits & (1 << 63) != 0,
							StaticValue::Constant(TypedValue::I32(c)) => c >= 0,
							StaticValue::Constant(TypedValue::I64(c)) => c >= 0,
							_ => panic!(),
						};

						if lhs_is_pos && rhs_is_pos {
							*instr = SsaInstr::DivS(dst, lhs, rhs);
						}
					}
				}
			}
			SsaInstr::And(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied();
				let rhs_val = get_sv(rhs, constants);

				if let (Some(StaticValue::Constant(l)), Some(StaticValue::Constant(r))) = (lhs_val, rhs_val) {
					match (l, r) {
						(TypedValue::I32(l), TypedValue::I32(r)) => {
							*instr = SsaInstr::I32Set(dst, l & r);
						}
						(TypedValue::I64(l), TypedValue::I64(r)) => {
							*instr = SsaInstr::I64Set(dst, l & r);
						}
						_ => panic!(),
					};
				}
			}
			SsaInstr::Or(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied();
				let rhs_val = get_sv(rhs, constants);

				match (lhs_val, rhs_val) {
					(Some(StaticValue::Mask(v1)), Some(StaticValue::Mask(v2))) => {
						let v1_maybe_set = !v1.clr_bits;
						let v2_maybe_set = !v2.clr_bits;
						if v1_maybe_set & v2_maybe_set == 0 {
							*instr = SsaInstr::Add(dst, lhs.into(), rhs);
						}
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
					}
					(Some(StaticValue::Constant(cst1)), Some(StaticValue::Constant(cst2))) => {
						match (cst1, cst2) {
							(TypedValue::I32(c1), TypedValue::I32(c2)) => {
								*instr = SsaInstr::I32Set(dst, c1 | c2);
							}
							(TypedValue::I64(c1), TypedValue::I64(c2)) => {
								*instr = SsaInstr::I64Set(dst, c1 | c2);
							}
							_ => panic!(),
						}
					}
					_ => {}
				}
			}
			SsaInstr::ShrU(dst, lhs, rhs) => {
				let lhs_val = constants.get(&lhs).copied().unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());
				let rhs_val = get_sv(rhs, constants).unwrap_or_else(|| BitMask { set_bits: 0, clr_bits: 0 }.into());

				match (lhs_val, rhs_val) {
					(StaticValue::Constant(cst), StaticValue::Mask(_)) => {
						match cst {
							TypedValue::I32(c) => if c >= 0 { *instr = SsaInstr::ShrS(dst, lhs, rhs); },
							TypedValue::I64(c) => if c >= 0 { *instr = SsaInstr::ShrS(dst, lhs, rhs); },
						};
					}
					(StaticValue::Mask(msk), _) => {
						let is_ok_64 = lhs.ty() == ValType::I64 && msk.clr_bits & (1 << 63) != 0;
						let is_ok_32 = lhs.ty() == ValType::I32 && msk.clr_bits & (1 << 31) != 0;

						if is_ok_64 || is_ok_32 {
							*instr = SsaInstr::ShrS(dst, lhs, rhs);
						}
					}
					(StaticValue::Constant(cst1), StaticValue::Constant(cst2)) => {
						match (cst1, cst2) {
							(TypedValue::I32(c1), TypedValue::I32(c2)) => {
								*instr = SsaInstr::I32Set(dst, (c1 as u32 >> c2) as i32);
							}
							(TypedValue::I64(c1), TypedValue::I64(c2)) => {
								*instr = SsaInstr::I64Set(dst, (c1 as u64 >> c2) as i64);
							}
							_ => panic!(),
						}

					}
				}
			}
			SsaInstr::Add(dst, lhs, rhs) => {
				let lhs_val = get_sv(lhs, constants);
				let rhs_val = get_sv(rhs, constants);

				if let (Some(lhs_val), Some(rhs_val)) = (lhs_val, rhs_val) {
					if let (StaticValue::Constant(l), StaticValue::Constant(r)) = (lhs_val, rhs_val) {
						match (l, r) {
							(TypedValue::I32(l), TypedValue::I32(r)) => {
								let d = l.wrapping_add(r);
								*instr = SsaInstr::I32Set(dst, d);
							}
							(TypedValue::I64(l), TypedValue::I64(r)) => {
								let d = l.wrapping_add(r);
								*instr = SsaInstr::I64Set(dst, d);
							}
							_ => panic!(),
						}
					}
				}
			}
			_ => {}
		}
	}
}