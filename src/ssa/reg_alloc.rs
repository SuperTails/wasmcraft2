use std::collections::HashSet;

use crate::lir::{Register, DoubleRegister};

use super::{SsaFunction, SsaVar, liveness::{NoopLivenessInfo, LivenessInfo, FullLivenessInfo}, TypedSsaVar};

pub trait RegAlloc {
	fn analyze(ssa_func: &SsaFunction) -> Self;

	fn get(&self, val: SsaVar) -> Register;

	fn get_double(&self, val: SsaVar) -> DoubleRegister;

	fn get_const(&mut self, val: i32) -> Register;

	fn get_temp(&mut self) -> Register;
}

pub struct NoopRegAlloc {
	pub const_pool: HashSet<i32>,
	func: u32,
	temp: u32,
}

impl RegAlloc for NoopRegAlloc {
	fn analyze(func: &SsaFunction) -> Self {
		NoopRegAlloc { const_pool: HashSet::new(), func: func.iter().next().unwrap().0.func as u32, temp: 1000 }
	}

	fn get(&self, val: SsaVar) -> Register {
		Register::work_lo(self.func, val.0)
	}

	fn get_double(&self, val: SsaVar) -> DoubleRegister {
		DoubleRegister::Work(self.func, val.0)
	}

	fn get_const(&mut self, val: i32) -> Register {
		self.const_pool.insert(val);
		Register::const_val(val)
	}

	fn get_temp(&mut self) -> Register {
		let reg = Register::temp_lo(self.temp);
		self.temp += 1;
		reg
	}
}

pub struct FullRegAlloc {
	pub const_pool: HashSet<i32>,
	pub set: RegisterSet,
	func: u32,
	temp: u32,
}

fn elems_in_set<'a>(all_vars: &'a HashSet<TypedSsaVar>, rep: &'a SsaVar, union_find: &'a UnionFind<SsaVar>) -> impl Iterator<Item=TypedSsaVar> + 'a {
	all_vars.iter().copied().filter(|v| union_find.equiv(*rep, v.into_untyped()))
}

mod register_set {
	use crate::ssa::liveness::LiveRange;

	use super::*;

	pub struct MergedRegister {
		pub members: HashSet<TypedSsaVar>,
		pub live_range: LiveRange,
	}

	pub struct RegisterSet(Vec<MergedRegister>);
	
	impl RegisterSet {
		pub fn new(func: &SsaFunction) -> Self {
			let all_vars = NoopLivenessInfo::analyze(func).vars;

			let live_info = FullLivenessInfo::analyze(func);

			let regs = all_vars.into_iter().map(|var| {
				let members = Some(var).into_iter().collect();
				let live_range = live_info.live_range(var);
				MergedRegister { members, live_range }
			}).collect();

			RegisterSet(regs)
		}

		pub fn get(&self, elem: TypedSsaVar) -> &MergedRegister {
			self.0.iter().find(|reg| reg.members.contains(&elem)).unwrap()
		}

		pub fn merge(&mut self, var1: TypedSsaVar, var2: TypedSsaVar) {
			let r1 = self.get_idx(var1);
			let mut merged1 = self.0.remove(r1);

			let r2 = self.get_idx(var2);
			let merged2 = self.0.remove(r2);

			merged1.members.extend(merged2.members);
			merged1.live_range = merged1.live_range.overlap(&merged2.live_range);

			self.0.push(merged1);
		}

		pub fn get_idx(&self, var1: TypedSsaVar) -> usize {
			self.0.iter().enumerate().find(|(_, reg)| reg.members.contains(&var1)).unwrap().0
		}

		pub fn get_idx2(&self, var1: SsaVar) -> usize {
			self.0.iter().enumerate().find(|(_, reg)| reg.members.iter().any(|r| r.0 == var1.0)).unwrap().0
		}
	}
}

use register_set::*;

impl RegAlloc for FullRegAlloc {
	fn analyze(func: &SsaFunction) -> Self {
		let mut sets = RegisterSet::new(func);

		for (block_id, block) in func.iter() {
			for (instr_idx, instr) in block.body.iter().enumerate() {
				if let Some((dst, src)) = instr.coalescable_vars() {
					let uses = instr.uses();
					let defs = instr.defs();
					assert!(uses.contains(src));
					assert!(defs.contains(dst));

					let dst_set = sets.get(*dst);
					let src_set = sets.get(*src);

					println!("{:?} {:?}", dst, src);

					println!("{} : {:?}", instr_idx, instr);

					println!("DST RANGE {:?}", dst_set.live_range);
					println!("SRC RANGE {:?}", src_set.live_range);

					let overlap = dst_set.live_range.overlap(&src_set.live_range);

					if let Some((overlap_id, overlap_instr)) = overlap.get_single_point() {
						assert_eq!(overlap_id, block_id);
						assert_eq!(overlap_instr, instr_idx);

						sets.merge(*dst, *src);
					}

				}
			}
		}

		FullRegAlloc { const_pool: HashSet::new(), set: sets, func: func.func_id(), temp: 1000 }
	}

	fn get(&self, val: SsaVar) -> Register {
		let rep = self.set.get_idx2(val);
		Register::work_lo(self.func, rep as u32)
	}

	fn get_double(&self, val: SsaVar) -> DoubleRegister {
		let rep = self.set.get_idx2(val);
		DoubleRegister::Work(self.func, rep as u32)
	}

	fn get_const(&mut self, val: i32) -> Register {
		self.const_pool.insert(val);
		Register::const_val(val)
	}

	fn get_temp(&mut self) -> Register {
		let reg = Register::temp_lo(self.temp);
		self.temp += 1;
		reg
	}
}