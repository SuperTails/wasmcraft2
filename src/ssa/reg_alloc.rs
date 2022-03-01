use std::collections::HashSet;

use crate::{lir::{Register, DoubleRegister}, ssa::liveness::print_live_ranges};

use super::{SsaFunction, SsaVar, liveness::{NoopLivenessInfo, LivenessInfo, FullLivenessInfo}, TypedSsaVar, BlockId};

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

mod register_set {
	use crate::ssa::liveness::LiveRange;

	use super::*;

	#[derive(Debug)]
	pub struct MergedRegister {
		pub members: HashSet<TypedSsaVar>,
		pub live_range: LiveRange,
	}

	#[derive(Debug)]
	pub struct RegisterSet(Vec<MergedRegister>);
	
	impl RegisterSet {
		pub fn new(func: &SsaFunction) -> Self {
			let all_vars = NoopLivenessInfo::analyze(func).vars;

			let live_info = FullLivenessInfo::analyze(func);

			//crate::ssa::liveness::print_liveness_info(&live_info, func);

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
			merged1.live_range.merge(merged2.live_range);

			self.0.push(merged1);
		}

		pub fn get_idx(&self, var1: TypedSsaVar) -> usize {
			self.0.iter().enumerate().find(|(_, reg)| reg.members.contains(&var1)).unwrap().0
		}

		pub fn get_idx2(&self, var1: SsaVar) -> usize {
			self.0.iter().enumerate().find(|(_, reg)| reg.members.iter().any(|r| r.0 == var1.0)).unwrap().0
		}

		pub fn get_rep(&self, var1: SsaVar) -> TypedSsaVar {
			let idx = self.get_idx2(var1);
			*self.0[idx].members.iter().next().unwrap()
		}
	}
}

use register_set::*;

fn try_merge(sets: &mut RegisterSet, block_id: BlockId, instr_idx: usize, dst: &TypedSsaVar, src: &TypedSsaVar, func: &SsaFunction) {
	if sets.get_idx(*dst) == sets.get_idx(*src) {
		return;
	}

	let dst_set = sets.get(*dst);
	let src_set = sets.get(*src);

	//println!();
	//println!("{:?} {:?}", dst, src);
	//println!("Members: {:?}", dst_set.members);
	//println!("         {:?}", src_set.members);

	//print_live_ranges(&[dst_set.live_range.clone(), src_set.live_range.clone()], func);

	let overlap = dst_set.live_range.overlap(&src_set.live_range);

	if let Some((_overlap_id, _overlap_instr)) = overlap.get_single_point() {
		//assert_eq!(overlap_id, block_id);
		//assert_eq!(overlap_instr, instr_idx);

		sets.merge(*dst, *src);
	}
}

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

					try_merge(&mut sets, block_id, instr_idx, dst, src, func);
				}
			}

			for (dst, src) in func.coalescable_term_vars(block_id) {
				try_merge(&mut sets, block_id, block.body.len(), &dst, &src, func);
			}
		}

		FullRegAlloc { const_pool: HashSet::new(), set: sets, func: func.func_id(), temp: 1000 }
	}

	fn get(&self, val: SsaVar) -> Register {
		let rep = self.set.get_rep(val).0;
		Register::work_lo(self.func, rep as u32)
	}

	fn get_double(&self, val: SsaVar) -> DoubleRegister {
		let rep = self.set.get_rep(val).0;
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

#[cfg(test)]
mod test {
	use wasmparser::Type;

	use crate::ssa::{SsaBasicBlock, TypedSsaVar, SsaInstr, SsaTerminator, JumpTarget, BlockId, SsaFunction, liveness::{FullLivenessInfo, LivenessInfo, print_liveness_info}};

use super::{FullRegAlloc, RegAlloc};

	/*#[test]
	fn reg_across_jump() {
		let r0 = TypedSsaVar(0, Type::I32);
		let r1 = TypedSsaVar(1, Type::I32);

		let b0 = SsaBasicBlock {
			// TODO: ???
			params: Vec::new(),
			body: vec![
				SsaInstr::I32Set(r0, 123),
			],
			term: SsaTerminator::Jump(JumpTarget {
				label: BlockId { func: 0, block: 1 },
				params: vec![r0],
			})
		};
	}*/

	#[test]
	#[ignore]
	fn coalesce_jump_param() {
		let r0 = TypedSsaVar(0, Type::I32);
		let r1 = TypedSsaVar(1, Type::I32);

		let b0 = SsaBasicBlock {
			params: Vec::new(),
			body: vec![
				SsaInstr::I32Set(r0, 123)
			],
			term: SsaTerminator::Jump(JumpTarget {
				label: BlockId { func: 0, block: 1},
				params: vec![r0]
			})
		};

		let b1 = SsaBasicBlock {
			params: vec![r1],
			body: Vec::new(),
			term: SsaTerminator::Return(vec![r1]),
		};

		let func = SsaFunction {
			code: vec![
				(BlockId { func: 0, block: 0 }, b0),
				(BlockId { func: 0, block: 1 }, b1),
			],
			params: Box::new([]),
			returns: Box::new([Type::I32])
		};

		let reg_alloc = FullRegAlloc::analyze(&func);

		println!("{:?}", reg_alloc.set);
		panic!();
		
		/*let liveness = FullLivenessInfo::analyze(&func);

		let r0_range = liveness.live_range(r0);
		let r1_range = liveness.live_range(r1);

		print_liveness_info(&liveness, &func);
		println!();

		println!("{:?}\n", r0_range);
		println!("{:?}\n", r1_range);
		println!("{:?}\n", overlap);
		panic!();*/
	}
}