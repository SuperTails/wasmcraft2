use std::collections::HashSet;

use crate::{lir::{Register, DoubleRegister}, ssa::liveness::print_live_ranges};

use super::{SsaFunction, SsaVar, liveness::{NoopLivenessInfo, LivenessInfo, FullLivenessInfo}, TypedSsaVar, BlockId};

pub trait RegAlloc {
	fn analyze(ssa_func: &SsaFunction) -> Self;

	fn get(&self, val: SsaVar) -> Register;

	fn get_double(&self, val: SsaVar) -> DoubleRegister;

	fn get_const(&mut self, val: i32) -> Register;

	fn get_temp(&mut self) -> Register;

	fn get_temp_double(&mut self) -> DoubleRegister;
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

	fn get_temp_double(&mut self) -> DoubleRegister {
		let reg = DoubleRegister::temp(self.temp);
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
	use std::collections::HashMap;

use wasmparser::Type;

use crate::ssa::{liveness::LiveRange, SsaTerminator, SsaInstr};

	use super::*;

	#[derive(Debug)]
	pub struct InterfGraph(HashMap<TypedSsaVar, Vec<TypedSsaVar>>);

	impl InterfGraph {
		pub fn new(func: &SsaFunction) -> Self {
			let mut result = InterfGraph(HashMap::new());

			for (_, block) in func.iter() {
				for instr in block.body.iter() {
					match instr {
						SsaInstr::Add(dst, lhs, rhs) if dst.ty() == Type::I64 => {
							if let Some(l) = lhs.get_var() {
								result.add_edge(*dst, l)
							}
							if let Some(r) = rhs.get_var() {
								result.add_edge(*dst, r);
							}
							if let (Some(l), Some(r)) = (lhs.get_var(), rhs.get_var()) {
								result.add_edge(l, r);
							}
						}
						_ => {}
					}
				}
			}

			result
		}

		pub fn add_edge(&mut self, a: TypedSsaVar, b: TypedSsaVar) {
			self.add_dir_edge(a, b);
			self.add_dir_edge(b, a);
		}

		fn add_dir_edge(&mut self, a: TypedSsaVar, b: TypedSsaVar) {
			let others = self.0.entry(a).or_insert_with(Vec::new);
			if !others.contains(&b) {
				others.push(b);
			}
		}

		pub fn interferes(&self, lhs: &MergedRegister, rhs: &MergedRegister) -> bool {
			for lhs_reg in lhs.members.iter() {
				if let Some(interf_regs) = self.0.get(lhs_reg) {
					for rhs_reg in interf_regs.iter() {
						if rhs.members.contains(rhs_reg) {
							return true;
						}
					}
				}
			}

			false
		}
	}

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

fn try_merge(sets: &mut RegisterSet, block_id: BlockId, instr_idx: usize, dst: &TypedSsaVar, src: &TypedSsaVar, func: &SsaFunction, interf_graph: &InterfGraph) {
	if sets.get_idx(*dst) == sets.get_idx(*src) {
		return;
	}

	let dst_set = sets.get(*dst);
	let src_set = sets.get(*src);

	if interf_graph.interferes(dst_set, src_set) {
		return;
	}

	let overlap = dst_set.live_range.overlap(&src_set.live_range);

	if let Some((_overlap_id, _overlap_instr)) = overlap.get_single_point() {
		//assert_eq!(overlap_id, block_id);
		//assert_eq!(overlap_instr, instr_idx);

		println!("merging {:?} {:?}", dst, src);

		sets.merge(*dst, *src);
	} /*else if block_id.func == 9 && block_id.block == 13 {
		println!();
		println!("{:?} {:?}", dst, src);
		println!("Members: {:?}", dst_set.members);
		println!("         {:?}", src_set.members);

		println!("DST LIVE RANGE:");
		for part in dst_set.live_range.0.iter() {
			if !part.1.is_empty() {
				println!("{:?}", part);
			}
		}
		println!();

		println!("SRC LIVE RANGE:");
		for part in src_set.live_range.0.iter() {
			if !part.1.is_empty() {
				println!("{:?}", part);
			}
		}
		println!();

		//print_live_ranges(&[dst_set.live_range.clone(), src_set.live_range.clone()], func);

		println!("OVERLAP:");
		for (block_id, block) in overlap.0.iter() {
			if !block.is_empty() {
				println!("{:?} {:?}", block_id, block);

				println!("--- block ----");

				let b = func.get(*block_id);

				println!("    {:?}", b.params);
				for instr in b.body.iter() {
					println!("    {:?}", instr);
				}

				println!("    {:?}", b.term);

				for succ in b.term.successors() {
					let s = func.get(succ);
					println!();
					println!("Succ params: {:?}", s.params);
				}
			}
		}
		println!();

		println!("===== The source func =====");

		let b = func.get(block_id);
		println!("--- block ----");

		println!("    {:?}", b.params);
		for (i, instr) in b.body.iter().enumerate() {
			if i == instr_idx {
				println!("+   {:?}", instr);
			} else {
				println!("    {:?}", instr);
			}
		}

		println!("    {:?}", b.term);

		for succ in b.term.successors() {
			let s = func.get(succ);
			println!();
			println!("Succ params: {:?}", s.params);
		}

		panic!();
	}*/

}

impl RegAlloc for FullRegAlloc {
	fn analyze(func: &SsaFunction) -> Self {
		let interf_graph = InterfGraph::new(func);

		println!("Interference graph: {:?}", interf_graph);

		let mut sets = RegisterSet::new(func);

		for (block_id, block) in func.iter() {
			for (instr_idx, instr) in block.body.iter().enumerate() {
				for (dst, src) in instr.coalescable_vars() {
					let uses = instr.uses();
					let defs = instr.defs();
					assert!(uses.contains(src));
					assert!(defs.contains(dst));

					try_merge(&mut sets, block_id, instr_idx, dst, src, func, &interf_graph);
				}
			}

			for (dst, src) in func.coalescable_term_vars(block_id) {
				try_merge(&mut sets, block_id, block.body.len(), &dst, &src, func, &interf_graph);
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

	fn get_temp_double(&mut self) -> DoubleRegister {
		let reg = DoubleRegister::temp(self.temp);
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