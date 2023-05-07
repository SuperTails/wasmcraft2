use std::collections::{HashSet, HashMap};

use crate::{lir::{Register, DoubleRegister, DynRegister}, ssa::liveness::print_live_ranges, graph::Graph};

use super::{SsaFunction, SsaVar, liveness::{NoopLivenessInfo, FullLivenessInfo}, TypedSsaVar, BlockId};

pub trait RegAlloc {
	//fn analyze(ssa_func: &SsaFunction) -> Self;

	fn get(&self, val: SsaVar) -> Register;

	fn get_double(&self, val: SsaVar) -> DoubleRegister;

	fn get_const(&mut self, val: i32) -> Register;

	fn get_double_const(&mut self, val: i64) -> DoubleRegister;

	fn get_temp(&mut self) -> Register;

	fn get_temp_double(&mut self) -> DoubleRegister;

	fn get_typed(&self, var: TypedSsaVar) -> DynRegister {
		match var.ty() {
			wasmparser::ValType::I32 => self.get(var.into_untyped()).into(),
			wasmparser::ValType::I64 => self.get_double(var.into_untyped()).into(),
			wasmparser::ValType::F32 => todo!(),
			wasmparser::ValType::F64 => todo!(),
			_ => unreachable!(),
		}
	}

	fn const_pool(&self) -> &HashSet<i32>;
}

pub struct NoopRegAlloc {
	pub const_pool: HashSet<i32>,
	func: u32,
	temp: u32,
}

impl NoopRegAlloc {
	pub fn analyze(func: &SsaFunction) -> Self {
		NoopRegAlloc { const_pool: HashSet::new(), func: func.iter().next().unwrap().0.func as u32, temp: 1000 }
	}
}

impl RegAlloc for NoopRegAlloc {
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

	fn get_double_const(&mut self, val: i64) -> DoubleRegister {
		self.const_pool.insert(val as i32);
		self.const_pool.insert((val >> 32) as i32);
		DoubleRegister::const_val(val)
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

	fn const_pool(&self) -> &HashSet<i32> {
		&self.const_pool
	}
}

pub struct FullRegAlloc {
	pub const_pool: HashSet<i32>,
	pub map: HashMap<SsaVar, u32>,
	func: u32,
	temp: u32,
}

mod register_set {
	use std::collections::{HashMap, BTreeSet};

	use crate::ssa::{liveness::LiveRange};

	use super::*;

	#[derive(Debug)]
	pub struct InterfGraph(HashMap<TypedSsaVar, Vec<TypedSsaVar>>);

	impl InterfGraph {
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
		pub members: BTreeSet<TypedSsaVar>,
	}

	#[derive(Debug)]
	pub struct RegisterSet(Vec<MergedRegister>);
	
	impl RegisterSet {
		pub fn new(func: &SsaFunction) -> Self {
			let all_vars = NoopLivenessInfo::analyze(func).vars;

			let live_info = FullLivenessInfo::analyze(func);

			//crate::ssa::liveness::print_liveness_info(&live_info, func);

			println!("Need to create {} registers in set", all_vars.len());

			let regs = all_vars.into_iter().map(|var| {
				let members = Some(var).into_iter().collect();
				MergedRegister { members }
			}).collect();

			RegisterSet(regs)
		}

		pub fn len(&self) -> usize {
			self.0.len()
		}

		pub fn to_map(&self) -> HashMap<SsaVar, u32> {
			let mut result = HashMap::new();
			for merged_reg in self.0.iter() {
				let rep = merged_reg.members.iter().next().unwrap();
				for reg in merged_reg.members.iter().copied() {
					result.insert(reg.into_untyped(), rep.0);
				}
			}
			result
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

fn try_merge(sets: &mut RegisterSet, dst: &TypedSsaVar, src: &TypedSsaVar, interf_graph: &Graph) {
	if sets.get_idx(*dst) == sets.get_idx(*src) {
		return;
	}

	let dst_set = sets.get(*dst);
	let src_set = sets.get(*src);

	for m1 in &dst_set.members {
		for m2 in &src_set.members {
			if interf_graph.interferes(m1.into_untyped(), m2.into_untyped()) {
				return;
			}
		}
	}

	sets.merge(*dst, *src);
}

impl FullRegAlloc {
	pub fn analyze(func: &SsaFunction, liveness: &FullLivenessInfo) -> Self {
		println!("Starting regalloc for {}", func.func_id());

		let interf_graph = Graph::new(func, liveness);

		let mut sets = RegisterSet::new(func);

		println!("Register sets created for {}", func.func_id());

		/*for (block_id, block) in func.iter() {
			for (instr_idx, instr) in block.body.iter().enumerate() {
				for (dst, src) in instr.coalescable_vars() {
					let uses = instr.uses();
					let defs = instr.defs();
					assert!(uses.contains(src));
					assert!(defs.contains(dst));

					try_merge(&mut sets, dst, src, &interf_graph);
				}
			}

			/*for (dst, src) in func.coalescable_term_vars(block_id) {
				try_merge(&mut sets, &dst, &src, &interf_graph);
			}*/
		}

		println!("Coalesced into {} registers", sets.len());*/


		FullRegAlloc { const_pool: HashSet::new(), map: sets.to_map(), func: func.func_id(), temp: 1000 }
	}
}

impl RegAlloc for FullRegAlloc {
	fn get(&self, val: SsaVar) -> Register {
		let rep = *self.map.get(&val).unwrap();
		Register::work_lo(self.func, rep)
	}

	fn get_double(&self, val: SsaVar) -> DoubleRegister {
		let rep = *self.map.get(&val).unwrap();
		DoubleRegister::Work(self.func, rep)
	}

	fn get_const(&mut self, val: i32) -> Register {
		self.const_pool.insert(val);
		Register::const_val(val)
	}

	fn get_double_const(&mut self, val: i64) -> DoubleRegister {
		self.const_pool.insert(val as i32);
		self.const_pool.insert((val >> 32) as i32);
		DoubleRegister::const_val(val)
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

	fn const_pool(&self) -> &HashSet<i32> {
		&self.const_pool
	}
}

#[cfg(test)]
mod test {
	use wasmparser::ValType;

	use crate::ssa::{SsaBasicBlock, TypedSsaVar, SsaInstr, SsaTerminator, JumpTarget, BlockId, SsaFunction, liveness::FullLivenessInfo};

	use super::{FullRegAlloc, RegAlloc};

	/*#[test]
	fn reg_across_jump() {
		let r0 = TypedSsaVar(0, ValType::I32);
		let r1 = TypedSsaVar(1, ValType::I32);

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

	/*#[test]
	#[ignore]
	fn coalesce_jump_param() {
		let r0 = TypedSsaVar(0, ValType::I32);
		let r1 = TypedSsaVar(1, ValType::I32);

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

		let func = SsaFunction::new(
			vec![
				(BlockId { func: 0, block: 1 }, b1),
				(BlockId { func: 0, block: 0 }, b0),
			],
			Box::new([]),
			Box::new([ValType::I32]),
		);

		let lv = FullLivenessInfo::analyze(&func);

		let reg_alloc = FullRegAlloc::analyze(&func, &lv);

		println!("{:?}", reg_alloc.map);
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
	}*/
}