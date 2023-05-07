use std::{collections::{HashSet, BinaryHeap}, ops::RangeInclusive};

use crate::{block_id_map::LocalBlockMap, set::PairMap};

use super::{TypedSsaVar, SsaFunction, BlockId, SsaBasicBlock, TypedSsaVarSet, SsaInstr, SsaVar};

fn range_overlap(r1: RangeInclusive<usize>, r2: RangeInclusive<usize>) -> Option<RangeInclusive<usize>> {
	let start = *r1.start().max(r2.start());
	let end = *r1.end().min(r2.end());

	if start <= end {
		Some(start..=end)
	} else {
		None
	}
}

fn merge_ranges(a: RangeInclusive<usize>, b: RangeInclusive<usize>) -> Option<RangeInclusive<usize>> {
	if range_overlap(a.clone(), b.clone()).is_some() {
		let start = *a.start().min(b.start());
		let end = *a.end().max(b.end());
		Some(start..=end)
	} else {
		None
	}
}

fn simplify_range_list(list: &mut Vec<RangeInclusive<usize>>) {
	let mut i = 0;
	'outer: while i != list.len() {
		for j in i + 1..list.len() {
			let r1 = list[i].clone();
			let r2 = list[j].clone();

			if let Some(merged) = merge_ranges(r1, r2) {
				list[i] = merged;
				list.remove(j);
				continue 'outer;
			}
		}

		i += 1;
	}
}

#[derive(Debug, Clone)]
pub struct BlockLiveRange {
	/// The set of all predecessor blocks from which this variable is used as an input parameter.
	pub live_in_params: HashSet<BlockId>,
	/// If true, this variable comes from a predecessor block but is not solely an input parameter.
	pub live_in: bool,
	/// The range of instructions for which this variable is in the live_in set.
	/// Thus, this excludes the instruction that creates the variable.
	/// Thus, if this includes index 0, it must be either a parameter's output or come from a predecessor block directly.
	pub body: Vec<RangeInclusive<usize>>,
	/// True if this variable is used in any successor block. This can mean it is either an input parameter or directly used.
	pub live_out: bool,
}

impl BlockLiveRange {
	pub fn merge(&mut self, other: &BlockLiveRange) {
		self.live_in_params.extend(other.live_in_params.iter().copied());

		self.live_in |= other.live_in;

		self.body.extend(other.body.iter().cloned());
		simplify_range_list(&mut self.body);

		self.live_out |= other.live_out;
	}

	pub fn body_contains(&self, idx: usize) -> bool {
		self.body.iter().any(|range| range.contains(&idx))
	}

	/*pub fn is_live_into(&self, instr_idx: usize) -> bool {
		if instr_idx == 0 {
			self.live_in
		} else {
			for range in self.body.iter() {
				if range.contains(&(instr_idx - 1)) {
					return true
				}
			}
			false
		}
	}*/

	pub fn overlap(&self, other: &BlockLiveRange) -> BlockLiveRange {
		let live_in = self.live_in && other.live_in;
		let live_out = self.live_out && other.live_out;

		let live_in_params;
		if !live_in {
			if self.live_in {
				live_in_params = other.live_in_params.clone();
			} else if other.live_in {
				live_in_params = self.live_in_params.clone();
			} else {
				live_in_params = HashSet::new();
			}
		} else {
			live_in_params = self.live_in_params.intersection(&other.live_in_params).copied().collect();
		}

		let mut body = Vec::new();

		for r1 in self.body.iter() {
			for r2 in other.body.iter() {
				if let Some(overlap) = range_overlap(r1.clone(), r2.clone()) {
					body.push(overlap);
				}
			}
		}

		simplify_range_list(&mut body);

		BlockLiveRange { live_in_params, live_in, body, live_out }
	}

	pub fn is_empty(&self) -> bool {
		if self.live_in || self.live_out {
			false
		} else {
			self.body.is_empty() && self.live_in_params.is_empty()
		}
	}
}

#[derive(Debug, Clone)]
pub struct LiveRange(pub LocalBlockMap<BlockLiveRange>);

impl LiveRange {
	pub fn is_empty(&self) -> bool {
		self.0.values().all(|r| r.is_empty())
	}

	pub fn merge(&mut self, other: LiveRange) {
		for (id, range) in other.0 {
			if let Some(r1) = self.0.get_mut(id) {
				r1.merge(&range);
			} else {
				self.0.insert(id, range);
			}
		}
	}

	/*pub fn get_single_point(&self) -> Option<(BlockId, usize)> {
		let mut single_points = self.0.iter().filter_map(|(id, b)| {
			if b.is_empty() || b.live_in || b.live_out || b.body.len() != 1 || b.body[0].clone().count() != 1 {
				None
			} else {
				Some((*id, *b.body[0].start()))
			}
		});

		if let Some(result) = single_points.next() {
			if single_points.next().is_none() {
				Some(result)
			} else {
				None
			}
		} else {
			None
		}
	}*/
}

impl LiveRange {
	pub fn overlap(&self, other: &LiveRange) -> LiveRange {
		let mut result = LocalBlockMap::new(self.0.func_id());

		for (id, r1) in self.0.iter() {
			if let Some(r2) = other.0.get(id) {
				result.insert(id, r1.overlap(r2));
			}
		}

		LiveRange(result)
	}
}

pub struct NoopLivenessInfo {
	pub vars: HashSet<TypedSsaVar>,
}

impl NoopLivenessInfo {
	pub fn analyze(func: &SsaFunction) -> Self {
		let mut vars = HashSet::new();

		for (_, block) in func.iter() {
			vars.extend(block.phi_node.dests.iter());
			for arm in block.phi_node.arms() {
				vars.extend(arm.sources().map(|(_, t)| t));
			}

			for instr in block.body.iter() {
				vars.extend(instr.defs());
				vars.extend(instr.uses());
			}

			vars.extend(block.term.uses());
		}

		NoopLivenessInfo { vars }
	}

	pub fn live_in_body(&self, _: BlockId, _: usize) -> HashSet<TypedSsaVar> {
		self.vars.clone()
	}

	pub fn live_out_body(&self, _: BlockId, _: usize) -> HashSet<TypedSsaVar> {
		self.vars.clone()
	}
}

// Just a list of the variables that are defined at each instruction
pub struct SimpleBlockLivenessInfo {
	vars: Vec<HashSet<TypedSsaVar>>,
	live_out: HashSet<TypedSsaVar>,
}

impl SimpleBlockLivenessInfo {
	pub fn new(block: &SsaBasicBlock, mut live_in: HashSet<TypedSsaVar>) -> Self {
		todo!();
		/*live_in.extend(block.params.iter().copied());

		let mut vars = Vec::new();
		for instr in block.body.iter() {
			vars.push(live_in.clone());
			live_in.extend(instr.defs());
		}

		//data.push(live_in);
		//live_in.extend(block.term.defs());

		SimpleBlockLivenessInfo { vars, live_out: live_in }*/
	}
}

pub struct SimpleLivenessInfo(LocalBlockMap<SimpleBlockLivenessInfo>);

impl SimpleLivenessInfo {
	fn analyze(func: &SsaFunction) -> Self {
		let dom_tree = DomTree::analyze(func);
		
		let mut result = LocalBlockMap::new(func.func_id() as usize);

		let entry_id = func.entry_point_id();
		let mut queue = vec![(entry_id, HashSet::new())];

		while let Some((next_block_id, next_block_ins)) = queue.pop() {
			let node = dom_tree.0.get(next_block_id).unwrap();

			let info = SimpleBlockLivenessInfo::new(func.get(next_block_id), next_block_ins);

			for child in node.children.iter() {
				queue.push((*child, info.live_out.clone()));
			}

			result.insert(next_block_id, info);
		}

		SimpleLivenessInfo(result)
	}

	fn live_in_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar> {
		let block_info = if let Some(bi) = self.0.get(block) {
			bi
		} else {
			return HashSet::new();
		};

		block_info.vars[instr].clone()
	}

	fn live_out_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar> {
		let block_info = if let Some(bi) = self.0.get(block) {
			bi
		} else {
			return HashSet::new();
		};

		assert!(instr < block_info.vars.len());
		if instr == block_info.vars.len() - 1 {
			block_info.live_out.clone()
		} else {
			block_info.vars[instr + 1].clone()
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Label {
    Phi,
    Instr(usize),
    Term,
}

#[derive(Debug, Clone, Default)]
pub struct BlockInVars(PairMap<usize, TypedSsaVarSet>);

impl Extend<TypedSsaVar> for BlockInVars {
    fn extend<T: IntoIterator<Item = TypedSsaVar>>(&mut self, iter: T) {
		for item in iter {
			for (_, s) in self.0.iter_mut() {
				s.insert(item).unwrap();
			}
		}
    }
}

type BlockUses = BlockInVars;

type BlockLiveIn = BlockInVars;

// Just a list of the variables that are defined at each instruction
#[derive(Debug, Clone)]
pub struct FullBlockLivenessInfo {
	/// If this block has a phi node/named arguments,
	/// this specifies which incoming edge each variables is live-in on.
	pub phi_live_in: Option<BlockInVars>,

	/// Entry 0 is the live-in variables to index 0,
	/// entry 1 is the live-in variables to index 1, etc.
	/// The last index is the live-in to the terminator.
	pub live_in: Vec<TypedSsaVarSet>,

	/// Contains the live-out variables from this block's terminator.
	/// This is the union of all control-flow edges.
	pub live_out: TypedSsaVarSet,
}

impl FullBlockLivenessInfo {
	fn empty(block: &SsaBasicBlock) -> Self {
		FullBlockLivenessInfo {
			phi_live_in: None,
			live_in: vec![TypedSsaVarSet::new(); block.body.len() + 1],
			live_out: TypedSsaVarSet::new(),
		}
	}

	pub fn live_in_body(&self, instr: usize) -> TypedSsaVarSet {
		todo!();
		self.live_in[instr].clone()
	}

	pub fn live_out_body(&self, instr: usize) -> TypedSsaVarSet {
		todo!();
		if instr == self.live_in.len() - 1 {
			self.live_out.clone()
		} else {
			self.live_in[instr + 1].clone()
		}
	}

	fn compute_liveness_info_multi(&mut self, block_id: BlockId, block: &SsaBasicBlock, live_out: &TypedSsaVarSet) {
        self.live_out = live_out.clone();

        let mut live_in = live_out.clone();
        live_in.extend_typed(block.term.uses()).unwrap();

        *self.live_in.last_mut().unwrap() = live_in.clone();

        for (live_in_set, instr) in self.live_in.iter_mut().zip(block.body.iter()).rev() {
            live_in = compute_straight_live_in_multi(instr, live_in);

            live_in_set.extend_typed(live_in.iter()).unwrap();
        }

		if !block.phi_node.is_empty() {
            for &def in &block.phi_node.dests {
                live_in.remove_typed(def).unwrap();
            }

			let mut phi_live_in = BlockLiveIn::default();

			for (source_block, source_vars) in block.phi_node.sources() {
				let source_block = BlockId { func: block_id.block, block: source_block };
				let mut vars = live_in.clone();
				vars.extend_typed(source_vars.iter().copied()).unwrap();
				phi_live_in.0.insert(source_block.block, vars);
			}

			self.phi_live_in = Some(phi_live_in);
		}
    }

	pub fn live_range(&self, var: TypedSsaVar) -> BlockLiveRange {
		todo!()

		/*let live_in_params = if let Some(phi_live_in) = &self.phi_live_in {
			phi_live_in.0.iter().filter_map(|(pred, vars)| {
				if vars.contains(var) {
					Some(pred)
				} else {
					None
				}
			})
			.collect()
		} else {
			Vec::new()
		};

		let live_in = self.live_in_all.contains(&var);
		let live_out = self.live_out.contains(&var);

		let mut body = Vec::new();

		let mut start_idx = None;

		for (idx, live_in) in self.live_in.iter().enumerate() {
			if start_idx.is_none() && live_in.contains(&var) {
				start_idx = Some(idx);
			} else if start_idx.is_some() && !live_in.contains(&var) {
				let start = start_idx.take().unwrap();
				body.push(start..=idx - 1)
			}
		}

		if let Some(start_idx) = start_idx {
			body.push(start_idx..=self.live_in.len() - 1);
		}

		BlockLiveRange { live_in_params, live_in, body, live_out }*/
	}

	pub fn get_live_in_at(&self, source_block: BlockId, label: Label) -> &TypedSsaVarSet {
        let len = self.live_in.len() - 1;

        match label {
            Label::Phi => {
                if let Some(phi_live_in) = &self.phi_live_in {
                    phi_live_in.0.get(&source_block.block).unwrap()
                } else {
                    &self.live_in[0]
                }
            }
            Label::Instr(i) if i < len => &self.live_in[i],
            Label::Instr(i) => {
                panic!("Trying to get live out for instr {i}, but only have live_in up to {len}")
            }
            Label::Term => self.live_in.last().unwrap(),
        }
    }

	fn set_live_in(&mut self, live_in: BlockLiveIn) {
		assert!(!live_in.0.is_empty());
		if live_in.0.len() == 1 {
			assert!(self.phi_live_in.is_none());
			self.live_in[0] = live_in.0.into_iter().next().unwrap().1;
		} else {
			self.phi_live_in = Some(live_in);
		}
    }

	pub fn iter_live_out(&self) -> impl Iterator<Item = &TypedSsaVarSet> {
        self.live_in.iter().chain(Some(&self.live_out))
    }


}

fn compute_liveness_single<D>(live_out: TypedSsaVarSet, uses: &BlockUses, defs: D) -> BlockLiveIn
where
    D: IntoIterator<Item = TypedSsaVar>,
{
	let mut liveness = live_out;
    liveness.remove_typed_from(defs).unwrap();

	let mut result = BlockLiveIn::default();
	for (block_id, uses) in uses.0.iter() {
		let mut lv = liveness.clone();
		lv.extend_typed(uses.iter()).unwrap();
		result.0.insert(*block_id, lv);
	}

	result
}

fn compute_liveness_info(function: &SsaFunction, predecessors: &PredInfo) -> FullLivenessInfo {
	let block_use_def_sets = function 
		.iter()
		.map(|(i, b)| (i, compute_block_use_def_sets(i, b)))
		.collect::<LocalBlockMap<_>>();

	let mut dirty_blocks = BinaryHeap::<usize>::new();

	let mut info = FullLivenessInfo::empty(function);

	// Do a first pass to mark dirty blocks and get initial values.
	for (block, _) in function.iter() {
		let mut new_live_out = TypedSsaVarSet::new();
		for succ in function.get(block).term.successors() {
			println!("{:?} {:?}", block, succ);
			let succ_live_in = info.0.get(succ).unwrap().get_live_in_at(block, Label::Phi);
			new_live_out.extend_typed(succ_live_in.iter()).unwrap();
		}

		let (block_uses, block_defs) = block_use_def_sets.get(block).unwrap();

		info.0.get_mut(block).unwrap().live_out = new_live_out.clone();
		let live_in = compute_liveness_single(new_live_out, block_uses, block_defs.iter());
		info.0.get_mut(block).unwrap().set_live_in(live_in);

		let dirty_predecessors = predecessors
			.get_predecessors(block)
			.iter()
			.map(|b| b.block);
		dirty_blocks.extend(dirty_predecessors);
	}

	while let Some(dirty_block) = dirty_blocks.pop() {
		let dirty_block = BlockId { func: function.func_id() as usize, block: dirty_block };

		let mut new_live_out = TypedSsaVarSet::new();
		for succ in function.get(dirty_block).term.successors() {
			let succ_live_in = info.0.get(succ).unwrap().get_live_in_at(dirty_block, Label::Phi);
			new_live_out.extend_typed(succ_live_in.iter()).unwrap();
		}

		if info.0.get(dirty_block).unwrap().live_out != new_live_out {
			// Only recompute liveness info for predecessors if the liveness actually has changed.
			// live_in_states[dirty_block] = new_live_in;

			let (block_uses, block_defs) = block_use_def_sets.get(dirty_block).unwrap();

			info.0.get_mut(dirty_block).unwrap().live_out = new_live_out.clone();
			let live_in = compute_liveness_single(new_live_out, block_uses, block_defs.iter());
			info.0.get_mut(dirty_block).unwrap().set_live_in(live_in);

			let dirty_predecessors = predecessors
				.get_predecessors(dirty_block)
				.iter()
				.map(|b| b.block);
			dirty_blocks.extend(dirty_predecessors);
		}
	}

	for (block_id, block) in function.iter() {
		let block_info = info.0.get_mut(block_id).unwrap();
		let live_out = std::mem::take(&mut block_info.live_out);
		block_info.compute_liveness_info_multi(block_id, block, &live_out);
	}

	info
}

fn compute_straight_live_in_multi(instr: &SsaInstr, live_out: TypedSsaVarSet) -> TypedSsaVarSet {
    let mut live_in_before = live_out;
    for def in instr.defs() {
        live_in_before.remove_typed(def).unwrap();
    }

    live_in_before.extend_typed(instr.uses());

    live_in_before
}

/// Computes the effective use and def sets for a basic block.
///
/// To make liveness analysis faster, we can do the liveness calculations at the block level first.
/// Then we can do instruction-level liveness strictly within each block.
fn compute_block_use_def_sets(block_id: BlockId, block: &SsaBasicBlock) -> (BlockUses, TypedSsaVarSet) {
    // A block defines all variables that are defined by any instruction within.
    // A block uses all variables that are used before they are defined in the block.

    // For each instruction L in the block:
    //     Uses(b) = Uses(b) ∪ (Uses(L) − Defs(b))
    //     Defs(b) = Defs(b) ∪ Defs(L)

    let mut uses = if block.phi_node.is_empty() {
		let mut block_uses = BlockUses::default();
		block_uses.0.insert(Default::default(), TypedSsaVarSet::new());
		block_uses
	} else {
		let mut block_uses = BlockUses::default();

		assert_ne!(block.phi_node.sources().count(), 0, "{:?}", block_id);

		for (source_block, source_vars) in block.phi_node.sources() {
			let source_block = BlockId { func: block_id.block, block: source_block };
			let mut vars = TypedSsaVarSet::new();
			vars.extend_typed(source_vars.iter().copied()).unwrap();
			block_uses.0.insert(source_block.block, vars);
		}

		block_uses
    };

    let mut defs = TypedSsaVarSet::new();

    for &dest in &block.phi_node.dests {
        defs.insert(dest).unwrap();
    }

    for instr in &block.body {
        let new_uses = instr.uses().into_iter().filter(|u| !defs.contains(*u));
        uses.extend(new_uses);
        defs.extend_typed(instr.defs()).unwrap();
    }

    let new_uses = block.term.uses().into_iter().filter(|u| !defs.contains(*u));
    uses.extend(new_uses);

    (uses, defs)
}

#[derive(Debug)]
pub struct FullLivenessInfo(pub LocalBlockMap<FullBlockLivenessInfo>);

impl FullLivenessInfo {
	fn empty(func: &SsaFunction) -> Self {
		let mut map = LocalBlockMap::new(func.func_id() as usize);
		for (id, block) in func.iter() {
			map.insert(id, FullBlockLivenessInfo::empty(block));
		}
		FullLivenessInfo(map)
	}

    pub fn print_live_in(&self, func: &SsaFunction, vars: &[SsaVar]) {
        let max_instr_width = func 
            .iter()
            .map(|(_, block)| {
				//let p = format!("parameters: {:?}", block.params);
                let phi_width = "".len();
                let instr_width = block
                    .body
                    .iter()
                    .map(|instr| instr.to_string().len())
                    .max()
                    .unwrap_or(0);

                let term_width = block.term.to_string().len();

                phi_width.max(instr_width.max(term_width))
            })
            .max()
            .unwrap_or(0);

        for (block_id, block) in func.iter() {
			let info = self.0.get(block_id).unwrap();

            println!("\n=== BLOCK {block_id:?} ===\n");

            print!("{:max_instr_width$}", "");
            for var in vars {
                print!(" | {:<3}", var.0.to_string());
            }
            println!(" |");

            /*for node in block.phi_nodes.iter() {
                print!("{:max_instr_width$}", node.to_string());
                for &var in vars {
                    let live_in_path = self.0[block_idx].get_live_in_path(var);
                    let live_in_path = match live_in_path {
                        Some(LiveInPath::Multi) => "+".to_string(),
                        Some(LiveInPath::Single(s)) => s.to_string(),
                        None => " ".to_string(),
                    };

                    print!(" |  {live_in_path} ");
                }
                println!(" |");
            }*/

			// TODO:
			println!("parameters: {:?}", block.phi_node);

            for (idx, instr) in block.body.iter().enumerate() {
                print!("{:max_instr_width$}", instr.to_string());
                for &var in vars {
                    if info.live_in[idx].contains_untyped(var) {
                        print!(" |  + ");
                    } else {
                        print!(" |    ");
                    }
                }
                println!(" |");
            }

            print!("{:max_instr_width$}", block.term.to_string());
            for &var in vars {
                if info.live_in[block.body.len()].contains_untyped(var) {
                    print!(" |  + ");
                } else {
                    print!(" |    ");
                }
            }
            println!(" |");
        }
    }

}

impl FullLivenessInfo {
	pub fn analyze(func: &SsaFunction) -> Self {
		let pred = PredInfo::new(func);

		compute_liveness_info(func, &pred)
	}

	pub fn live_in_body(&self, block: BlockId, instr: usize) -> &TypedSsaVarSet {
		todo!()
	}

	pub fn live_out_body(&self, block: BlockId, instr: usize) -> &TypedSsaVarSet {
		let block = self.0.get(block).unwrap();
		&block.live_in[instr + 1]
	}

	fn live_range(&self, var: TypedSsaVar) -> LiveRange {
		let blocks = self.0.iter().map(|(id, block)| (id, block.live_range(var))).collect();
		LiveRange(blocks)
	}
}

pub fn print_liveness_info(li: &FullLivenessInfo, func: &SsaFunction) {
	for (id, block) in func.iter() {
		let block_info = li.0.get(id).unwrap();
		assert_eq!(block_info.live_in.len(), block.body.len() + 1);
		println!("---- block {id:?} ---- ");
		for (live_in, instr) in block_info.live_in.iter().zip(block.body.iter()) {
			println!("\t--> {:?}", live_in);
			println!("\t\t{:?}", instr);
		}
		println!("\t--> {:?}", block_info.live_in.last().unwrap());
		println!("\t\t{:?}", block.term);
		println!("\t--> {:?}", block_info.live_out);
	}
}


pub fn print_live_ranges(lr: &[LiveRange], func: &SsaFunction) {
	for (block_id, block) in func.iter() {
		println!("------- block {:?} ------", block_id);

		for r in lr.iter() {
			print!("{:?}, ", r.0.get(block_id).unwrap().live_in_params);
		}
		println!();

		for r in lr.iter() {
			let r = r.0.get(block_id).unwrap();
			if r.live_in { print!(" + "); } else { print!("   "); }
		}
		println!("parameters: {:?}", block.phi_node);
		
		for (idx, instr) in block.body.iter().enumerate() {
			for r in lr.iter() {
				let r = r.0.get(block_id).unwrap();
				let is_live = r.body.iter().any(|r2| r2.contains(&idx));
				if is_live { print!(" + ") } else { print!("   "); }
			}
			println!("{:?}", instr);
		}

		for r in lr.iter() {
			let r = r.0.get(block_id).unwrap();
			let is_live = r.body.iter().any(|r2| r2.contains(&block.body.len()));
			if is_live { print!(" + "); } else { print!("   "); }
		}
		println!("{:?}", block.term);
	}
}


pub fn get_postorder(func: &SsaFunction) -> Vec<BlockId> {
	let mut result = Vec::new();
	let mut visited = HashSet::new();
	let node = func.entry_point_id();
	get_postorder_impl(func, node, &mut visited, &mut result);
	result
}

pub fn get_postorder_impl(func: &SsaFunction, node: BlockId, visited: &mut HashSet<BlockId>, result: &mut Vec<BlockId>) {
	if visited.contains(&node) {
		return;
	}

	visited.insert(node);

	let block = func.get(node);

	for child in block.term.successors() {
		get_postorder_impl(func, child, visited, result);
	}

	result.push(node);
}

#[derive(Debug)]
pub struct PredInfo(LocalBlockMap<Vec<BlockId>>);

impl PredInfo {
	pub fn new(func: &SsaFunction) -> Self {
		let mut result = LocalBlockMap::new(func.func_id() as usize);

		for (block_id, _) in func.iter() {
			// Make sure all blocks have a default.
			result.insert(block_id, Vec::new());
		}

		for (pred_id, block) in func.iter() {
			for succ_id in block.term.successors() {
				let v = result.entry(succ_id).get_or_insert_with(Vec::new);
				if !v.contains(&pred_id) {
					v.push(pred_id);
				}
			}
		}

		PredInfo(result)
	}

	pub fn get_predecessors(&self, block_id: BlockId) -> &[BlockId] {
		self.0.get(block_id).unwrap()
	}
}

pub fn get_predecessors(func: &SsaFunction, node: BlockId) -> Vec<BlockId> {
	func.iter()
		.filter_map(|(block_id, block)| {
			if block.term.successors().contains(&node) {
				Some(block_id)
			} else {
				None
			}
		})
		.collect()
}

// https://www.cs.rice.edu/~keith/EMBED/dom.pdf

#[derive(Debug)]
pub struct DomTreeNode {
	pub parent: Option<BlockId>,
	pub children: Vec<BlockId>,
}

#[derive(Debug)]
pub struct DomTree(LocalBlockMap<DomTreeNode>);

impl DomTree {
	/// Gets a postorder traversal of the tree, so it will start at the leaf nodes.
	pub fn get_postorder(&self) -> Vec<BlockId> {
		let entry_node = self.0.keys().find(|b| b.block == 0).unwrap();

		let mut stack = vec![(entry_node, 0)];

		let mut result = Vec::new();

		while let Some((node_id, state)) = stack.last_mut() {
			let node = self.0.get(*node_id).unwrap();
			if node.children.len() == *state {
				result.push(*node_id);
				stack.pop();
			} else {
				let next_node = node.children[*state];
				*state += 1;
				stack.push((next_node, 0));
			}
		}

		result
	}

	pub fn dominates(&self, parent: BlockId, child: BlockId) -> bool {
		if parent == child {
			return true
		}

		if !self.0.contains_key(parent) {
			// Anywhere else in the function this should panic
			return false
		}

		let mut to_visit = vec![parent];
		while let Some(node) = to_visit.pop() {
			if node == child {
				return true;
			}
			to_visit.extend(self.0.get(node).unwrap().children.iter().copied())
		}

		false
	}

	pub fn analyze(func: &SsaFunction) -> Self {
		let postorder = get_postorder(func);
		assert_eq!(postorder.last(), Some(&func.entry_point_id()));

		let mut doms = LocalBlockMap::new(func.func_id() as usize);

		for id in postorder.iter() {
			doms.insert(*id, None);
		}
		doms.insert(func.entry_point_id(), Some(func.entry_point_id()));

		let mut changed = true;

		let pred_info = PredInfo::new(func);

		while changed {
			changed = false;
			for (b_idx, b) in postorder.iter().enumerate().rev().skip(1) {
				// Make sure we only consider reachable blocks
				let preds = pred_info.get_predecessors(*b).iter().filter(|p| doms.contains_key(**p)).copied().collect::<Vec<_>>();

				let mut new_idom = *preds.iter().find(|p| {
					let other_idx = postorder.iter().enumerate().find(|(_, p2)| p2 == p).unwrap().0;
					other_idx > b_idx
				}).unwrap_or_else(|| panic!("{:?} {:?}", b, preds));

				let idom = new_idom;

				for p in preds.iter() {
					if *p == idom {
						continue
					}

					if let Some(dom) = doms.get(*p).unwrap() {
						new_idom = intersect(*dom, new_idom, &doms, &postorder);
					}
				}

				if *doms.get(*b).unwrap() != Some(new_idom) {
					doms.insert(*b, Some(new_idom));
					changed = true;
				}
			}
		}

		fn intersect(mut finger1: BlockId, mut finger2: BlockId, doms: &LocalBlockMap<Option<BlockId>>, postorder: &[BlockId]) -> BlockId {
			let get_idx = |id: BlockId| -> usize {
				postorder.iter().enumerate().find(|(_, i)| **i == id).unwrap().0
			};

			let mut finger1_idx = get_idx(finger1);
			let mut finger2_idx = get_idx(finger2);

			while finger1 != finger2 {
				while finger1_idx < finger2_idx {
					finger1 = doms.get(finger1).unwrap().unwrap();
					finger1_idx = get_idx(finger1);
				}
				while finger2_idx < finger1_idx {
					finger2 = doms.get(finger2).unwrap().unwrap();
					finger2_idx = get_idx(finger2);
				}
			}

			finger1
		}

		let doms = doms.into_iter().map(|(k, v)| (k, v.unwrap())).collect::<LocalBlockMap<_>>();

		let mut tree = doms.iter().map(|(c, p)| {
			let node = DomTreeNode {
				parent: if c == *p { None } else { Some(*p) },
				children: Vec::new()
			};
			(c, node)
		}).collect::<LocalBlockMap<_>>();

		for (c, p) in doms.into_iter() {
			if c != p {
				tree.get_mut(p).unwrap().children.push(c);
			}
		}

		DomTree(tree)
	}

	pub fn print(&self) {
		let entry_node = self.0.keys().find(|b| b.block == 0).unwrap();
		self.print_impl(entry_node, 0);
	}

	fn print_impl(&self, key: BlockId, indent: u32) {
		let node = self.0.get(key).unwrap();
		for _ in 0..indent {
			print!("  ");
		}
		println!("{:?} (parent: {:?})", key, node.parent);
		for child in node.children.iter() {
			self.print_impl(*child, indent + 1);
		}
	}
}

#[cfg(test)]
mod test {
	use wasmparser::ValType;

	use crate::ssa::{SsaFunction, SsaTerminator, TypedSsaVar, JumpTarget, BlockId, SsaBasicBlock, liveness::DomTree, PhiNode};

	fn make_func(v: Vec<Vec<usize>>) -> SsaFunction {
		let code = v.into_iter().enumerate().map(|(id, dests)| {
			let dests = dests.into_iter().map(|d| {
				JumpTarget {
					label: BlockId { func: 0, block: d },
				}
			}).collect::<Vec<_>>();

			let term = if dests.is_empty() {
				SsaTerminator::Return(Vec::new())
			} else {
				SsaTerminator::BranchTable { cond: TypedSsaVar(0, ValType::I32), default: dests[0].clone(), arms: dests }
			};

			let block_id = BlockId { func: 0, block: id };
			let block = SsaBasicBlock {
				phi_node: PhiNode::default(),
				body: Vec::new(),
				term,
			};

			(block_id, block)
		});

		SsaFunction::new(code, Box::new([]), Box::new([]))
	}

	#[test]
	#[ignore]
	fn dom_tree() {
		let func = make_func(vec![
			vec![1], // 0
			vec![2], // 1
			vec![3, 4, 6], // 2
			vec![5], // 3
			vec![5], // 4
			vec![2], // 5
			vec![], // 6
		]);

		let tree = DomTree::analyze(&func);

		tree.print();

		todo!();
	}
}