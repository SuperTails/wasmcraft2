use std::{collections::{HashSet, HashMap}, ops::RangeInclusive, fmt};

use super::{TypedSsaVar, SsaFunction, BlockId, SsaBasicBlock};

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
	pub live_in: bool,
	/// This *includes* the instruction that creates/kills the variable!
	pub body: Vec<RangeInclusive<usize>>,
	pub live_out: bool,
}

impl BlockLiveRange {
	pub fn merge(&mut self, other: &BlockLiveRange) {
		self.live_in |= other.live_in;

		self.body.extend(other.body.iter().cloned());
		simplify_range_list(&mut self.body);

		self.live_out |= other.live_out;
	}

	pub fn overlap(&self, other: &BlockLiveRange) -> BlockLiveRange {
		let live_in = self.live_in && other.live_in;
		let live_out = self.live_out && other.live_out;

		let mut body = Vec::new();

		for r1 in self.body.iter() {
			for r2 in other.body.iter() {
				if let Some(overlap) = range_overlap(r1.clone(), r2.clone()) {
					body.push(overlap);
				}
			}
		}

		simplify_range_list(&mut body);

		BlockLiveRange { live_in, body, live_out }
	}

	pub fn is_empty(&self) -> bool {
		if self.live_in || self.live_out {
			false
		} else {
			self.body.is_empty()
		}
	}
}

#[derive(Debug, Clone)]
pub struct LiveRange(pub HashMap<BlockId, BlockLiveRange>);

impl LiveRange {
	pub fn merge(&mut self, other: LiveRange) {
		for (id, range) in other.0 {
			if let Some(r1) = self.0.get_mut(&id) {
				r1.merge(&range);
			} else {
				self.0.insert(id, range);
			}
		}
	}

	pub fn get_single_point(&self) -> Option<(BlockId, usize)> {
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
	}
}

impl LiveRange {
	pub fn overlap(&self, other: &LiveRange) -> LiveRange {
		let mut result = HashMap::new();

		for (id, r1) in self.0.iter() {
			if let Some(r2) = other.0.get(id) {
				result.insert(*id, r1.overlap(r2));
			}
		}

		LiveRange(result)
	}
}

pub trait LivenessInfo {
	fn analyze(func: &SsaFunction) -> Self;

	fn live_in_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar>;

	fn live_out_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar>;

	fn live_range(&self, _var: TypedSsaVar) -> LiveRange { todo!() }
}



pub struct NoopLivenessInfo {
	pub vars: HashSet<TypedSsaVar>,
}

impl LivenessInfo for NoopLivenessInfo {
	fn analyze(func: &SsaFunction) -> Self {
		let mut vars = HashSet::new();

		for (_, block) in func.iter() {
			vars.extend(block.params.iter());

			for instr in block.body.iter() {
				vars.extend(instr.defs());
				vars.extend(instr.uses());
			}

			vars.extend(block.term.uses());
		}

		NoopLivenessInfo { vars }
	}

	fn live_in_body(&self, _: BlockId, _: usize) -> HashSet<TypedSsaVar> {
		self.vars.clone()
	}

	fn live_out_body(&self, _: BlockId, _: usize) -> HashSet<TypedSsaVar> {
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
		live_in.extend(block.params.iter().copied());

		let mut vars = Vec::new();
		for instr in block.body.iter() {
			vars.push(live_in.clone());
			live_in.extend(instr.defs());
		}

		//data.push(live_in);
		//live_in.extend(block.term.defs());

		SimpleBlockLivenessInfo { vars, live_out: live_in }
	}
}

pub struct SimpleLivenessInfo(HashMap<BlockId, SimpleBlockLivenessInfo>);

impl LivenessInfo for SimpleLivenessInfo {
	fn analyze(func: &SsaFunction) -> Self {
		let dom_tree = DomTree::analyze(func);

		let mut result = HashMap::new();

		let entry_id = func.entry_point_id();
		let mut queue = vec![(entry_id, HashSet::new())];

		while let Some((next_block_id, next_block_ins)) = queue.pop() {
			let node = dom_tree.0.get(&next_block_id).unwrap();

			let info = SimpleBlockLivenessInfo::new(func.get(next_block_id), next_block_ins);

			for child in node.children.iter() {
				queue.push((*child, info.live_out.clone()));
			}

			result.insert(next_block_id, info);
		}

		SimpleLivenessInfo(result)
	}

	fn live_in_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar> {
		let block_info = self.0.get(&block).unwrap();
		block_info.vars[instr].clone()
	}

	fn live_out_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar> {
		let block_info = if let Some(bi) = self.0.get(&block) {
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

// Just a list of the variables that are defined at each instruction
#[derive(Debug)]
pub struct FullBlockLivenessInfo {
	pub live_in: Vec<HashSet<TypedSsaVar>>,
	live_out: HashSet<TypedSsaVar>,
}

impl FullBlockLivenessInfo {
	pub fn new(block: &SsaBasicBlock, parent: &SsaFunction, mut live_out: HashSet<TypedSsaVar>) -> Self {
		let live_out2 = live_out.clone();

		let term_defs = block.term.defs(parent);
		let mut live_in = live_out.iter().copied().filter(|v| !term_defs.contains(v)).collect::<HashSet<_>>();
		live_in.extend(block.term.uses());

		let mut live_in_list = vec![live_in.clone()];

		live_out = live_in.clone();

		for instr in block.body.iter().rev() {
			let defs = instr.defs();
			let uses = instr.uses();

			live_in = live_out.iter().copied().filter(|v| !defs.contains(v)).collect();
			live_in.extend(uses.into_iter());

			live_in_list.push(live_in.clone());

			live_out = live_in.clone();
		}

		live_in_list.reverse();

		// TODO:
		//live_in_list[0].extend(block.params.iter().copied());

		FullBlockLivenessInfo { live_in: live_in_list, live_out: live_out2 }
	}

	pub fn live_in_body(&self, instr: usize) -> HashSet<TypedSsaVar> {
		self.live_in[instr].clone()
	}

	pub fn live_out_body(&self, instr: usize) -> HashSet<TypedSsaVar> {
		if instr == self.live_in.len() - 1 {
			self.live_out.clone()
		} else {
			self.live_in[instr + 1].clone()
		}
	}

	pub fn live_range(&self, var: TypedSsaVar) -> BlockLiveRange {
		let live_in = self.live_in[0].contains(&var);
		let live_out = self.live_out.contains(&var);

		let mut body = Vec::new();

		let mut start_idx = None;

		for (idx, live_in) in self.live_in.iter().enumerate().skip(1) {
			if let Some(si) = start_idx {
				if !live_in.contains(&var) {
					body.push(si - 1..=idx - 1);
					start_idx = None;
				}
			} else if live_in.contains(&var) {
				start_idx = Some(idx);
			}
		}

		if let Some(start_idx) = start_idx {
			body.push(start_idx - 1..=self.live_in.len() - 1);
		} else if live_out {
			// It must be defined by the terminator
			body.push(self.live_in.len() - 1..=self.live_in.len() - 1);
		}

		BlockLiveRange { live_in, body, live_out }
	}
}

#[derive(Debug)]
pub struct FullLivenessInfo(pub HashMap<BlockId, FullBlockLivenessInfo>);

impl LivenessInfo for FullLivenessInfo {
	fn analyze(func: &SsaFunction) -> Self {
		let mut result = HashMap::new();

		for (block_id, block) in func.iter() {
			result.insert(block_id, FullBlockLivenessInfo::new(block, func, HashSet::new()));
		}

		let mut changed = true;
		while changed {
			changed = false;

			for (block_id, block) in func.iter() {
				let mut live_out = HashSet::new();
				for succ in block.term.successors() {
					let succ_live_in = result.get(&succ).unwrap();
					live_out.extend(succ_live_in.live_in[0].iter().copied());
				}

				let block_info = result.get(&block_id).unwrap();
				if block_info.live_out != live_out {
					changed = true;
					let new_info = FullBlockLivenessInfo::new(block, func, live_out);
					/*println!("New info for block {:?}", block_id);
					println!("LIVE IN:  {:?}", new_info.live_in[0]);
					println!("LIVE OUT: {:?}", new_info.live_out);
					println!();*/
					result.insert(block_id, new_info);
				}
			}
		}

		FullLivenessInfo(result)
	}

	fn live_in_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar> {
		let block_info = self.0.get(&block).unwrap();
		block_info.live_in_body(instr)
	}

	fn live_out_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar> {
		let block_info = if let Some(bi) = self.0.get(&block) {
			bi
		} else {
			return HashSet::new();
		};

		block_info.live_out_body(instr)
	}

	fn live_range(&self, var: TypedSsaVar) -> LiveRange {
		let blocks = self.0.iter().map(|(id, block)| (*id, block.live_range(var))).collect();
		LiveRange(blocks)
	}
}

pub fn print_liveness_info(li: &FullLivenessInfo, func: &SsaFunction) {
	for (id, block) in func.iter() {
		let block_info = li.0.get(&id).unwrap();
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
			let r = r.0.get(&block_id).unwrap();
			if r.live_in { print!(" + "); } else { print!("   "); }
		}
		println!("parameters: {:?}", block.params);
		
		for (idx, instr) in block.body.iter().enumerate() {
			for r in lr.iter() {
				let r = r.0.get(&block_id).unwrap();
				let is_live = r.body.iter().any(|r2| r2.contains(&idx));
				if is_live { print!(" + ") } else { print!("   "); }
			}
			println!("{:?}", instr);
		}

		for r in lr.iter() {
			let r = r.0.get(&block_id).unwrap();
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
pub struct DomTree(HashMap<BlockId, DomTreeNode>);

impl DomTree {
	pub fn analyze(func: &SsaFunction) -> Self {
		let postorder = get_postorder(func);
		assert_eq!(postorder.last(), Some(&func.entry_point_id()));

		let mut doms = HashMap::new();

		for id in postorder.iter() {
			doms.insert(*id, None);
		}
		doms.insert(func.entry_point_id(), Some(func.entry_point_id()));

		let mut changed = true;

		while changed {
			changed = false;
			for (b_idx, b) in postorder.iter().enumerate().rev().skip(1) {
				let preds = get_predecessors(func, *b);
				// Make sure we only consider reachable blocks
				let preds = preds.into_iter().filter(|p| doms.contains_key(p)).collect::<Vec<_>>();

				let mut new_idom = *preds.iter().find(|p| {
					let other_idx = postorder.iter().enumerate().find(|(_, p2)| p2 == p).unwrap().0;
					other_idx > b_idx
				}).unwrap();
				let idom = new_idom;

				for p in preds.iter() {
					if *p == idom {
						continue
					}

					if let Some(dom) = doms.get(p).unwrap() {
						new_idom = intersect(*dom, new_idom, &doms, &postorder);
					}
				}

				if *doms.get(b).unwrap() != Some(new_idom) {
					println!("new_idom is now {:?}", new_idom);
					doms.insert(*b, Some(new_idom));
					changed = true;
				}
			}
		}

		fn intersect(mut finger1: BlockId, mut finger2: BlockId, doms: &HashMap<BlockId, Option<BlockId>>, postorder: &[BlockId]) -> BlockId {
			let get_idx = |id: BlockId| -> usize {
				postorder.iter().enumerate().find(|(_, i)| **i == id).unwrap().0
			};

			let mut finger1_idx = get_idx(finger1);
			let mut finger2_idx = get_idx(finger2);

			while finger1 != finger2 {
				while finger1_idx < finger2_idx {
					finger1 = doms.get(&finger1).unwrap().unwrap();
					finger1_idx = get_idx(finger1);
				}
				while finger2_idx < finger1_idx {
					finger2 = doms.get(&finger2).unwrap().unwrap();
					finger2_idx = get_idx(finger2);
				}
			}

			finger1
		}

		let doms = doms.into_iter().map(|(k, v)| (k, v.unwrap())).collect::<HashMap<_, _>>();

		let mut tree = doms.iter().map(|(c, p)| {
			let node = DomTreeNode {
				parent: if c == p { None } else { Some(*p) },
				children: Vec::new()
			};
			(*c, node)
		}).collect::<HashMap<_, _>>();

		for (c, p) in doms.iter() {
			if c != p {
				tree.get_mut(p).unwrap().children.push(*c);
			}
		}

		DomTree(tree)
	}

	pub fn print(&self) {
		let entry_node = self.0.keys().find(|b| b.block == 0).unwrap();
		self.print_impl(entry_node, 0);
	}

	fn print_impl(&self, key: &BlockId, indent: u32) {
		let node = self.0.get(key).unwrap();
		for _ in 0..indent {
			print!("  ");
		}
		println!("{:?} (parent: {:?})", key, node.parent);
		for child in node.children.iter() {
			self.print_impl(child, indent + 1);
		}
	}
}

#[cfg(test)]
mod test {
	use wasmparser::Type;

	use crate::ssa::{SsaFunction, SsaTerminator, TypedSsaVar, JumpTarget, BlockId, SsaBasicBlock, liveness::DomTree};

	fn make_func(v: Vec<Vec<usize>>) -> SsaFunction {
		let code = v.into_iter().enumerate().map(|(id, dests)| {
			let dests = dests.into_iter().map(|d| {
				JumpTarget {
					label: BlockId { func: 0, block: d },
					params: Vec::new(),
				}
			}).collect::<Vec<_>>();

			let term = if dests.is_empty() {
				SsaTerminator::Return(Vec::new())
			} else {
				SsaTerminator::BranchTable { cond: TypedSsaVar(0, Type::I32), default: dests[0].clone(), arms: dests }
			};

			let block_id = BlockId { func: 0, block: id };
			let block = SsaBasicBlock {
				params: Vec::new(),
				body: Vec::new(),
				term,
			};

			(block_id, block)
		}).collect();

		SsaFunction { code, params: Box::new([]), returns: Box::new([]) }
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