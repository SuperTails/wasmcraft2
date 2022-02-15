use std::collections::{HashSet, HashMap};

use super::{TypedSsaVar, SsaFunction, BlockId, SsaBasicBlock};

pub trait LivenessInfo {
	fn analyze(func: &SsaFunction) -> Self;

	fn live_in_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar>;

	fn live_out_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar>;
}

pub struct NoopLivenessInfo {
	vars: HashSet<TypedSsaVar>,
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
		let block_info = self.0.get(&block).unwrap();

		assert!(instr < block_info.vars.len());
		if instr == block_info.vars.len() - 1 {
			block_info.live_out.clone()
		} else {
			block_info.vars[instr + 1].clone()
		}
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