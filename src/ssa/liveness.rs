use std::collections::HashSet;

use super::{TypedSsaVar, SsaFunction, BlockId};

pub struct NoopLivenessInfo {
	vars: HashSet<TypedSsaVar>,
}

impl NoopLivenessInfo {
	pub fn new(func: &SsaFunction) -> Self {
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

	pub fn live_in_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar> {
		self.vars.clone()
	}

	pub fn live_out_body(&self, block: BlockId, instr: usize) -> HashSet<TypedSsaVar> {
		self.vars.clone()
	}
}