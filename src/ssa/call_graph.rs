use super::SsaProgram;

pub struct NoopCallGraph {
	
}

impl NoopCallGraph {
	pub fn new(_program: &SsaProgram) -> Self {
		NoopCallGraph {}
	}

	pub fn may_recurse(&self, func: u32) -> bool {
		// TODO:
		true
	}

	pub fn may_recurse_in(&self, caller: u32, callee: u32) -> bool {
		// TODO:
		true
	}

	pub fn may_recurse_in_table(&self, caller: u32, callee_table: u32) -> bool {
		// TODO:
		true
	}

	pub fn is_single_tick(&self, func: u32) -> bool {
		// TODO:
		false
	}

	pub fn table_is_single_tick(&self, table: u32) -> bool {
		// TODO:
		false
	}
}