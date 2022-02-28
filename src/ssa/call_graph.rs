use std::collections::{HashMap, HashSet};

use crate::ssa::SsaTerminator;

use super::{SsaProgram, SsaInstr, SsaFunction};

pub struct CallGraph {
	direct_calls: HashMap<u32, HashSet<u32>>,

	is_single_tick: HashMap<u32, bool>,
}

impl CallGraph {
	pub fn new(program: &SsaProgram) -> Self {
		let direct_calls = get_direct_calls(program);

		let is_single_tick = get_single_tick_funcs(program);

		CallGraph { direct_calls, is_single_tick }
	}

	pub fn may_call(&self, caller: u32, callee: u32) -> bool {
		let callees = self.direct_calls.get(&caller).unwrap();
		callees.contains(&callee)
	}

	pub fn table_may_call(&self, _caller_table: u32, _callee: u32) -> bool {
		// TODO:
		true
	}

	pub fn is_single_tick(&self, func: u32) -> bool {
		*self.is_single_tick.get(&func).unwrap()
	}

	pub fn table_is_single_tick(&self, _table: u32) -> bool {
		// TODO:
		false
	}
}

fn get_direct_calls(program: &SsaProgram) -> HashMap<u32, HashSet<u32>> {
	let mut direct_calls = HashMap::new();

	for func in program.code.iter() {
		let caller_id = func.func_id();

		let mut callee_ids = HashSet::new();

		callee_ids.extend(iter_all_calls(program, func));

		assert!(!direct_calls.contains_key(&caller_id));
		direct_calls.insert(caller_id, callee_ids);
	}

	direct_calls

}

fn iter_direct_calls<'a>(func: &'a SsaFunction) -> impl Iterator<Item=u32> + 'a {
	func.code.iter().flat_map(|(_, block)| {
		block.body.iter().filter_map(|instr| {
			if let SsaInstr::Call { function_index, .. } = instr {
				Some(*function_index)
			} else {
				None
			}
		})
	})
}

fn iter_indirect_tables<'a>(func: &'a SsaFunction) -> impl Iterator<Item=u32> + 'a {
	func.code.iter().flat_map(|(_, block)| {
		block.body.iter().filter_map(|instr| {
			if let SsaInstr::CallIndirect { table_index, .. } = instr {
				Some(*table_index)
			} else {
				None
			}
		})
	})
}

fn iter_indirect_calls<'a>(program: &'a SsaProgram, func: &'a SsaFunction) -> impl Iterator<Item=u32> + 'a {
	iter_indirect_tables(func).flat_map(|table_idx| {
		let table = &program.tables[table_idx as usize];
		table.elements.iter().copied().flatten().map(|e| e as u32)
	})
}

fn iter_all_calls<'a>(program: &'a SsaProgram, func: &'a SsaFunction) -> impl Iterator<Item=u32> + 'a {
	iter_direct_calls(func).chain(iter_indirect_calls(program, func))
}

fn contains_scheduled_jump(func: &SsaFunction) -> bool {
	func.code.iter().any(|(_, block)| {
		matches!(block.term, SsaTerminator::ScheduleJump { .. } )
	})
}

fn get_single_tick_funcs(program: &SsaProgram) -> HashMap<u32, bool> {
	// A function is single tick iff:
	//	It contains no scheduled jumps, AND
	//	It all of its callees are single tick

	let mut is_single_tick = HashMap::new();

	for func in program.code.iter() {
		let is_st = !contains_scheduled_jump(func);
		is_single_tick.insert(func.func_id(), is_st);
	}

	let mut changed = true;
	while changed {
		changed = false;
		
		for func in program.code.iter() {
			if is_single_tick.get(&func.func_id()) == Some(&false) {
				continue;
			}

			let mut callees = iter_all_calls(program, func);
			let callees_single_tick = callees.all(|c| is_single_tick.get(&c).copied().unwrap_or(true));

			if !callees_single_tick {
				is_single_tick.insert(func.func_id(), false);
				changed = true;
			}
		}
	}

	is_single_tick
}