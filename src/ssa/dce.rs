use super::{SsaProgram, liveness::{LivenessInfo, NoopLivenessInfo}, BlockId, SsaBasicBlock};

pub fn do_dead_code_elim(program: &mut SsaProgram) {
	for func in program.code.iter_mut() {
		let live_info = NoopLivenessInfo::analyze(func);
		
		for (block_id, block) in func.code.iter_mut() {
			let changes = get_dce_changes(*block_id, block, &live_info);

			apply_changes(block, &changes);
		}
	}
}

fn get_dce_changes(block_id: BlockId, block: &SsaBasicBlock, live_info: &NoopLivenessInfo) -> Vec<usize> {
	block.body.iter().enumerate().filter_map(|(idx, instr)| {
		let live_out = live_info.live_out_body(block_id, idx);
		let defs = instr.defs();
		let all_defs_unused = defs.iter().all(|def| !live_out.contains(def));
		if all_defs_unused && !instr.has_side_effects() {
			Some(idx)
		} else {
			None
		}
	}).collect()
}

fn apply_changes(block: &mut SsaBasicBlock, mut changes: &[usize]) {
	let mut offset = 0;

	let mut i = 0;
	while let Some(change) = changes.first() {
		if *change - offset == i {
			changes = &changes[1..];
			block.body.remove(i);
			offset += 1;
		} else {
			i += 1;
		}
	}

	assert!(changes.is_empty());
}