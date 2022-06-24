use std::collections::HashSet;

use super::{SsaProgram, liveness::{LivenessInfo, FullLivenessInfo}, BlockId, SsaBasicBlock, SsaFunction, SsaTerminator};

pub fn do_dead_code_elim(program: &mut SsaProgram) {
	for func in program.code.iter_mut() {
		let live_info = FullLivenessInfo::analyze(func);
		
		for (block_id, block) in func.iter_mut() {
			let changes = get_dce_changes(block_id, block, &live_info);

			apply_changes(block, &changes);
		}
	}

	/*
	for func in program.code.iter_mut() {
		// Safe to precalculate it once because DCE doesn't change overall control-flow.
		let pred_info = super::liveness::PredInfo::new(func);

		let mut changed = true;
		while changed {
			changed = false;

			let live_info = FullLivenessInfo::analyze(func);

			let ids = func.iter().map(|(i, _)| i).collect::<Vec<_>>();

			for block_id in ids {
				let block_ids = pred_info.get_predecessors(block_id).iter().copied().collect::<HashSet<_>>();

				let block = func.get_mut(block_id);
				let indices_to_remove = get_param_dce_indices(block_id, block, &live_info);

				if !indices_to_remove.is_empty() {
					changed = true;
				}

				remove_from_params(block, &indices_to_remove);

				for pred_id in block_ids {
					let block = func.get_mut(pred_id);
					remove_from_target(pred_id, block_id, block, &indices_to_remove);
				}
			}
		}

		println!("Did DCE on {}", func.func_id());
	}
	*/
}

fn remove_from_target(block_id: BlockId, child: BlockId, block: &mut SsaBasicBlock, mut indices: &[usize]) {
	let mut offset = 0;
	let mut i = 0;
	while let Some(index) = indices.first() {
		if index - offset == i {
			indices = &indices[1..];
			offset += 1;

			match &mut block.term {
				SsaTerminator::Unreachable => {},
				SsaTerminator::ScheduleJump(t, _) |
				SsaTerminator::Jump(t) => {
					t.params.remove(i);
					assert_eq!(t.label, child);
				}
				SsaTerminator::BranchIf { cond: _, true_target, false_target } => {
					if true_target.label == child {
						true_target.params.remove(i);
					}
					if false_target.label == child {
						false_target.params.remove(i);
					}
					assert!(true_target.label == child || false_target.label == child);
				}
				SsaTerminator::BranchTable { cond: _, default, arms } => {
					assert!(default.label == child || arms.iter().any(|a| a.label == child));
					if default.label == child {
						default.params.remove(i);
					}
					for arm in arms {
						if arm.label == child {
							arm.params.remove(i);
						}
					}
				}
				SsaTerminator::Return(_) => panic!(),
			}
		} else {
			i += 1;
		}
	}
}

fn remove_from_params(block: &mut SsaBasicBlock, mut indices: &[usize]) {
	let mut offset = 0;
	let mut i = 0;
	while let Some(index) = indices.first() {
		if index - offset == i {
			indices = &indices[1..];
			block.params.remove(i);
			offset += 1;
		} else {
			i += 1;
		}
	}
}

fn get_param_dce_indices(block_id: BlockId, block: &SsaBasicBlock, live_info: &FullLivenessInfo) -> Vec<usize> {
	block.params.iter().enumerate().filter(|(_i, p)| {
		!live_info.0.get(&block_id).unwrap().live_in[0].contains(p)
	}).map(|(i, _)| i).collect()
}

fn get_dce_changes(block_id: BlockId, block: &SsaBasicBlock, live_info: &FullLivenessInfo) -> Vec<usize> {
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