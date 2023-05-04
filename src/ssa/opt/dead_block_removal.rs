use std::collections::HashSet;

use crate::{ssa::{SsaFunction, SsaTerminator}, block_id_map::LocalBlockMap};

/// Removes blocks that are never jumped to.
pub fn dead_block_removal(func: &mut SsaFunction) {
    let mut visited_blocks = HashSet::new();
    let mut queue = vec![func.entry_point_id()];

    //println!("Determining visited blocks");

    while let Some(block) = queue.pop() {
        if visited_blocks.contains(&block) {
            continue;
        }

        visited_blocks.insert(block);

        let block = &func.get(block);

        for successor in block.term.successors() {
            queue.push(successor);
        }
    }

	let id = func.func_id();

	let code = std::mem::replace(&mut func.code, LocalBlockMap::new(id as usize));
	
    func.code = code
        .into_iter()
		.filter(|(idx, _)| visited_blocks.contains(idx))
        .collect();

    //println!("Removing redundant phi nodes");

    // Remove any phi nodes that have only a single predecessor now
    //remove_redundant_phi_nodes(program);

    //println!("Finished dead block removal");
}
