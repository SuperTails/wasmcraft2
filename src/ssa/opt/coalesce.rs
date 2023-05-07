use std::collections::{BinaryHeap, HashMap};

use union_find_rs::{prelude::DisjointSets, traits::UnionFind};
use wasmparser::ValType;

type Color = Register;

use crate::{set::{Set, DenseMap}, ssa::{SsaVar, SsaFunction, SsaInstr, SsaVarOrConst, is_simple_and_mask, liveness::PredInfo}, graph::Graph, lir::Register};

#[derive(Debug, Clone, Default)]
struct AffinityEdges(HashMap<SsaVar, Vec<(SsaVar, u32)>>);

impl AffinityEdges {
    pub fn from_list(affinity_edges: &[(u32, (SsaVar, SsaVar))]) -> Self {
        let mut result = AffinityEdges::default();
        for &(cost, (a, b)) in affinity_edges {
            result.insert_edge(a, b, cost);
        }
        result
    }

    fn insert_edge(&mut self, a: SsaVar, b: SsaVar, cost: u32) {
        self.0.entry(a).or_default().push((b, cost));
    }

    #[allow(clippy::needless_lifetimes)]
    fn neighbors<'a>(&'a self, a: SsaVar) -> impl Iterator<Item = (SsaVar, u32)> + 'a {
        self.0.get(&a).unwrap().iter().copied()
    }

    fn cost(&self, a: SsaVar, b: SsaVar) -> Option<u32> {
        self.neighbors(a).find(|(t, _)| *t == b).map(|(_, c)| c)
    }
}

/// Returns all of the affinity edges in a function.
/// Two temps have an affinity edge between them when one is copied to another,
/// e.g. by a phi node or by a mov instruction.
///
/// The goal of coalescing is to color all nodes related by affinity edges
/// the same color, because then we don't have to actually generate any instructions to copy them.
///
/// # Returns
///
/// The list of affinity edges and their costs, sorted from highest cost to lowest.
fn compute_affinity_edges(func: &SsaFunction) -> Vec<(u32, (SsaVar, SsaVar))> {
    // TODO: We could increase the cost of moves that occur inside of loops.

    let mut result = Vec::new();

    for (block_id, block) in func.iter() {
        for arm in block.phi_node.arms() {
			let dest = arm.dest();
            for (_, src) in arm.sources() {
                result.push((1, (dest, src)));
                result.push((1, (src, dest)));
            }
        }

        for instr in &block.body {
            match *instr {
				SsaInstr::Mul(dst, lhs, SsaVarOrConst::Var(rhs)) |
				SsaInstr::Add(dst, SsaVarOrConst::Var(lhs), SsaVarOrConst::Var(rhs)) if dst.ty() == ValType::I32 => {
					result.push((1, (dst, lhs)));
					result.push((1, (dst, rhs)));
					result.push((1, (lhs, dst)));
					result.push((1, (rhs, dst)));
				}
				SsaInstr::Mul(dst, lhs, _) |
				SsaInstr::Add(dst, SsaVarOrConst::Var(lhs), _) if dst.ty() == ValType::I32 => {
					result.push((1, (dst, lhs)));
					result.push((1, (lhs, dst)));
				}
				SsaInstr::Mul(dst, _, SsaVarOrConst::Var(rhs)) |
				SsaInstr::Add(dst, _, SsaVarOrConst::Var(rhs)) if dst.ty() == ValType::I32 => {
					result.push((1, (dst, rhs)));
					result.push((1, (rhs, dst)));
				}

				SsaInstr::Assign(dst, SsaVarOrConst::Var(src)) => {
					result.push((1, (dst, src)));
					result.push((1, (src, dst)));
				}

				SsaInstr::ShrS(dst, lhs, _) |
				SsaInstr::ShrU(dst, lhs, _) |
				SsaInstr::Shl(dst, lhs, _) => {
					result.push((1, (dst, lhs)));
					result.push((1, (lhs, dst)));
				}

				SsaInstr::And(dst, lhs, SsaVarOrConst::Const(c))
					if dst.ty() == ValType::I32 && is_simple_and_mask(c.into_i32().unwrap()) => {
						result.push((1, (dst, lhs)));
						result.push((1, (lhs, dst)));
					}
				
				_ => {}
			}
        }
    }

    // Sort from high to low instead of low to high
    result.sort_by_key(|(cost, _)| std::cmp::Reverse(*cost));

    todo!()

    //result
}

#[derive(Clone)]
pub struct IFGraph(pub Graph);

impl IFGraph {
    fn interferes(&self, t1: SsaVar, t2: SsaVar) -> bool {
        let edges = self.0.get_edges();
        let t1_edges = edges.get(&t1).unwrap();
        t1_edges.contains(&t2)
    }

    fn union(&mut self, t1: SsaVar, t2: SsaVar, dest: SsaVar) {
        let mut edges_1 = self.0.get_edges().get(&t1).unwrap().clone();
        edges_1.extend(self.0.get_edges().get(&t2).unwrap());
        self.0.edges.insert(dest, edges_1);
    }

    #[allow(clippy::needless_lifetimes)]
    fn neighbors<'a>(&'a self, t1: SsaVar) -> impl Iterator<Item = SsaVar> + 'a {
        self.0.get_edges().get(&t1).unwrap().iter().copied()
    }
}

/// A chunk is a set of affinity-related variables.
/// For example, in the following code:
/// ```
/// T1 <- T0
/// T2 <- T1
/// ```
/// T0, T1, and T2 would all be affinity-related.
/// If we allocated them to the same register,
/// all of the moves could be eliminated.
#[derive(Debug)]
struct Chunk {
    cost: u32,
    rep: SsaVar,
    temps: Vec<SsaVar>,
}

impl PartialEq for Chunk {
    fn eq(&self, other: &Self) -> bool {
        self.cost == other.cost
    }
}

impl Eq for Chunk {}

impl PartialOrd for Chunk {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.cost.partial_cmp(&other.cost)
    }
}

impl Ord for Chunk {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.cost.cmp(&other.cost)
    }
}

fn compute_chunks(
    mut if_graph: IFGraph,
    affinity_edges: &[(u32, (SsaVar, SsaVar))],
    coloring: &DenseMap<SsaVar, Color>,
) -> BinaryHeap<Chunk> {
    // Place each node in a separate chunk.
    // For each affinity edge (a = xy) from high to low cost:
    //      If no node in x's chunk inteferes with a node in y's chunk:
    //          Merge the chunks of x and y.

    let mut chunks = DisjointSets::new();
    for temp in coloring.keys() {
        chunks.make_set(temp).unwrap();
    }

    let num_temps = coloring.keys().map(|k| k.0 as usize + 1).max().unwrap_or(0);

    let mut costs = vec![0; num_temps];

    // Keep an adjacency matrix, where only the nodes that have a representative in the relation actually are used.
    // For each edge a = xy from high to low cost:
    //      rep_x = chunks.find(x)
    //      rep_y = chunks.find(y)
    //      edges_x = graph.get(x)
    //      edges_y = graph.get(y)
    //      if (edges_x & edges_y) == 0:
    //          graph.merge(x, y)
    //          chunks.merge(x, y)

    for (cost, (temp1, temp2)) in affinity_edges {
        let temp1 = chunks.find_set(temp1).unwrap();
        let temp2 = chunks.find_set(temp2).unwrap();
        if temp1 == temp2 {
            continue;
        }

        if !if_graph.interferes(temp1, temp2) {
            let temp1_cost = costs[temp1.0 as usize];
            let temp2_cost = costs[temp2.0 as usize];

            chunks.union(&temp1, &temp2).unwrap();
            let dest = chunks.find_set(&temp1).unwrap();

            costs[dest.0 as usize] = temp1_cost + temp2_cost + cost;

            if_graph.union(temp1, temp2, dest);
        }
    }

    let mut result = Vec::<Chunk>::new();

    for temp in coloring.keys() {
        let rep = chunks.find_set(&temp).unwrap();

        let existing_chunk = result.iter_mut().find(|chk| chk.rep == rep);
        if let Some(existing_chunk) = existing_chunk {
            if !existing_chunk.temps.contains(&temp) {
                existing_chunk.temps.push(temp);
            }
        } else {
            result.push(Chunk {
                cost: costs[rep.0 as usize],
                rep,
                temps: vec![temp],
            })
        }
    }

    result.into_iter().collect()
}

pub struct Coloring {
    pub colors: DenseMap<SsaVar, Color>,
    /// Precolored temps cannot be recolored because of x86 rules.
    precolors: HashMap<SsaVar, Color>,
}

impl Coloring {
    pub fn is_admissible(&self, node: SsaVar, color: Color) -> bool {
        if let Some(precolor) = self.precolors.get(&node) {
            if *precolor != color {
                return false;
            }
        }

        true
    }

    pub fn get_color(&self, node: SsaVar) -> Color {
        self.colors.get(node).copied().unwrap()
    }

    pub fn set_color(&mut self, node: SsaVar, color: Color) -> Color {
        self.colors.insert(node, color).unwrap()
    }
}

fn avoid_color(
    if_graph: &IFGraph,
    node: SsaVar,
    bad_color: Color,
    fixed_nodes: &mut Set<SsaVar>,
    old_colors: &mut HashMap<SsaVar, Color>,
    coloring: &mut Coloring,
) -> Result<(), ()> {
    if coloring.get_color(node) != bad_color {
        // Node already avoids the color
        return Ok(());
    }

    if fixed_nodes.contains(node) {
        // We cannot recolor the node to avoid the color
        //println!("Node already fixed, giving up");
        return Err(());
    }

    // loop {

	let new_color = todo!("any admissible color");

    /*let new_color = (0..x86::CALLER_SAVED_REG_COUNT)
        .map(|c| Color::from_abstract_reg(c, *classes.get(node).unwrap()))
        .filter(|c| coloring.is_admissible(node, *c))
        .find(|c| {
            for neighbor in if_graph.neighbors(node) {
                if coloring.get_color(neighbor) == *c {
                    return false;
                }
            }

            true
        });*/

    // TODO: We could perform another recursive recoloring step after this instead of just giving up.
    let Some(new_color) = new_color else {
        //println!("Giving up");
        return Err(());
    };

    // Optimistically set the node to this new color.
    fixed_nodes.insert(node);
    let old_color = coloring.set_color(node, new_color);
    old_colors.insert(node, old_color);

    // } end loop

    Ok(())
}

fn recolor(
    if_graph: &IFGraph,
    node: SsaVar,
    color: Color,
    fixed_nodes: &mut Set<SsaVar>,
    coloring: &mut Coloring,
) {
    if coloring.is_admissible(node, color) && !fixed_nodes.contains(node) {
        let mut old_colors = HashMap::new();

        // Optimistically set the node to our desired color.
        fixed_nodes.insert(node);
        let old_color = coloring.set_color(node, color);
        old_colors.insert(node, old_color);

        // Then, try to recolor neighbors so that they don't conflict with the new color.
        for neighbor in if_graph.neighbors(node) {
            let neighbor_color = coloring.get_color(neighbor);
            if neighbor_color == color {
                let ok = avoid_color(
                    if_graph,
                    neighbor,
                    color,
                    fixed_nodes,
                    &mut old_colors,
                    coloring,
                );
                if ok.is_err() {
                    // The coloring is not possible, roll back to the old colors.
                    //println!("Rolling back colors...");
                    for (&old_node, &old_color) in &old_colors {
                        coloring.set_color(old_node, old_color);
                    }
                    // TODO: Should this break be here?
                    break;
                }
            }
        }

        fixed_nodes.remove_from(old_colors.keys().copied());
    }
}

fn best_affine_subset(
    chunk: &Chunk,
    affinity_edges: &AffinityEdges,
    goal_color: Color,
    coloring: &Coloring,
) -> (u32, Vec<SsaVar>) {
    let successful_temps = chunk
        .temps
        .iter()
        .copied()
        .filter(|t| coloring.get_color(*t) == goal_color)
        .collect::<Vec<_>>();

    let mut best_cost = -1;
    let mut best_subset = Vec::new();

    let mut visited = Set::<SsaVar>::new();
    loop {
        let Some(&root) = successful_temps.iter().find(|t| !visited.contains(**t)) else {
            break;
        };

        let mut cost = 0;
        let mut queue = vec![root];
        let mut nodes = Vec::new();

        while let Some(node) = queue.pop() {
            visited.insert(node);
            nodes.push(node);

            for (dest, edge_cost) in affinity_edges.neighbors(node) {
                if visited.contains(dest) {
                    continue;
                }

                if !successful_temps.contains(&dest) {
                    continue;
                }

                cost += edge_cost;
                queue.push(dest);
            }
        }

        if best_cost < cost as i32 {
            best_cost = cost as i32;
            best_subset = nodes;
        }
    }

    if best_cost == -1 {
        // TODO: ?
        (0, vec![])
    } else {
        (best_cost as u32, best_subset)
    }
}

fn compute_chunk_cost(nodes: &[SsaVar], affinity_edges: &AffinityEdges) -> u32 {
    let mut cost = 0;

    for (idx, &t1) in nodes.iter().enumerate() {
        for &t2 in &nodes[idx..] {
            if let Some(c) = affinity_edges.cost(t1, t2) {
                cost += c;
            }
        }
    }

    cost
}

/// Attempts to recolor all temps in the given chunk to be the same color.
/// Which color is chosen depends on some heuristics.
/// If not all temps can be recolored to be the same,
/// this adds a new chunk containing the failed temps to the queue.
fn recolor_chunk(
    if_graph: &IFGraph,
    affinity_edges: &AffinityEdges,
    chunk: Chunk,
    fixed_nodes: &mut Set<SsaVar>,
    chunk_queue: &mut BinaryHeap<Chunk>,
    coloring: &mut Coloring,
) {
    assert!(!chunk.temps.is_empty());
    if chunk.temps.len() == 1 {
        return;
    }

    // Try all the different possible colors to see which one works best.

	let color = todo!("any unused color");

	// Unfix all nodes in chunk
	fixed_nodes.remove_from(chunk.temps.iter().copied());

	// Attempt to recolor all nodes in the chunk to the current color
	for &node in &chunk.temps {
		recolor(
			if_graph,
			node,
			color,
			fixed_nodes,
			coloring,
		);
		fixed_nodes.insert(node);
	}

	// It may not have been possible to recolor all of the nodes in the chunk.
	// So, pick the best part of the chunk that actually did get recolored, and just use that.
	let (cost, best_affine_subset) =
		best_affine_subset(&chunk, affinity_edges, color, coloring);

    // We tried all the colors and found the best color for this chunk,
    // so recolor everything we can to that new color.

    /*println!("best costs: {best_costs}");
    println!("best color: {best_color:?}");
    println!("best set: {best_set:?}");*/

    // If any of the temps couldn't be recolored,
    // just put them in their own chunk and try again.

    let failed_nodes = chunk
        .temps
        .iter()
        .copied()
        .filter(|t| !best_affine_subset.contains(t))
        .collect::<Vec<_>>();

    if failed_nodes.len() == chunk.temps.len() {
        // The entire chunk failed,
        // so there is no point in trying it again.
        return;
    }

    if !failed_nodes.is_empty() {
        let cost = compute_chunk_cost(&failed_nodes, affinity_edges);
        let chunk = Chunk {
            cost,
            rep: failed_nodes[0], // Not used
            temps: failed_nodes,
        };
        chunk_queue.push(chunk);
    }
}

pub fn coalesce(
    func: &SsaFunction,
    if_graph: &IFGraph,
    coloring: &mut Coloring,
) {
    //let affinity_edges = compute_affinity_edges(func);
	let affinity_edges = Vec::new();

    // Place all chunks into a priority queue (first element has highest cost).
    // While the priority queue is not empty:
    //      Pop the maximum-cost chunk from the queue.
    //      For each color c:
    //          Try to recolor the chunk.

    let mut chunks = compute_chunks(if_graph.clone(), &affinity_edges, &coloring.colors);

    let affinity_edges = AffinityEdges::from_list(&affinity_edges);

    let mut fixed_nodes = Set::<SsaVar>::new();

    while let Some(chunk) = chunks.pop() {
        recolor_chunk(
            if_graph,
            &affinity_edges,
            chunk,
            &mut fixed_nodes,
            &mut chunks,
            coloring,
        );
    }
}
