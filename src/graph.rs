use std::{iter::zip, collections::{HashSet, HashMap}};

use wasmparser::ValType;

use crate::ssa::{SsaVar, liveness::FullLivenessInfo, SsaFunction, SsaInstr};

#[derive(Default, Debug, Clone)]
pub struct Graph {
    pub edges: HashMap<SsaVar, HashSet<SsaVar>>,
}

impl Graph {
    pub fn new(func: &SsaFunction, lv_info: &FullLivenessInfo) -> Graph {
        let mut graph = Graph::default();

        for (block_id, block_info) in lv_info.0.iter() {
            let block = func.get(block_id);

            for &d in &block.params {
                graph.add_vertex(d.into_untyped());
            }
            for instr in &block.body {
                for d in instr.defs() {
                    graph.add_vertex(d.into_untyped());
                }
                for u in instr.uses() {
                    graph.add_vertex(u.into_untyped());
                }
            }
            for u in block.term.uses() {
                graph.add_vertex(u.into_untyped());
            }

            println!("{:?}", func.func_id());
            println!("{:?}", block_id);
            for i in &block.body {
                println!("{:?}", i);
            }
            println!("{:?}", block.term);

            assert_eq!(block_info.iter_live_out().count(), block.body.len() + 2);

            let mut live_out = block_info.iter_live_out();

            let defs = Some(block.params.clone()).into_iter().chain(
                block.body.iter().map(|i| i.defs())
            ).chain(
                Vec::new() // terminators do not define vars
            );

            let zipped = zip(live_out.by_ref(), defs);

            for (live_outs, defs) in zipped {
                for x in live_outs.iter() {
                    for &y in &defs {
                        graph.add_edge(x.into_untyped(), y.into_untyped());
                    }
                }
            }
        }

        for (_, block) in func.iter() {
            for instr in block.body.iter() {
                match instr {
                    SsaInstr::Add(dst, lhs, rhs) if dst.ty() == ValType::I64 => {
                        if let Some(l) = lhs.get_var() {
                            graph.add_edge(dst.into_untyped(), l.into_untyped())
                        }
                        if let Some(r) = rhs.get_var() {
                            graph.add_edge(dst.into_untyped(), r.into_untyped());
                        }
                        if let (Some(l), Some(r)) = (lhs.get_var(), rhs.get_var()) {
                            graph.add_edge(l.into_untyped(), r.into_untyped());
                        }
                    }
                    SsaInstr::Ctz(dst, src) if dst.ty() == ValType::I64 => {
                        graph.add_edge(dst.into_untyped(), src.into_untyped());
                    }
                    SsaInstr::GeU(dst, lhs, rhs) |
                    SsaInstr::GtU(dst, lhs, rhs) |
                    SsaInstr::LeU(dst, lhs, rhs) |
                    SsaInstr::LtU(dst, lhs, rhs) => {
                        if let Some(l) = lhs.get_var() {
                            graph.add_edge(dst.into_untyped(), l.into_untyped());
                        }
                        if let Some(r) = rhs.get_var() {
                            graph.add_edge(dst.into_untyped(), r.into_untyped());
                        }
                    }
                    SsaInstr::GeS(dst, lhs, rhs) |
                    SsaInstr::GtS(dst, lhs, rhs) |
                    SsaInstr::LeS(dst, lhs, rhs) |
                    SsaInstr::LtS(dst, lhs, rhs) if dst.ty() == ValType::I64 => {
                        if let Some(l) = lhs.get_var() {
                            graph.add_edge(dst.into_untyped(), l.into_untyped());
                        }
                        if let Some(r) = rhs.get_var() {
                            graph.add_edge(dst.into_untyped(), r.into_untyped());
                        }
                    }
                    SsaInstr::RemU(dst, lhs, rhs) if dst.ty() == ValType::I32 => {
                        graph.add_edge(dst.into_untyped(), lhs.into_untyped());
                        if let Some(r) = rhs.get_var() {
                            graph.add_edge(dst.into_untyped(), r.into_untyped());
                        }
                    }
                    _ => {}
                }
            }
        }

        graph
    }

    pub fn add_vertex(&mut self, x: SsaVar) {
        self.edges.entry(x).or_default();
    }

    pub fn add_edge(&mut self, x: SsaVar, y: SsaVar) {
        if x == y {
            return;
        }

        self.insert(x, y);
        self.insert(y, x);
    }

    fn insert(&mut self, src: SsaVar, dest: SsaVar) {
        self.edges.entry(src).or_default().insert(dest);
    }

    pub fn get_edges(&self) -> &HashMap<SsaVar, HashSet<SsaVar>> {
        &self.edges
    }

    pub fn interferes(&self, a: SsaVar, b: SsaVar) -> bool {
        self.edges.get(&a).unwrap().contains(&b)
    }
}
