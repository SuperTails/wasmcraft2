use std::collections::HashMap;

use super::{SsaProgram, SsaBasicBlock, TypedSsaVar, interp::TypedValue, SsaInstr, SsaVarOrConst};

pub fn do_const_prop(program: &mut SsaProgram) {
	for func in program.code.iter_mut() {
		for (_, block) in func.code.iter_mut() {
			do_block_const_prop(block);
		}
	}
}

fn do_block_const_prop(block: &mut SsaBasicBlock) {
	let mut constants = HashMap::<TypedSsaVar, TypedValue>::new();

	for instr in block.body.iter_mut() {
		match *instr {
			SsaInstr::I32Set(dst, src) => {
				constants.insert(dst, TypedValue::I32(src));
			}
			SsaInstr::Add(dst, SsaVarOrConst::Var(lhs), SsaVarOrConst::Var(rhs)) => {
				if let (Some(l), Some(r)) = (constants.get(&lhs), constants.get(&rhs)) {
					assert_eq!(lhs.ty(), dst.ty());
					assert_eq!(rhs.ty(), dst.ty());

					match (l, r) {
						(&TypedValue::I32(l), &TypedValue::I32(r)) => {
							let d = l + r;
							*instr = SsaInstr::I32Set(dst, d);
							constants.insert(dst, d.into());
						}
						(&TypedValue::I64(l), &TypedValue::I64(r)) => {
							let d = l + r;
							*instr = SsaInstr::I64Set(dst, d);
							constants.insert(dst, d.into());
						}
						_ => panic!(),
					}
				}
			}
			_ => {
				let args = instr.constable_vars();

				for arg in args {
					if let SsaVarOrConst::Var(v) = arg {
						if let Some(a) = constants.get(v) {
							*arg = (*a).into();
						}
					}
				}
			}
		}
	}
}