pub mod interp;
pub mod lir_emitter;
pub mod liveness;
pub mod call_graph;
pub mod const_prop;
pub mod dce;
pub mod reg_alloc;
pub mod opt;

use std::{collections::{HashMap, HashSet}, fmt};

use wasmparser::{MemoryImmediate, ValType};

use crate::{block_id_map::LocalBlockMap, set::PairMap};

use self::{interp::TypedValue, liveness::PredInfo};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BlockId {
	pub func: usize,
	pub block: usize
}

#[derive(Clone, Copy)]
pub struct PhiArm<'a> {
	node: &'a PhiNode,
	index: usize,
}

impl<'a> PhiArm<'a> {
	pub fn dest(&self) -> TypedSsaVar {
		self.node.dests[self.index]
	}

	pub fn sources(&'a self) -> impl Iterator<Item = (usize, TypedSsaVar)> + 'a {
		self.node.srcs.iter().map(|(k, v)| (*k, v[self.index]))
	}
}

pub struct PhiArmMut<'a> {
	node: &'a mut PhiNode,
	index: usize,
}

impl<'a> PhiArmMut<'a> {
	pub fn dest(&self) -> TypedSsaVar {
		self.node.dests[self.index]
	}

	pub fn sources(&'a self) -> impl Iterator<Item = (usize, TypedSsaVar)> + 'a {
		self.node.srcs.iter().map(|(k, v)| (*k, v[self.index]))
	}

	pub fn sources_mut(&'a mut self) -> impl Iterator<Item = (usize, &'a mut TypedSsaVar)> + 'a {
		self.node.srcs.iter_mut().map(|(k, v)| (*k, &mut v[self.index]))
	}
}


#[derive(Clone, Default, Debug)]
pub struct PhiNode {
	pub dests: Vec<TypedSsaVar>,
	srcs: PairMap<usize, Vec<TypedSsaVar>>,
}

impl PhiNode {
	pub fn new_with_dests(dests: Vec<TypedSsaVar>) -> Self {
		PhiNode {
			dests,
			srcs: PairMap::new(),
		}
	}

	pub fn arms<'a>(&'a self) -> impl Iterator<Item = PhiArm<'a>> + 'a {
		(0..self.dests.len()).map(|index| PhiArm { node: self, index })
	}

	pub fn arm_mut(&mut self, index: usize) -> PhiArmMut {
		PhiArmMut {
			node: self, 
			index,
		}
	}

	pub fn swap_remove_arm(&mut self, arm: usize) {
		self.dests.swap_remove(arm);
		for (_, source) in self.srcs.iter_mut() {
			source.swap_remove(arm);
		}
	}

	pub fn sources<'a>(&'a self) -> impl Iterator<Item = (usize, &'a [TypedSsaVar])> + 'a {
		self.srcs.iter().map(|(k, v)| (*k, &v[..]))
	}

	pub fn remove_source(&mut self, block: usize) -> Option<Vec<TypedSsaVar>> {
		self.srcs.remove(&block)
	}

	pub fn add_source(&mut self, block: usize, vars: Vec<TypedSsaVar>) -> Result<(), String> {
		if self.dests.len() != vars.len() {
			return Err(format!(
				"length of dests {} did not match length of new vars {}",
				self.dests.len(),
				vars.len()
			));
		}

		if let Some(src) = self.srcs.get(&block) {
			if *src != vars {
				return Err(format!("block {block} was already set"));
			}
		}

		self.srcs.insert(block, vars);

		Ok(())
	}

	pub fn rename_source(&mut self, old: usize, new: usize) {
		if let Some(vars) = self.srcs.remove(&old) {
			let conflict = self.srcs.insert(new, vars);
			if conflict.is_some() {
				todo!();
			}
		}
	}

	pub fn vars_from(&self, block: usize) -> Option<&[TypedSsaVar]> {
		self.srcs.get(&block).map(|s| &s[..])
	}

	pub fn var_count(&self) -> usize {
		self.dests.len()
	}

	pub fn is_empty(&self) -> bool {
		self.dests.is_empty()
	}
}

#[derive(Clone)]
pub struct SsaBasicBlock {
	pub phi_node: PhiNode,
	pub body: Vec<SsaInstr>,
	pub term: SsaTerminator,
}

impl Default for SsaBasicBlock {
	fn default() -> Self {
		SsaBasicBlock {
			phi_node: PhiNode::default(), body: Default::default(), term: SsaTerminator::Unreachable,
		}
	}
}

#[derive(Default)]
pub struct SsaVarAlloc(u32);

impl SsaVarAlloc {
	pub fn new() -> Self {
		Default::default()
	}

	fn next_id(&mut self) -> u32 {
		let id = self.0;
		self.0 += 1;
		id
	}

	pub fn new_typed(&mut self, ty: ValType) -> TypedSsaVar {
		TypedSsaVar(self.next_id(), ty)
	}

	pub fn new_i32(&mut self) -> TypedSsaVar {
		self.new_typed(ValType::I32)
	}

	pub fn new_i64(&mut self) -> TypedSsaVar {
		self.new_typed(ValType::I64)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SsaVar(u32);

impl SsaVar {
	pub fn into_typed(self, ty: ValType) -> TypedSsaVar {
		TypedSsaVar(self.0, ty)
	}
}

impl crate::set::Enumerable for SsaVar {
    fn to_usize(self) -> usize {
        self.0 as usize
    }

    fn from_usize(index: usize) -> Self {
        SsaVar(u32::try_from(index).unwrap())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SsaVarOrConst {
	Var(TypedSsaVar),
	Const(TypedValue),
}

impl SsaVarOrConst {
	pub fn ty(&self) -> ValType {
		match self {
			SsaVarOrConst::Var(v) => v.ty(),
			SsaVarOrConst::Const(c) => c.ty(),
		}
	}

	pub fn get_var(self) -> Option<TypedSsaVar> {
		if let Self::Var(v) = self {
			Some(v)
		} else {
			None
		}
	}

	pub fn get_var_mut(&mut self) -> Option<&mut TypedSsaVar> {
		if let Self::Var(v) = self {
			Some(v)
		} else {
			None
		}
	}
}

impl From<TypedSsaVar> for SsaVarOrConst {
	fn from(v: TypedSsaVar) -> Self {
		Self::Var(v)
	}
}

impl From<TypedValue> for SsaVarOrConst {
	fn from(c: TypedValue) -> Self {
		Self::Const(c)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypedSsaVar(u32, ValType);

impl TypedSsaVar {
	pub fn ty(self) -> ValType {
		self.1
	}

	pub fn into_untyped(self) -> SsaVar {
		SsaVar(self.0)
	}

	pub fn unwrap_i32(self) -> SsaVar {
		assert_eq!(self.1, ValType::I32);
		SsaVar(self.0)
	}

	pub fn unwrap_i64(self) -> SsaVar {
		assert_eq!(self.1, ValType::I64);
		SsaVar(self.0)
	}
}

impl fmt::Display for TypedSsaVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "V{}:{:?}", self.0, self.1)
    }
}

fn get_ty_index(ty: ValType) -> u32 {
	match ty {
		ValType::I32 => 0,
		ValType::I64 => 1,
		ValType::F32 => 2,
		ValType::F64 => 3,
		ValType::V128 => 4,
		ValType::FuncRef => 5,
		ValType::ExternRef => 6,
	}
}

impl PartialOrd for TypedSsaVar {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		(self.0, get_ty_index(self.1)).partial_cmp(&(other.0, get_ty_index(self.1)))
    }
}

impl Ord for TypedSsaVar {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.partial_cmp(other).unwrap()
	}
}

#[derive(Default, Clone)]
pub struct TypedSsaVarSet(HashMap<SsaVar, ValType>);

impl TypedSsaVarSet {
	pub fn new() -> Self {
		Self::default()
	}

	/// Returns whether the value was newly inserted.
	pub fn insert(&mut self, var: TypedSsaVar) -> Result<bool, ValType> {
		let old_type = self.0.insert(var.into_untyped(), var.ty());
		if let Some(old_type) = old_type {
			if old_type == var.ty() {
				Ok(false)
			} else {
				Err(old_type)
			}
		} else {
			Ok(true)
		}
	}

	pub fn extend_typed<I>(&mut self, iter: I) -> Result<(), ()>
	where
		I: IntoIterator<Item = TypedSsaVar>
	{
		for elem in iter {
			self.insert(elem).map_err(|_| ())?;
		}
		Ok(())
	}

	pub fn remove_typed(&mut self, var: TypedSsaVar) -> Result<bool, ValType> {
		if let Some(old_type) = self.0.remove(&var.into_untyped()) {
			if var.ty() == old_type {
				Ok(true)
			} else {
				Err(old_type)
			}
		} else {
			Ok(false)
		}
	}

	pub fn remove_untyped(&mut self, var: SsaVar) -> Option<ValType> {
		self.0.remove(&var)
	}

    /// Equivalent to calling `remove_typed` on all elements in the iterator.
    pub fn remove_typed_from<I>(&mut self, iter: I) -> Result<(), ()>
    where
        I: IntoIterator<Item = TypedSsaVar>,
    {
        for elem in iter {
            self.remove_typed(elem).map_err(|_| ())?;
        }

		Ok(())
    }

	pub fn iter<'a>(&'a self) -> impl Iterator<Item = TypedSsaVar> + 'a {
		self.0.iter().map(|(k, v)| k.into_typed(*v))
	}

	pub fn contains(&self, var: TypedSsaVar) -> bool {
		if let Some(ty) = self.0.get(&var.into_untyped()) {
			assert_eq!(*ty, var.ty());
			true
		} else {
			false
		}
	}

	pub fn contains_untyped(&self, var: SsaVar) -> bool {
		self.0.get(&var).is_some()
	}
}

impl fmt::Debug for TypedSsaVarSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let i = self.0.iter().map(|(v, t)| v.into_typed(*t));
		f.debug_set().entries(i).finish()
    }
}

impl PartialEq for TypedSsaVarSet {
    fn eq(&self, other: &Self) -> bool {
		if self.0.len() != other.0.len() {
			return false;
		}

		for (v1, t1) in self.0.iter() {
			let Some(t2) = other.0.get(v1) else {
				return false;
			};

			if t1 != t2 {
				return false;
			}
		}

		for (v1, t1) in other.0.iter() {
			let Some(t2) = self.0.get(v1) else {
				return false;
			};

			if t1 != t2 {
				return false;
			}
		}

		true
    }
}

impl Eq for TypedSsaVarSet {}

#[derive(Debug, Clone)]
pub enum SsaInstr {
	I32Set(TypedSsaVar, i32),
	I64Set(TypedSsaVar, i64),

	Assign(TypedSsaVar, SsaVarOrConst),

	// binop instructions: dst, lhs, rhs

	Add(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	Sub(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	Mul(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	DivS(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	DivU(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	RemS(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	RemU(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	Shl(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	ShrS(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	ShrU(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	Rotl(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Rotr(TypedSsaVar, TypedSsaVar, TypedSsaVar),
	Xor(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	And(TypedSsaVar, TypedSsaVar, SsaVarOrConst),
	Or(TypedSsaVar, TypedSsaVar, SsaVarOrConst),

	// comp instructions: dst, lhs, rhs

	GtS(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	GtU(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	GeS(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	GeU(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	LtS(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	LtU(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	LeS(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	LeU(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	Eq(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),
	Ne(TypedSsaVar, SsaVarOrConst, SsaVarOrConst),

	// unary instructions: dst, src

	Popcnt(TypedSsaVar, TypedSsaVar),
	Clz(TypedSsaVar, TypedSsaVar),
	Ctz(TypedSsaVar, TypedSsaVar),

	// test instructions: dst, src

	Eqz(TypedSsaVar, TypedSsaVar),

	// memory instructions
	// loads: dst, addr
	// stores: src, addr

	Load64(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load32S(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load32U(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load16S(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load16U(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load8S(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Load8U(MemoryImmediate, TypedSsaVar, SsaVarOrConst),

	Store64(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Store32(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Store16(MemoryImmediate, TypedSsaVar, SsaVarOrConst),
	Store8(MemoryImmediate, TypedSsaVar, SsaVarOrConst),

	// variable instructions

	GlobalSet(u32, TypedSsaVar),
	GlobalGet(TypedSsaVar, u32),

	LocalSet(u32, TypedSsaVar),
	LocalGet(TypedSsaVar, u32),

	ParamGet(TypedSsaVar, u32),

	// conversion ops: dst, src

	Extend8S(TypedSsaVar, TypedSsaVar),
	//Extend8U(TypedSsaVar, TypedSsaVar),
	Extend16S(TypedSsaVar, TypedSsaVar),
	//Extend16U(TypedSsaVar, TypedSsaVar),
	Extend32S(TypedSsaVar, TypedSsaVar),
	Extend32U(TypedSsaVar, TypedSsaVar),

	Wrap(TypedSsaVar, TypedSsaVar),

	// misc instructions

	Select {
		dst: TypedSsaVar,
		true_var: SsaVarOrConst,
		false_var: SsaVarOrConst,
		cond: TypedSsaVar,
	},

	Call {
		function_index: u32,
		params: Vec<TypedSsaVar>,
		returns: Vec<TypedSsaVar>,
	},
	CallIndirect {
		table_index: u32,
		table_entry: TypedSsaVar,
		params: Vec<TypedSsaVar>,
		returns: Vec<TypedSsaVar>,
	},

	// Optimized C stdlib calls

	Memset { dest: TypedSsaVar, value: TypedSsaVar, length: TypedSsaVar, result: TypedSsaVar },

	// Minecraft IO instructions

	TurtleSetX(SsaVarOrConst),
	TurtleSetY(SsaVarOrConst),
	TurtleSetZ(SsaVarOrConst),
	TurtleSetBlock(TypedSsaVar),
	TurtleFillBlock { block: SsaVarOrConst, x_span: SsaVarOrConst, y_span: SsaVarOrConst, z_span: SsaVarOrConst },
	TurtleCopyRegion { x_span: SsaVarOrConst, y_span: SsaVarOrConst, z_span: SsaVarOrConst },
	TurtlePasteRegionMasked { x_span: SsaVarOrConst, y_span: SsaVarOrConst, z_span: SsaVarOrConst },
	TurtleGetBlock(TypedSsaVar),
	TurtleCopy,
	TurtlePaste,
	PrintInt(TypedSsaVar),
	PutChar(TypedSsaVar),
}

impl SsaInstr {
	pub fn uses(&self) -> Vec<TypedSsaVar> {
		match self {
			SsaInstr::I32Set(_, _) => vec![],
			SsaInstr::I64Set(_, _) => vec![],

			SsaInstr::Assign(_, SsaVarOrConst::Var(src)) => vec![*src],
			SsaInstr::Assign(_, SsaVarOrConst::Const(_)) => vec![],

			SsaInstr::Add(_, lhs, rhs) |
			SsaInstr::Sub(_, lhs, rhs) => lhs.get_var().into_iter().chain(rhs.get_var()).collect(),

			SsaInstr::Mul(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::DivS(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::DivU(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::RemS(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::RemU(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::Shl(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::ShrS(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::ShrU(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::Rotl(_, lhs, rhs) |
			SsaInstr::Rotr(_, lhs, rhs) |
			SsaInstr::And(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::Xor(_, lhs, SsaVarOrConst::Var(rhs)) |
			SsaInstr::Or(_, lhs, SsaVarOrConst::Var(rhs)) => vec![*lhs, *rhs],

			SsaInstr::GtS(_, lhs, rhs) |
			SsaInstr::GtU(_, lhs, rhs) |
			SsaInstr::GeS(_, lhs, rhs) |
			SsaInstr::GeU(_, lhs, rhs) |
			SsaInstr::LtS(_, lhs, rhs) |
			SsaInstr::LtU(_, lhs, rhs) |
			SsaInstr::LeS(_, lhs, rhs) |
			SsaInstr::LeU(_, lhs, rhs) |
			SsaInstr::Eq(_, lhs, rhs) |
			SsaInstr::Ne(_, lhs, rhs) => lhs.get_var().into_iter().chain(rhs.get_var()).collect(),

			SsaInstr::Mul(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::And(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::Xor(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::Or(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::Shl(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::ShrS(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::ShrU(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::RemS(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::RemU(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::DivS(_, lhs, SsaVarOrConst::Const(_)) |
			SsaInstr::DivU(_, lhs, SsaVarOrConst::Const(_)) => vec![*lhs],


			SsaInstr::Popcnt(_, src) |
			SsaInstr::Clz(_, src) |
			SsaInstr::Ctz(_, src) => vec![*src],

			SsaInstr::Eqz(_, src) => vec![*src],

			SsaInstr::Load64(_, _, addr) | 
			SsaInstr::Load32S(_, _, addr) |
			SsaInstr::Load32U(_, _, addr) |
			SsaInstr::Load16S(_, _, addr) |
			SsaInstr::Load16U(_, _, addr) |
			SsaInstr::Load8S(_, _, addr) |
			SsaInstr::Load8U(_, _, addr) => addr.get_var().into_iter().collect(),

			SsaInstr::Store64(_, src, addr) |
			SsaInstr::Store32(_, src, addr) | 
			SsaInstr::Store16(_, src, addr) |
			SsaInstr::Store8(_, src, addr) => Some(*src).into_iter().chain(addr.get_var()).collect(),

			SsaInstr::GlobalSet(_, src) => vec![*src],
			SsaInstr::GlobalGet(_, _) => vec![],

			SsaInstr::LocalSet(_, src) => vec![*src],
			SsaInstr::LocalGet(_, _) => vec![], 

			SsaInstr::ParamGet(_, _) => vec![],

			SsaInstr::Extend8S(_, src) |
			SsaInstr::Extend16S(_, src) |
			SsaInstr::Extend32S(_, src) |
			SsaInstr::Extend32U(_, src) |
			SsaInstr::Wrap(_, src) => vec![*src],

			SsaInstr::Select { dst: _, true_var, false_var, cond } => {
				true_var.get_var().into_iter().chain(false_var.get_var()).chain(vec![*cond]).collect()
			}
			SsaInstr::Call { function_index: _, params, returns: _ } => params.clone(),
			SsaInstr::CallIndirect { table_index: _, table_entry, params, returns: _ } => {
				params.iter().copied().chain(Some(*table_entry)).collect()
			},

			SsaInstr::Memset { dest, value, length, result: _, } => {
				vec![*dest, *value, *length]
			}

			SsaInstr::TurtleSetX(SsaVarOrConst::Var(v)) |
			SsaInstr::TurtleSetY(SsaVarOrConst::Var(v)) |
			SsaInstr::TurtleSetZ(SsaVarOrConst::Var(v)) => vec![*v],

			SsaInstr::TurtleSetX(SsaVarOrConst::Const(_)) |
			SsaInstr::TurtleSetY(SsaVarOrConst::Const(_)) |
			SsaInstr::TurtleSetZ(SsaVarOrConst::Const(_)) => Vec::new(),

			SsaInstr::TurtleSetBlock(b) => vec![*b],
			SsaInstr::TurtleFillBlock { block, x_span, y_span, z_span } => {
				let mut result = Vec::new();
				result.extend(block.get_var());
				result.extend(x_span.get_var());
				result.extend(y_span.get_var());
				result.extend(z_span.get_var());
				result
			}
			SsaInstr::TurtleCopyRegion { x_span, y_span, z_span } => {
				let mut result = Vec::new();
				result.extend(x_span.get_var());
				result.extend(y_span.get_var());
				result.extend(z_span.get_var());
				result
			}
			SsaInstr::TurtlePasteRegionMasked { x_span, y_span, z_span } => {
				let mut result = Vec::new();
				result.extend(x_span.get_var());
				result.extend(y_span.get_var());
				result.extend(z_span.get_var());
				result
			}
			SsaInstr::TurtleGetBlock(_) => Vec::new(),
			SsaInstr::TurtleCopy => Vec::new(),
			SsaInstr::TurtlePaste => Vec::new(),
			SsaInstr::PrintInt(i) => vec![*i],
			SsaInstr::PutChar(i) => vec![*i],
		}
	}

	pub fn defs(&self) -> Vec<TypedSsaVar> {
		match self {
			SsaInstr::I32Set(dst, _) => vec![*dst],
			SsaInstr::I64Set(dst, _) => vec![*dst],

			SsaInstr::Assign(dst, _) => vec![*dst],

			SsaInstr::Add(dst, _, _) |
			SsaInstr::Sub(dst, _, _) |
			SsaInstr::Mul(dst, _, _) |
			SsaInstr::DivS(dst, _, _) |
			SsaInstr::DivU(dst, _, _) |
			SsaInstr::RemS(dst, _, _) |
			SsaInstr::RemU(dst, _, _) |
			SsaInstr::Shl(dst, _, _) |
			SsaInstr::ShrS(dst, _, _) |
			SsaInstr::ShrU(dst, _, _) |
			SsaInstr::Rotl(dst, _, _) |
			SsaInstr::Rotr(dst, _, _) |
			SsaInstr::Xor(dst, _, _) |
			SsaInstr::And(dst, _, _) |
			SsaInstr::Or(dst, _, _) |
			SsaInstr::GtS(dst, _, _) |
			SsaInstr::GtU(dst, _, _) |
			SsaInstr::GeS(dst, _, _) |
			SsaInstr::GeU(dst, _, _) |
			SsaInstr::LtS(dst, _, _) |
			SsaInstr::LtU(dst, _, _) |
			SsaInstr::LeS(dst, _, _) |
			SsaInstr::LeU(dst, _, _) |
			SsaInstr::Eq(dst, _, _) |
			SsaInstr::Ne(dst, _, _) => vec![*dst],

			SsaInstr::Popcnt(dst, _) |
			SsaInstr::Clz(dst, _) |
			SsaInstr::Ctz(dst, _) => vec![*dst],

			SsaInstr::Eqz(dst, _) => vec![*dst],

			SsaInstr::Load64(_, dst, _) |
			SsaInstr::Load32S(_, dst, _) |
			SsaInstr::Load32U(_, dst, _) |
			SsaInstr::Load16S(_, dst, _) |
			SsaInstr::Load16U(_, dst, _) |
			SsaInstr::Load8S(_, dst, _) |
			SsaInstr::Load8U(_, dst, _) => vec![*dst],

			SsaInstr::Store64(_, _, _) |
			SsaInstr::Store32(_, _, _) |
			SsaInstr::Store16(_, _, _) |
			SsaInstr::Store8(_, _, _) => vec![],

			SsaInstr::GlobalSet(_, _) => vec![],
			SsaInstr::GlobalGet(dst, _) => vec![*dst],

			SsaInstr::LocalSet(_, _) => vec![],
			SsaInstr::LocalGet(dst, _) => vec![*dst], 

			SsaInstr::ParamGet(dst, _) => vec![*dst],

			SsaInstr::Extend8S(dst, _) |
			SsaInstr::Extend16S(dst, _) |
			SsaInstr::Extend32S(dst, _) |
			SsaInstr::Extend32U(dst, _) |
			SsaInstr::Wrap(dst, _) => vec![*dst],

			SsaInstr::Select { dst, true_var: _, false_var: _, cond: _ } => vec![*dst],
			SsaInstr::Call { function_index: _, params: _, returns } => returns.clone(),
			SsaInstr::CallIndirect { returns, .. } => returns.clone(),

			SsaInstr::Memset { dest: _, value: _, length: _, result, } => {
				vec![*result]
			}

			SsaInstr::TurtleSetX(_) => Vec::new(),
			SsaInstr::TurtleSetY(_) => Vec::new(),
			SsaInstr::TurtleSetZ(_) => Vec::new(),
			SsaInstr::TurtleSetBlock(_) => Vec::new(),
			SsaInstr::TurtleFillBlock { .. } => Vec::new(),
			SsaInstr::TurtleCopyRegion { .. } => Vec::new(),
			SsaInstr::TurtlePasteRegionMasked { .. } => Vec::new(),
			SsaInstr::TurtleGetBlock(b) => vec![*b],
			SsaInstr::TurtleCopy => Vec::new(),
			SsaInstr::TurtlePaste => Vec::new(),
			SsaInstr::PrintInt(_) => Vec::new(),
			SsaInstr::PutChar(_) => Vec::new(),
		}
	}

	pub fn has_side_effects(&self) -> bool {
		match self {
			SsaInstr::I32Set(_, _) |
			SsaInstr::I64Set(_, _) |
			SsaInstr::Assign(_, _) |
			SsaInstr::Add(_, _, _) |
			SsaInstr::Sub(_, _, _) |
			SsaInstr::Mul(_, _, _) |
			SsaInstr::DivS(_, _, _) |
			SsaInstr::DivU(_, _, _) |
			SsaInstr::RemS(_, _, _) |
			SsaInstr::RemU(_, _, _) |
			SsaInstr::Shl(_, _, _) |
			SsaInstr::ShrS(_, _, _) |
			SsaInstr::ShrU(_, _, _) |
			SsaInstr::Rotl(_, _, _) |
			SsaInstr::Rotr(_, _, _) |
			SsaInstr::Xor(_, _, _) |
			SsaInstr::And(_, _, _) |
			SsaInstr::Or(_, _, _) |
			SsaInstr::GtS(_, _, _) |
			SsaInstr::GtU(_, _, _) |
			SsaInstr::GeS(_, _, _) |
			SsaInstr::GeU(_, _, _) |
			SsaInstr::LtS(_, _, _) |
			SsaInstr::LtU(_, _, _) |
			SsaInstr::LeS(_, _, _) |
			SsaInstr::LeU(_, _, _) |
			SsaInstr::Eq(_, _, _) |
			SsaInstr::Ne(_, _, _) |
			SsaInstr::Popcnt(_, _) |
			SsaInstr::Clz(_, _) |
			SsaInstr::Ctz(_, _) |
			SsaInstr::Eqz(_, _) => false,

			SsaInstr::Load64(_, _, _) |
			SsaInstr::Load32S(_, _, _) |
			SsaInstr::Load32U(_, _, _) |
			SsaInstr::Load16S(_, _, _) |
			SsaInstr::Load16U(_, _, _) |
			SsaInstr::Load8S(_, _, _) |
			SsaInstr::Load8U(_, _, _) |
			SsaInstr::Store64(_, _, _) |
			SsaInstr::Store32(_, _, _) |
			SsaInstr::Store16(_, _, _) |
			SsaInstr::Store8(_, _, _) |
			SsaInstr::GlobalSet(_, _) |
			SsaInstr::GlobalGet(_, _) |
			SsaInstr::LocalSet(_, _) |
			SsaInstr::LocalGet(_, _) |
			SsaInstr::ParamGet(_, _) => true,

			SsaInstr::Extend8S(_, _) |
			SsaInstr::Extend16S(_, _) |
			SsaInstr::Extend32S(_, _) |
			SsaInstr::Extend32U(_, _) |
			SsaInstr::Wrap(_, _) |
			SsaInstr::Select { .. } => false,

			SsaInstr::Call { .. } |
			SsaInstr::CallIndirect { .. } |
			SsaInstr::Memset { .. } |
			SsaInstr::TurtleSetX(_) |
			SsaInstr::TurtleSetY(_) |
			SsaInstr::TurtleSetZ(_) |
			SsaInstr::TurtleSetBlock(_) |
			SsaInstr::TurtleFillBlock { .. } |
			SsaInstr::TurtleCopyRegion { .. } |
			SsaInstr::TurtlePasteRegionMasked { .. } |
			SsaInstr::TurtleGetBlock(_) |
			SsaInstr::TurtleCopy |
			SsaInstr::TurtlePaste |
			SsaInstr::PrintInt(_) |
			SsaInstr::PutChar(_) => true,
		}

	}

	pub fn constable_vars(&mut self) -> Vec<&mut SsaVarOrConst> {
		match self {
			SsaInstr::Add(_, l, r) => vec![l, r],
			SsaInstr::Sub(_, l, r) => vec![l, r],

			SsaInstr::Assign(_, r) => vec![r],

			SsaInstr::Shl(_, _, r) |
			SsaInstr::ShrS(_, _, r) |
			SsaInstr::ShrU(_, _, r) |
			SsaInstr::And(_, _, r) |
			SsaInstr::Xor(_, _, r) |
			SsaInstr::Or(_, _, r) |
			SsaInstr::Mul(_, _, r) | 
			SsaInstr::RemS(_, _, r) | 
			SsaInstr::RemU(_, _, r) | 
			SsaInstr::DivS(_, _, r) | 
			SsaInstr::DivU(_, _, r) => vec![r],

			SsaInstr::GtS(_, lhs, rhs) |
			SsaInstr::GtU(_, lhs, rhs) |
			SsaInstr::GeS(_, lhs, rhs) |
			SsaInstr::GeU(_, lhs, rhs) |
			SsaInstr::LtS(_, lhs, rhs) |
			SsaInstr::LtU(_, lhs, rhs) |
			SsaInstr::LeS(_, lhs, rhs) |
			SsaInstr::LeU(_, lhs, rhs) |
			SsaInstr::Eq(_, lhs, rhs) |
			SsaInstr::Ne(_, lhs, rhs) => vec![lhs, rhs],

			SsaInstr::Load64(_, _, addr) | 
			SsaInstr::Load32S(_, _, addr) |
			SsaInstr::Load32U(_, _, addr) |
			SsaInstr::Load16S(_, _, addr) |
			SsaInstr::Load16U(_, _, addr) |
			SsaInstr::Load8S(_, _, addr) |
			SsaInstr::Load8U(_, _, addr) => vec![addr],

			SsaInstr::Store64(_, _, addr) |
			SsaInstr::Store32(_, _, addr) |
			SsaInstr::Store16(_, _, addr) |
			SsaInstr::Store8(_, _, addr) => vec![addr],

			SsaInstr::Select { dst: _, true_var, false_var, cond: _ } => vec![true_var, false_var],

			SsaInstr::TurtleSetX(v) |
			SsaInstr::TurtleSetY(v) |
			SsaInstr::TurtleSetZ(v) => vec![v],

			SsaInstr::TurtleFillBlock { block, x_span, y_span, z_span } => vec![block, x_span, y_span, z_span],

			SsaInstr::TurtleCopyRegion { x_span, y_span, z_span } |
			SsaInstr::TurtlePasteRegionMasked { x_span, y_span, z_span } => vec![x_span, y_span, z_span],

			_ => Vec::new(),
		}
	}

	pub fn coalescable_vars(&self) -> Vec<(&TypedSsaVar, &TypedSsaVar)> {
		match self {
			SsaInstr::Add(dst, SsaVarOrConst::Var(lhs), SsaVarOrConst::Var(rhs)) if dst.ty() == ValType::I32 => vec![(dst, lhs), (dst, rhs)],
			SsaInstr::Add(dst, SsaVarOrConst::Var(lhs), _) if dst.ty() == ValType::I32 => vec![(dst, lhs)],
			SsaInstr::Add(dst, _, SsaVarOrConst::Var(rhs)) if dst.ty() == ValType::I32 => vec![(dst, rhs)],

			SsaInstr::Assign(dst, SsaVarOrConst::Var(src)) => vec![(dst, src)],

			SsaInstr::Mul(dst, lhs, _rhs) if dst.ty() == ValType::I32 => vec![(dst, lhs), /*(dst, rhs)*/],
			SsaInstr::ShrS(dst, lhs, _) if dst.ty() == ValType::I32 => vec![(dst, lhs)],
			SsaInstr::ShrU(dst, lhs, _) if dst.ty() == ValType::I32 => vec![(dst, lhs)],
			SsaInstr::Shl(dst, lhs, _) if dst.ty() == ValType::I32 => vec![(dst, lhs)],
			SsaInstr::Xor(dst, lhs, _) => vec![(dst, lhs)],

			SsaInstr::And(dst, lhs, SsaVarOrConst::Const(c))
				if dst.ty() == ValType::I32 && is_simple_and_mask(c.into_i32().unwrap()) => vec![(dst, lhs)],

			_ => Vec::new(), 
		}
	}

	pub fn renumber_uses(&mut self, old: SsaVar, new: SsaVar) {
		match self {
			SsaInstr::I32Set(_, _) => {}
			SsaInstr::I64Set(_, _) => {}

			SsaInstr::TurtleSetX(src) |
			SsaInstr::TurtleSetY(src) |
			SsaInstr::TurtleSetZ(src) |
			SsaInstr::Load64(_, _, src) |
			SsaInstr::Load32S(_, _, src) |
			SsaInstr::Load32U(_, _, src) |
			SsaInstr::Load16S(_, _, src) |
			SsaInstr::Load16U(_, _, src) |
			SsaInstr::Load8S(_, _, src) |
			SsaInstr::Load8U(_, _, src) |
			SsaInstr::Assign(_, src) => {
				if let SsaVarOrConst::Var(v) = src {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}
			}

			SsaInstr::Add(_, lhs, rhs) |
			SsaInstr::Sub(_, lhs, rhs) |
			SsaInstr::GtS(_, lhs, rhs) |
			SsaInstr::GtU(_, lhs, rhs) |
			SsaInstr::GeS(_, lhs, rhs) |
			SsaInstr::GeU(_, lhs, rhs) |
			SsaInstr::LtS(_, lhs, rhs) |
			SsaInstr::LtU(_, lhs, rhs) |
			SsaInstr::LeS(_, lhs, rhs) |
			SsaInstr::LeU(_, lhs, rhs) |
			SsaInstr::Eq(_, lhs, rhs) |
			SsaInstr::Ne(_, lhs, rhs) => {
				if let SsaVarOrConst::Var(v) = lhs {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}

				if let SsaVarOrConst::Var(v) = rhs {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}
			}

			SsaInstr::TurtleSetBlock(src) |
			SsaInstr::GlobalSet(_, src) |
			SsaInstr::LocalSet(_, src) |
			SsaInstr::PrintInt(src) |
			SsaInstr::PutChar(src) |
			SsaInstr::Extend8S(_, src) |
			SsaInstr::Extend16S(_, src) |
			SsaInstr::Extend32S(_, src) |
			SsaInstr::Extend32U(_, src) |
			SsaInstr::Clz(_, src) |
			SsaInstr::Ctz(_, src) |
			SsaInstr::Eqz(_, src) |
			SsaInstr::Wrap(_, src) |
			SsaInstr::Popcnt(_, src) => {
				if src.0 == old.0 {
					src.0 = new.0;
				}
			}

			SsaInstr::Store64(_, lhs, rhs) |
			SsaInstr::Store32(_, lhs, rhs) |
			SsaInstr::Store16(_, lhs, rhs) |
			SsaInstr::Store8(_, lhs, rhs) |
			SsaInstr::Mul(_, lhs, rhs) |
			SsaInstr::DivS(_, lhs, rhs) |
			SsaInstr::DivU(_, lhs, rhs) |
			SsaInstr::RemS(_, lhs, rhs) |
			SsaInstr::RemU(_, lhs, rhs) |
			SsaInstr::Shl(_, lhs, rhs) |
			SsaInstr::ShrS(_, lhs, rhs) |
			SsaInstr::ShrU(_, lhs, rhs) |
			SsaInstr::Xor(_, lhs, rhs) |
			SsaInstr::And(_, lhs, rhs) |
			SsaInstr::Or(_, lhs, rhs) => {
				if lhs.0 == old.0 {
					lhs.0 = new.0;
				}

				if let SsaVarOrConst::Var(v) = rhs {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}
			}

			SsaInstr::Rotl(_, lhs, _) => todo!(),
			SsaInstr::Rotr(_, lhs, _) => todo!(),

			SsaInstr::GlobalGet(_, _) => {}
			SsaInstr::LocalGet(_, _) => {}
			SsaInstr::ParamGet(_, _) => {}
			SsaInstr::TurtleGetBlock(_) => {}
			SsaInstr::TurtleCopy => {}
			SsaInstr::TurtlePaste => {}

			SsaInstr::Select { dst: _, true_var, false_var, cond } => {
				if cond.0 == old.0 {
					cond.0 = new.0;
				}

				if let SsaVarOrConst::Var(v) = true_var {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}

				if let SsaVarOrConst::Var(v) = false_var {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}
			}

			SsaInstr::CallIndirect { params, table_entry, .. } => {
				for param in params {
					if param.0 == old.0 {
						param.0 = new.0;
					}
				}

				if table_entry.0 == old.0 {
					table_entry.0 = new.0;
				}
			}

			SsaInstr::Call { params, .. } => {
				for param in params {
					if param.0 == old.0 {
						param.0 = new.0;
					}
				}
			}

			SsaInstr::Memset { dest, value, length, result: _ } => {
				if dest.0 == old.0 {
					dest.0 = new.0;
				}

				if value.0 == old.0 {
					value.0 = new.0;
				}

				if length.0 == old.0 {
					length.0 = new.0;
				}
			}

			SsaInstr::TurtleFillBlock { block, x_span, y_span, z_span } => {
				if let SsaVarOrConst::Var(v) = block {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}
				if let SsaVarOrConst::Var(v) = x_span {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}
				if let SsaVarOrConst::Var(v) = y_span {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}
				if let SsaVarOrConst::Var(v) = z_span {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}
			}
			SsaInstr::TurtleCopyRegion { x_span, y_span, z_span } |
			SsaInstr::TurtlePasteRegionMasked { x_span, y_span, z_span } => {
				if let SsaVarOrConst::Var(v) = x_span {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}
				if let SsaVarOrConst::Var(v) = y_span {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}
				if let SsaVarOrConst::Var(v) = z_span {
					if v.0 == old.0 {
						v.0 = new.0;
					}
				}

			}
		}

	}
}

impl fmt::Display for SsaInstr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			SsaInstr::I32Set(dst, val) => write!(f, "{dst} <- {val}_i32"),
			SsaInstr::I64Set(dst, val) => write!(f, "{dst} <- {val}_i32"),
			SsaInstr::Assign(dst, src) => write!(f, "{dst} <- {src:?}"),
			SsaInstr::ParamGet(dst, idx) => write!(f, "{dst} <- parameter {idx}"),
			_ => todo!(),
		}
    }
}

fn is_simple_and_mask(i: i32) -> bool {
	i == 0 || i.leading_zeros() + i.trailing_ones() == 32
}

#[derive(Debug, Clone)]
pub struct JumpTarget {
	pub label: BlockId,
}

impl fmt::Display for JumpTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "B{}.{}", self.label.func, self.label.block)
    }
}

#[derive(Debug, Clone)]
pub enum SsaTerminator {
	Unreachable,
	ScheduleJump(JumpTarget, u32),
	Jump(JumpTarget),
	BranchIf { cond: TypedSsaVar, true_target: JumpTarget, false_target: JumpTarget },
	BranchTable { cond: TypedSsaVar, default: JumpTarget, arms: Vec<JumpTarget> },
	Return(Vec<TypedSsaVar>),
}

impl SsaTerminator {
	pub fn uses(&self) -> Vec<TypedSsaVar> {
		match self {
			SsaTerminator::Unreachable => vec![],
			SsaTerminator::ScheduleJump(..) => Vec::new(),
			SsaTerminator::Jump(_) => Vec::new(),
			SsaTerminator::BranchIf { cond, .. } => {
				vec![*cond]
			}
			SsaTerminator::BranchTable { cond, .. } => {
				vec![*cond]
			}
			SsaTerminator::Return(vars) => vars.clone(),
		}

	}

	pub fn successors(&self) -> Vec<BlockId> {
		match self {
			SsaTerminator::Unreachable => Vec::new(),
			SsaTerminator::Jump(t) => vec![t.label],
			SsaTerminator::ScheduleJump(t, _) => vec![t.label],
			SsaTerminator::BranchIf { cond: _, true_target, false_target } => {
				vec![true_target.label, false_target.label]
			}
			SsaTerminator::BranchTable { cond: _, default, arms } => {
				let mut result = vec![default.label];
				result.extend(arms.iter().map(|t| t.label));
				result
			}
			SsaTerminator::Return(_) => Vec::new(),
		}
	}

	pub fn targets(&self) -> Vec<JumpTarget> {
		match self {
			SsaTerminator::Unreachable | SsaTerminator::Return(_) => Vec::new(),
			SsaTerminator::ScheduleJump(target, _) |
			SsaTerminator::Jump(target) => vec![target.clone()],
			SsaTerminator::BranchIf { cond: _, true_target, false_target } => {
				vec![true_target.clone(), false_target.clone()]
			}
			SsaTerminator::BranchTable { cond: _, default, arms } => {
				let mut result = vec![default.clone()];
				result.extend(arms.iter().cloned());
				result
			}
		}
	}

	pub fn renumber_target(&mut self, old: usize, new: usize) {
		match self {
			SsaTerminator::Unreachable => {},
			SsaTerminator::ScheduleJump(t, _) |
			SsaTerminator::Jump(t) => {
				if t.label.block == old {
					t.label.block = new;
				}
			}
			SsaTerminator::BranchIf { cond: _, true_target, false_target } => {
				if true_target.label.block == old {
					true_target.label.block = new;
				}

				if false_target.label.block == old {
					false_target.label.block = new;
				}
			}
			SsaTerminator::BranchTable { cond:_ , default, arms } => {
				if default.label.block == old {
					default.label.block = new;
				}

				for arm in arms {
					if arm.label.block == old {
						arm.label.block = new;
					}
				}
			}
			SsaTerminator::Return(_) => {},
		}
	}

	pub fn renumber_uses(&mut self, old: SsaVar, new: SsaVar) {
		match self {
			SsaTerminator::Unreachable => {}
			SsaTerminator::ScheduleJump(_, _) => {}
			SsaTerminator::Jump(_) => {}
			SsaTerminator::BranchIf { cond, .. } |
			SsaTerminator::BranchTable { cond, .. } => {
				if cond.0 == old.0 {
					cond.0 = new.0;
				}
			}
			SsaTerminator::Return(vars) => {
				for var in vars {
					if var.0 == old.0 {
						var.0 = new.0;
					}
				}
			}
		}
	}

}

impl fmt::Display for SsaTerminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			SsaTerminator::Unreachable => write!(f, "unreachable"),
			SsaTerminator::ScheduleJump(target, delay) => write!(f, "delay {delay} jump {target}"),
			SsaTerminator::Jump(target) => write!(f, "jump {target}"),
			SsaTerminator::BranchIf { cond, true_target, false_target } => {
				write!(f, "if {cond} then {true_target} else {false_target}")
			}
			SsaTerminator::BranchTable { .. } => todo!(),
			SsaTerminator::Return(args) => {
				write!(f, "return (")?;
				for (idx, a) in args.iter().enumerate() {
					write!(f, "{a}")?;
					if idx + 1 != args.len() {
						write!(f, ", ")?;
					}
				}
				write!(f, ")")
			}
		}
    }
}

pub struct SsaFunction {
	pub code: LocalBlockMap<SsaBasicBlock>,
	pub params: Box<[ValType]>,
	pub returns: Box<[ValType]>,
}

impl SsaFunction {
	pub fn new<C>(blocks: C, params: Box<[ValType]>, returns: Box<[ValType]>) -> Self
		where C: IntoIterator<Item=(BlockId, SsaBasicBlock)>
	{
		let code = blocks.into_iter().collect();

		SsaFunction { code, params, returns }
	}

	pub fn iter<'a>(&'a self) -> impl Iterator<Item=(BlockId, &'a SsaBasicBlock)> + 'a {
		self.code.iter()
	}

	pub fn iter_mut<'a>(&'a mut self) -> impl Iterator<Item=(BlockId, &'a mut SsaBasicBlock)> + 'a {
		self.code.iter_mut()
	}

	pub fn get(&self, block_id: BlockId) -> &SsaBasicBlock {
		self.code.get(block_id).unwrap()
	}

	pub fn get_mut(&mut self, block_id: BlockId) -> &mut SsaBasicBlock {
		self.code.get_mut(block_id).unwrap()
	}

	pub fn func_id(&self) -> u32 {
		self.code.func_id() as u32
	}

	pub fn entry_point_id(&self) -> BlockId {
		let func = self.func_id() as usize;
		BlockId { func, block: 0 }
	}

	pub fn add_block(&mut self) -> BlockId {
		let b = self.iter().map(|(id, _)| id.block + 1).max().unwrap_or(0);

		let block_id = BlockId {
			func: self.func_id() as usize,
			block: b,
		};

		let block = SsaBasicBlock::default();

		self.code.insert(block_id, block);

		block_id
	}

	/*pub fn coalescable_term_vars(&self, source_id: BlockId) -> Vec<(TypedSsaVar, TypedSsaVar)> {
		let mut result = Vec::new();
		let source = self.get(source_id);
		match &source.term {
			SsaTerminator::Unreachable => {},
			SsaTerminator::ScheduleJump(t, _) => { assert!(t.params.is_empty()); },
			SsaTerminator::Jump(t) => {
				let target_params = &self.get(t.label).params;
				result.extend(target_params.iter().copied().zip(t.params.iter().copied()));
			},
			SsaTerminator::BranchIf { cond: _, true_target, false_target } => {
				let t_params = &self.get(true_target.label).params;
				let f_params = &self.get(false_target.label).params;
				result.extend(t_params.iter().copied().zip(true_target.params.iter().copied()));
				result.extend(f_params.iter().copied().zip(false_target.params.iter().copied()));
			},
			SsaTerminator::BranchTable { cond: _, default, arms } => {
				let d_params = &self.get(default.label).params;
				result.extend(d_params.iter().copied().zip(default.params.iter().copied()));

				for arm in arms.iter() {
					let a_params = &self.get(arm.label).params;
					result.extend(a_params.iter().copied().zip(arm.params.iter().copied()));
				}
			},
			SsaTerminator::Return(_) => {},
		}
		result
	}*/
}

pub struct SsaProgram {
	pub local_types: HashMap<usize, Vec<ValType>>,
	pub globals: Vec<TypedValue>,
	pub memory: Vec<Memory>,
	pub tables: Vec<Table>,
	pub code: Vec<SsaFunction>,
	pub exports: HashMap<String, BlockId>,
}

impl SsaProgram {
	pub fn get_func(&self, func_id: u32) -> &SsaFunction {
		self.code.iter().find(|f| f.func_id() == func_id).unwrap_or_else(|| panic!("couldn't find function {:?}", func_id))
	}
}

pub struct Memory {
	pub data: Vec<u8>,
	pub maximum: Option<usize>,
}

impl Memory {
	pub fn new(initial: usize, maximum: Option<usize>) -> Memory {
		Memory {
			data: vec![0; 65536 * initial],
			maximum,
		}
	}

	pub fn store(&mut self, addr: usize, bytes: &[u8]) {
		let dest = &mut self.data[addr..][..bytes.len()];
		dest.copy_from_slice(bytes);
	}

	pub fn load(&self, addr: usize, len: usize) -> &[u8] {
		&self.data[addr..][..len]
	}
}

#[derive(Debug)]
pub struct Table {
	pub max: Option<usize>,
	pub elements: Vec<Option<usize>>,
}


/// Determines if an SSA-form function has any critical edges.
/// Returns the first critical edge found, if any.
///
/// A critical edge is an edge from blocks A -> B,
/// where A has multiple successors and B has multiple predecessors.
pub fn no_critical_edges(func: &SsaFunction, preds: &PredInfo) -> Result<(), (BlockId, BlockId)> {
    for (block_b_index, block_b) in func.iter() {
        let num_predecessors = preds.get_predecessors(block_b_index).len();
        if num_predecessors <= 1 {
            // This block can't end a critical edge because it doesn't have multiple predecessors.
            continue;
        }

		let predecessors = preds.get_predecessors(block_b_index);
        for &block_a_index in predecessors {
            let block_a = func.get(block_a_index);
			if block_a.term.successors().len() > 1 {
				// A -> B is a critical edge.
                return Err((block_a_index, block_b_index));
			}
        }
    }

    Ok(())
}

pub fn split_critical_edges(func: &mut SsaFunction) {
	loop {
		let preds = PredInfo::new(func);
		let Err((src, dst)) = no_critical_edges(func, &preds) else { break };

		let split_block_id = func.add_block();

		func.get_mut(src).term.renumber_target(dst.block, split_block_id.block);

		func.get_mut(dst).phi_node.rename_source(src.block, split_block_id.block);

		let split_block = func.get_mut(split_block_id);

		split_block.term = SsaTerminator::Jump(JumpTarget {
			label: dst,
		});
	}
}

/// Checks that phi nodes have exactly the edges corresponding to their actual predecessors.
///
/// For example, block 2 in the following case must have phi node edges 0 and 1.
/// No more, no less.
/// ```
/// 0       1
/// |       |
/// |       |
/// +-> 2 <-+
/// ```
pub fn phi_nodes_are_coherent(func: &SsaFunction, preds: &PredInfo) -> Result<(), BlockId> {
    for (block_id, block) in func.iter() {
        let predecessors = &preds.get_predecessors(block_id);

		if block.phi_node.is_empty() {
			continue;
		}

        if predecessors.len() <= 1 {
            // Blocks with only a single predecessor cannot have phi nodes.
            return Err(block_id);
        }

		if block.phi_node.sources().count() != predecessors.len() {
			todo!("{:?} {:?}", block.phi_node.sources().count(), predecessors.len());
			return Err(block_id);
		}

        for (source, _) in block.phi_node.sources() {
			if !predecessors.iter().any(|p| p.block == source) {
				return Err(block_id);
			}
        }
    }

    Ok(())
}

pub fn remove_redundant_phi_nodes(func: &mut SsaFunction) {
	let keys = func.code.keys().collect::<Vec<_>>();
    for block_id in keys {
        let mut renumber_time = std::time::Duration::default();

        let mut phi_node_idx = 0;
        while phi_node_idx != func.get(block_id).phi_node.dests.len() {
            let node = func.get(block_id).phi_node.arms().nth(phi_node_idx).unwrap();
            let (other_temp, is_redundant) = phi_arm_is_redundant(node);
            if !is_redundant {
                phi_node_idx += 1;
                continue;
            }

            // Swap remove is okay here because it
            // replaces the removed element with an element we haven't seen yet,
            // so we'll still visit all the nodes exactly once.

            if let Some(other_temp) = other_temp {
                let redundant_temp = node.dest();
                func.get_mut(block_id).phi_node.swap_remove_arm(phi_node_idx);

                let t1 = std::time::SystemTime::now();

                //func.renumber_temp(redundant_temp, other_temp);

                for (_, block) in func.iter_mut() {
					for arm in 0..block.phi_node.dests.len() {
						let mut arm = block.phi_node.arm_mut(arm);
                        for (_, source) in arm.sources_mut() {
							if *source == redundant_temp {
								*source = other_temp;
							}
                        }
                    }

                    for instr in block.body.iter_mut() {
                        instr.renumber_uses(redundant_temp.into_untyped(), other_temp.into_untyped());
                    }

                    block.term.renumber_uses(redundant_temp.into_untyped(), other_temp.into_untyped());
                }

                renumber_time += t1.elapsed().unwrap();
            } else {
                // The phi node is of the form `x1 <- phi(x1, x1, ..., x1)`
                // and thus can be completely deleted.
                func.get_mut(block_id).phi_node.swap_remove_arm(phi_node_idx);
            }
        }

        //println!("Renumbering {} vars", renames.len());

        /*for (redundant_temp, other_temp) in renames {
            func.renumber_temp(redundant_temp, other_temp);
        }*/

        //println!("renumber time: {} us", renumber_time.as_micros());
    }
}

pub fn phi_arm_is_redundant(node: PhiArm) -> (Option<TypedSsaVar>, bool) {
    let mut other = None;

    for (_, source) in node.sources() {
		if source != node.dest() {
			if let Some(other) = other {
				if other != source {
					return (None, false);
				}
			} else {
				other = Some(source);
			}
		}
    }

    (other, true)
}
