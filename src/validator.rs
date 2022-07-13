use std::{ops::{Index, IndexMut}, collections::HashMap};

use wasmparser::{Type, Operator, MemoryImmediate, DataKind, ElementKind, ElementItem, ExternalKind};

use crate::{wasm_file::{WasmFile, eval_init_expr_single}, ssa::{SsaBasicBlock, BlockId, SsaTerminator, TypedSsaVar, SsaInstr, SsaVarAlloc, JumpTarget, SsaProgram, SsaFunction, Memory, Table, SsaVarOrConst}};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UncertainVar {
	Unknown,
	Known(TypedSsaVar),
}

impl UncertainVar {
	pub fn ty(self) -> UncertainType {
		match self {
			UncertainVar::Unknown => UncertainType::Unknown,
			UncertainVar::Known(v) => UncertainType::Known(v.ty()),
		}
	}

	pub fn unwrap(self) -> TypedSsaVar {
		if let UncertainVar::Known(v) = self {
			v
		} else {
			panic!()
		}
	}
}

impl From<TypedSsaVar> for UncertainVar {
	fn from(v: TypedSsaVar) -> Self {
		UncertainVar::Known(v)
	}
}

impl From<UncertainVar> for Option<TypedSsaVar> {
	fn from(v: UncertainVar) -> Self {
		match v {
			UncertainVar::Unknown => None,
			UncertainVar::Known(v) => Some(v),
		}
	}
}

use UncertainVar::Unknown as UnknownVar;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UncertainType {
	Unknown,
	Known(Type),
}

impl UncertainType {
	pub fn matches(self, other: UncertainType) -> bool {
		if let (UncertainType::Known(lhs), UncertainType::Known(rhs)) = (self, other) {
			lhs == rhs
		} else {
			true
		}
	}
}

use UncertainType::Unknown as UnknownType;

impl From<Type> for UncertainType {
    fn from(ty: Type) -> Self {
	    UncertainType::Known(ty)
    }
}

#[derive(Default, Debug)]
struct ValueStack(Vec<UncertainVar>);

impl ValueStack {
	pub fn push<T>(&mut self, ty: T)
		where T: Into<UncertainVar>
	{
		self.0.push(ty.into())
	}

	pub fn push_many<T: Copy + Into<UncertainVar>>(&mut self, tys: &[T]) {
		self.0.extend(tys.iter().map(|t| (*t).into()));
	}

	pub fn pop(&mut self, control_stack: &ControlStack) -> UncertainVar {
		if let Some(ctrl_frame) = control_stack.top() {
			if self.0.len() == ctrl_frame.height && ctrl_frame.unreachable {
				return UnknownVar;
			}

			assert_ne!(self.0.len(), ctrl_frame.height);
		}

		self.0.pop().unwrap()
	}

	pub fn pop_ty<T: Into<UncertainType>>(&mut self, ty_to_pop: T, control_stack: &ControlStack) -> UncertainVar {
		let ty_to_pop = ty_to_pop.into();
		let var = self.pop(control_stack);
		assert!(var.ty() == ty_to_pop || var == UnknownVar || ty_to_pop == UnknownType);
		var
	}

	pub fn pop_many<T: Copy + Into<UncertainType>>(&mut self, tys: &[T], control_stack: &ControlStack) -> Vec<UncertainVar> {
		let mut result = Vec::new();
		for &ty_to_pop in tys.iter().rev() {
			let ty_to_pop = ty_to_pop.into();
			result.push(self.pop_ty(ty_to_pop, control_stack));
		}
		result.reverse();
		result
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ControlOp {
	Loop(BlockId),
	Block(BlockId),
	If { start_vars: Vec<UncertainVar>, false_label: BlockId, next_label: BlockId },
	Else { next_label: BlockId },
}

impl ControlOp {
	pub fn target_label(&self) -> BlockId {
		match self {
			ControlOp::Loop(l) => *l,
			ControlOp::Block(l) => *l,
			ControlOp::If { start_vars: _, false_label: _, next_label } => *next_label,
			ControlOp::Else { next_label } => *next_label,
		}
	}
}

#[derive(Clone, Debug)]
struct ControlFrame {
	operator: ControlOp,
	start_types: Box<[Type]>,
	end_types: Box<[Type]>,
	height: usize,
	unreachable: bool,
}

impl ControlFrame {
	pub fn label_types(&self) -> &[Type] {
		if matches!(self.operator, ControlOp::Loop(_)) {
			&self.start_types
		} else {
			&self.end_types
		}
	}
}

struct TopIndex(usize);

#[derive(Default, Debug)]
struct ControlStack(Vec<ControlFrame>);

impl ControlStack {
	pub fn push(&mut self, operator: ControlOp, start_vars: &[UncertainVar], start_types: Box<[Type]>, end_types: Box<[Type]>, value_stack: &mut ValueStack) {
		let height = value_stack.0.len();

		value_stack.push_many(start_vars);

		let frame = ControlFrame {
			operator,
			start_types,
			end_types,
			height,
			unreachable: false
		};

		self.0.push(frame);
	}

	pub fn pop(&mut self, value_stack: &mut ValueStack) -> (Vec<UncertainVar>, ControlFrame) {
		let frame = self.top().unwrap();
		let end_types = frame.end_types.iter().map(|t| UncertainType::Known(*t)).collect::<Vec<_>>();
		let height = frame.height;

		let vals = value_stack.pop_many(&end_types, self);
		assert_eq!(value_stack.0.len(), height);

		(vals, self.0.pop().unwrap())
	}

	pub fn mark_unreachable(&mut self, value_stack: &mut ValueStack) {
		let top = self.0.last_mut().unwrap();
		value_stack.0.truncate(top.height);
		top.unreachable = true;
	}

	pub fn reachable(&self) -> bool {
		if let Some(top) = self.top() {
			!top.unreachable
		} else {
			true
		}
	}


	pub fn top(&self) -> Option<&ControlFrame> {
		self.0.last()
	}
}

impl<'a> Index<TopIndex> for ControlStack {
    type Output = ControlFrame;

    fn index(&self, index: TopIndex) -> &Self::Output {
	assert!(!self.0.is_empty());
	let i = self.0.len() - 1 - index.0;
	&self.0[i]
    }
}

impl<'a> IndexMut<TopIndex> for ControlStack {
    fn index_mut(&mut self, index: TopIndex) -> &mut Self::Output {
	    assert!(!self.0.is_empty());
	    let i = self.0.len() - 1 - index.0;
	    &mut self.0[i]
    }
}

struct PartialBasicBlock {
	block: SsaBasicBlock,
	finished: bool,
	count: i32,
	has_new_locals: bool,
	starting_locals: Vec<TypedSsaVar>,
}

impl PartialBasicBlock {
	pub fn new(old_locals: &[TypedSsaVar]) -> Self {
		PartialBasicBlock {
			block: SsaBasicBlock::default(),
			finished: false,
			count: -1,
			has_new_locals: false,
			starting_locals: old_locals.to_owned(),
		}
	}

	pub fn new_with_locals(locals: &[Type], alloc: &mut SsaVarAlloc) -> Self {
		let new_locals = locals.iter().map(|&ty| alloc.new_typed(ty)).collect::<Vec<_>>();

		let mut block = SsaBasicBlock::default();
		block.params.extend(new_locals.iter().copied());

		PartialBasicBlock { block, finished: false, count: -1, has_new_locals: true, starting_locals: new_locals }
	}

	pub fn starting_locals(&self) -> &[TypedSsaVar] {
		&self.starting_locals
	}
}

struct SsaFuncBuilder {
	blocks: Vec<PartialBasicBlock>,
	counter: usize,
	current_block: usize,
	current_locals: Vec<TypedSsaVar>,
	func: usize,
}

impl SsaFuncBuilder {
	pub fn new(func: usize) -> Self {
		SsaFuncBuilder {
			blocks: Vec::new(),
			current_block: 0,
			counter: 0,
			current_locals: Vec::new(),
			func,
		}
	}

	pub fn alloc_block(&mut self) -> BlockId {
		let block = self.blocks.len();
		self.blocks.push(PartialBasicBlock::new(&self.current_locals));
		BlockId { func: self.func, block }
	}

	pub fn alloc_block_with_locals(&mut self, locals: &[Type], var_alloc: &mut SsaVarAlloc) -> BlockId {
		let block = self.blocks.len();
		self.blocks.push(PartialBasicBlock::new_with_locals(locals, var_alloc));
		BlockId { func: self.func, block }
	}

	pub fn set_block(&mut self, block_id: BlockId) {
		assert_eq!(self.func, block_id.func);
		self.current_block = block_id.block;
		assert_eq!(self.blocks[self.current_block].count, -1);
		self.blocks[self.current_block].count = self.counter as i32;
		self.current_locals = self.blocks[self.current_block].starting_locals().to_owned();
		self.counter += 1;
	}

	/*pub fn get_local_vars(&self, block_id: BlockId, locals: &[Type]) -> &[TypedSsaVar] {
		let part = &self.blocks[block_id.block];
		if part.has_new_locals {
			&self.get(block_id).params[..locals.len()]
		} else {
			&[]
		}
	}

	pub fn current_local_vars(&self, locals: &[Type]) -> &[TypedSsaVar] {
		let id = BlockId { func: self.func, block: self.current_block };
		self.get_local_vars(id, locals)
	}*/

	pub fn current_block_mut(&mut self) -> &mut SsaBasicBlock {
		let id = BlockId { func: self.func, block: self.current_block };
		self.get_mut(id)
	}

	pub fn get(&self, block_id: BlockId) -> &SsaBasicBlock {
		assert_eq!(self.func, block_id.func);
		&self.blocks[block_id.block].block
	}

	pub fn get_mut(&mut self, block_id: BlockId) -> &mut SsaBasicBlock {
		assert_eq!(self.func, block_id.func);
		let block = &mut self.blocks[block_id.block];
		assert!(!block.finished);
		&mut block.block
	}

	pub fn finish_block(&mut self, term: SsaTerminator) {
		let block = &mut self.blocks[self.current_block];
		assert!(!block.finished);

		block.block.term = term;
		block.finished = true;
	}

	pub fn finish(self) -> (usize, Vec<(BlockId, SsaBasicBlock)>) {
		let mut blocks = self.blocks.into_iter()
			.enumerate()
			.map(|(idx, part_block)| {
				assert_ne!(part_block.count, -1);
				assert!(part_block.finished);
				let id = BlockId { func: self.func, block: idx };
				(part_block.count, id, part_block.block)
			})
			.collect::<Vec<_>>();
		
		blocks.sort_by_key(|(c, _, _)| *c);

		blocks.iter().enumerate().for_each(|(i, (c, _, _))| assert_eq!(i as i32, *c));

		let blocks = blocks.into_iter().map(|(_, id, block)| (id, block)).collect();

		(self.func, blocks)
	}
}

#[derive(Default, Debug)]
struct Validator {
	value_stack: ValueStack,
	control_stack: ControlStack,
}

impl Validator {
	pub fn push_value<T>(&mut self, ty: T)
		where T: Into<UncertainVar>
	{
		self.value_stack.push(ty.into())
	}

	pub fn push_values<T>(&mut self, ty: &[T])
		where T: Copy + Into<UncertainVar>
	{
		self.value_stack.push_many(ty)
	}

	pub fn pop_value(&mut self) -> UncertainVar {
		self.value_stack.pop(&self.control_stack)
	}

	pub fn pop_push_values<T, U>(&mut self, input: &[T], output: &[U])
		where
			T: Copy + Into<UncertainType>,
			U: Copy + Into<UncertainVar>
	{
		self.value_stack.pop_many(input, &self.control_stack);
		self.value_stack.push_many(output);
	}

	pub fn pop_value_ty(&mut self, ty_to_pop: UncertainType) -> UncertainVar {
		self.value_stack.pop_ty(ty_to_pop, &self.control_stack)
	}

	pub fn pop_values<T: Copy + Into<UncertainType>>(&mut self, tys: &[T]) -> Vec<UncertainVar> {
		self.value_stack.pop_many(tys, &self.control_stack)
	}
	
	pub fn push_ctrl(&mut self, operator: ControlOp, start_vars: &[UncertainVar], start_types: Box<[Type]>, end_types: Box<[Type]>) {
		self.control_stack.push(operator, start_vars, start_types, end_types, &mut self.value_stack);
	}

	pub fn pop_ctrl(&mut self) -> (Vec<UncertainVar>, ControlFrame) {
		self.control_stack.pop(&mut self.value_stack)
	}

	pub fn mark_unreachable(&mut self) {
		self.control_stack.mark_unreachable(&mut self.value_stack);
	}

	pub fn reachable(&self) -> bool {
		self.control_stack.reachable()
	}
}

fn make_params(builder: &mut SsaFuncBuilder, validator: &mut Validator, t: &[Type], alloc: &mut SsaVarAlloc) {
	//assert!(builder.current_block_mut().params.is_empty());

	validator.pop_values(t);
	for &ty in t {
		let val = alloc.new_typed(ty);
		validator.push_value(val);
		builder.current_block_mut().params.push(val);
	}
}

macro_rules! zip_vars {
	( $( $opt:ident ),* ) => {
		zip_vars!( $( $opt , )* )
	};
	( $( $opt:ident, )* ) => {
		match ( $( $opt , )* ) {
			( $( UncertainVar::Known ( $opt ) , )* ) => Some ( ( $( $opt , )* ) ),
			_ => None
		}
	};
}

macro_rules! generic_zip_all {
	( $somevar:path , $nonevar:path , $( $opt:expr ),* ) => {
		zip_all!( $somevar , $nonevar , $( $opt , )* )
	};
	( $somevar:path , $nonevar:path , $( $opt:expr , )* ) => {
		match ( $( $opt , )* ) {
			( $( $somevar ( $opt ) )* ) => $somevar ( $( $opt , )* ),
			_ => $nonevar
		}
	};
}


macro_rules! generic_zip_all {
	( $somevar:path , $nonevar:path , $( $opt:expr ),* ) => {
		zip_all!( $somevar , $nonevar , $( $opt , )* )
	};
	( $somevar:path , $nonevar:path , $( $opt:expr , )* ) => {
		match ( $( $opt , )* ) {
			( $( $somevar ( $opt ) )* ) => $somevar ( $( $opt , )* ),
			_ => $nonevar
		}
	};
}

macro_rules! zip_all {
	( $($in:tt)* ) => {
		generic_zip_all!( Some, None, $($in)* )
	};
}

struct ValidationState<'a> {
	wasm_file: &'a WasmFile<'a>,
	func: usize,
	builder: SsaFuncBuilder,
	alloc: SsaVarAlloc,
	validator: Validator,
}

impl ValidationState<'_> {
	pub fn visit_operator(&mut self, op: &Operator) {
		let locals = self.wasm_file.func_locals(self.func);

		let wasm_file = &self.wasm_file;
		let builder = &mut self.builder;
		let alloc = &mut self.alloc;
		let validator = &mut self.validator;

		fn make_i32_binop<F, L, R>(f: F, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(TypedSsaVar, L, R) -> SsaInstr,
				L: From<TypedSsaVar>,
				R: From<TypedSsaVar>,
		{
			let rhs = validator.pop_value_ty(Type::I32.into());
			let lhs = validator.pop_value_ty(Type::I32.into());

			let dst = alloc.new_i32();
			validator.push_value(dst);

			if let Some((lhs, rhs)) = zip_vars!(lhs, rhs) {
				builder.current_block_mut().body.push(f(dst, lhs.into(), rhs.into()));
			}
		}

		fn make_i64_binop<F, L, R>(f: F, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(TypedSsaVar, L, R) -> SsaInstr,
				L: From<TypedSsaVar>,
				R: From<TypedSsaVar>,
		{
			let rhs = validator.pop_value_ty(Type::I64.into());
			let lhs = validator.pop_value_ty(Type::I64.into());

			let dst = alloc.new_i64();
			validator.push_value(dst);

			if let Some((lhs, rhs)) = zip_vars!(lhs, rhs) {
				builder.current_block_mut().body.push(f(dst, lhs.into(), rhs.into()));
			}
		}

		fn make_i64_comp<F>(f: F, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(TypedSsaVar, SsaVarOrConst, SsaVarOrConst) -> SsaInstr
		{
			let rhs = validator.pop_value_ty(Type::I64.into());
			let lhs = validator.pop_value_ty(Type::I64.into());

			let dst = alloc.new_i32();
			validator.push_value(dst);

			if validator.reachable() {
				builder.current_block_mut().body.push(f(dst, lhs.unwrap().into(), rhs.unwrap().into()));
			}
		}

		fn make_i32_unaryop<F>(f: F, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(TypedSsaVar, TypedSsaVar) -> SsaInstr
		{
			let src = validator.pop_value_ty(Type::I32.into());
			let dst = alloc.new_i32();
			validator.push_value(dst);

			if let Some(src) = src.into() {
				builder.current_block_mut().body.push(f(dst, src));
			}
		}

		fn make_i64_unaryop<F>(f: F, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(TypedSsaVar, TypedSsaVar) -> SsaInstr
		{
			let src = validator.pop_value_ty(Type::I64.into());
			let dst = alloc.new_i64();
			validator.push_value(dst);

			if validator.reachable() {
				builder.current_block_mut().body.push(f(dst, src.unwrap()));
			}
		}

		fn make_i32_load<F>(f: F, memarg: MemoryImmediate, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(MemoryImmediate, TypedSsaVar, SsaVarOrConst) -> SsaInstr,
		{
			let addr = validator.pop_value_ty(Type::I32.into());
			let dst = alloc.new_i32();
			validator.push_value(dst);

			if validator.reachable() {
				builder.current_block_mut().body.push(f(memarg, dst, addr.unwrap().into()));
			}
		}

		fn make_i64_load<F>(f: F, memarg: MemoryImmediate, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(MemoryImmediate, TypedSsaVar, SsaVarOrConst) -> SsaInstr,
		{
			let addr = validator.pop_value_ty(Type::I32.into());
			let dst = alloc.new_i64();
			validator.push_value(dst);
			
			if validator.reachable() {
				builder.current_block_mut().body.push(f(memarg, dst, addr.unwrap().into()));
			}
		}

		fn make_i32_store<F>(f: F, memarg: MemoryImmediate, builder: &mut SsaFuncBuilder, validator: &mut Validator, _: &mut SsaVarAlloc)
			where
				F: FnOnce(MemoryImmediate, TypedSsaVar, SsaVarOrConst) -> SsaInstr,
		{
			let src = validator.pop_value_ty(Type::I32.into());
			let addr = validator.pop_value_ty(Type::I32.into());

			if validator.reachable() {
				builder.current_block_mut().body.push(f(memarg, src.unwrap(), addr.unwrap().into()));
			}
		}

		fn make_i64_store<F>(f: F, memarg: MemoryImmediate, builder: &mut SsaFuncBuilder, validator: &mut Validator, _: &mut SsaVarAlloc)
			where
				F: FnOnce(MemoryImmediate, TypedSsaVar, SsaVarOrConst) -> SsaInstr,
		{
			let src = validator.pop_value_ty(Type::I64.into());
			let addr = validator.pop_value_ty(Type::I32.into());

			if validator.reachable() {
				builder.current_block_mut().body.push(f(memarg, src.unwrap(), addr.unwrap().into()));
			}
		}

		fn make_i32_extend<F>(f: F, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(TypedSsaVar, TypedSsaVar) -> SsaInstr,
		{
			let src = validator.pop_value_ty(Type::I32.into());
			let dst = alloc.new_i32();
			validator.push_value(dst);

			if validator.reachable() {
				builder.current_block_mut().body.push(f(dst, src.unwrap()));
			}
		}

		fn make_i64_extend<F>(f: F, source: Type, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(TypedSsaVar, TypedSsaVar) -> SsaInstr,
		{
			let src = validator.pop_value_ty(source.into());
			let dst = alloc.new_i64();
			validator.push_value(dst);

			if validator.reachable() {
				builder.current_block_mut().body.push(f(dst, src.unwrap()));
			}
		}

		match op {
			&Operator::I32Const { value } => {
				let var = alloc.new_i32();
				builder.current_block_mut().body.push(SsaInstr::I32Set(var, value));
				validator.push_value(var);
			},
			&Operator::I64Const { value } => {
				let var = alloc.new_i64();
				builder.current_block_mut().body.push(SsaInstr::I64Set(var, value));
				validator.push_value(var);
			}

			Operator::I32Eqz => {
				let src = validator.pop_value_ty(Type::I32.into());
				let dst = alloc.new_i32();
				if let UncertainVar::Known(src) = src {
					builder.current_block_mut().body.push(SsaInstr::Eqz(dst, src));
				}
				validator.push_value(dst);
			}
			Operator::I64Eqz => {
				let src = validator.pop_value_ty(Type::I64.into());
				let dst = alloc.new_i32();
				if let UncertainVar::Known(src) = src {
					builder.current_block_mut().body.push(SsaInstr::Eqz(dst, src));
				}
				validator.push_value(dst);
			}

			Operator::I32Add => make_i32_binop(SsaInstr::Add, builder, validator, alloc),
			Operator::I32Sub => make_i32_binop(SsaInstr::Sub, builder, validator, alloc),
			Operator::I32Mul => make_i32_binop(SsaInstr::Mul, builder, validator, alloc),
			Operator::I32DivS => make_i32_binop(SsaInstr::DivS, builder, validator, alloc),
			Operator::I32DivU => make_i32_binop(SsaInstr::DivU, builder, validator, alloc),
			Operator::I32RemS => make_i32_binop(SsaInstr::RemS, builder, validator, alloc),
			Operator::I32RemU => make_i32_binop(SsaInstr::RemU, builder, validator, alloc),
			Operator::I32Shl => make_i32_binop(SsaInstr::Shl, builder, validator, alloc),
			Operator::I32ShrS => make_i32_binop(SsaInstr::ShrS, builder, validator, alloc),
			Operator::I32ShrU => make_i32_binop(SsaInstr::ShrU, builder, validator, alloc),
			Operator::I32Rotl => make_i32_binop(SsaInstr::Rotl, builder, validator, alloc),
			Operator::I32Rotr => make_i32_binop(SsaInstr::Rotr, builder, validator, alloc),
			Operator::I32Xor => make_i32_binop(SsaInstr::Xor, builder, validator, alloc),
			Operator::I32And => make_i32_binop(SsaInstr::And, builder, validator, alloc),
			Operator::I32Or => make_i32_binop(SsaInstr::Or, builder, validator, alloc),

			Operator::I64Add => make_i64_binop(SsaInstr::Add, builder, validator, alloc),
			Operator::I64Sub => make_i64_binop(SsaInstr::Sub, builder, validator, alloc),
			Operator::I64Mul => make_i64_binop(SsaInstr::Mul, builder, validator, alloc),
			Operator::I64DivS => make_i64_binop(SsaInstr::DivS, builder, validator, alloc),
			Operator::I64DivU => make_i64_binop(SsaInstr::DivU, builder, validator, alloc),
			Operator::I64RemS => make_i64_binop(SsaInstr::RemS, builder, validator, alloc),
			Operator::I64RemU => make_i64_binop(SsaInstr::RemU, builder, validator, alloc),
			Operator::I64Shl => make_i64_binop(SsaInstr::Shl, builder, validator, alloc),
			Operator::I64ShrS => make_i64_binop(SsaInstr::ShrS, builder, validator, alloc),
			Operator::I64ShrU => make_i64_binop(SsaInstr::ShrU, builder, validator, alloc),
			Operator::I64Rotl => make_i64_binop(SsaInstr::Rotl, builder, validator, alloc),
			Operator::I64Rotr => make_i64_binop(SsaInstr::Rotr, builder, validator, alloc),
			Operator::I64Xor => make_i64_binop(SsaInstr::Xor, builder, validator, alloc),
			Operator::I64And => make_i64_binop(SsaInstr::And, builder, validator, alloc),
			Operator::I64Or => make_i64_binop(SsaInstr::Or, builder, validator, alloc),

			Operator::I32Popcnt => make_i32_unaryop(SsaInstr::Popcnt, builder, validator, alloc),
			Operator::I32Clz => make_i32_unaryop(SsaInstr::Clz, builder, validator, alloc),
			Operator::I32Ctz => make_i32_unaryop(SsaInstr::Ctz, builder, validator, alloc),

			Operator::I64Popcnt => make_i64_unaryop(SsaInstr::Popcnt, builder, validator, alloc),
			Operator::I64Clz => make_i64_unaryop(SsaInstr::Clz, builder, validator, alloc),
			Operator::I64Ctz => make_i64_unaryop(SsaInstr::Ctz, builder, validator, alloc),

			Operator::I32GtS => make_i32_binop(SsaInstr::GtS, builder, validator, alloc),
			Operator::I32GtU => make_i32_binop(SsaInstr::GtU, builder, validator, alloc),
			Operator::I32GeS => make_i32_binop(SsaInstr::GeS, builder, validator, alloc),
			Operator::I32GeU => make_i32_binop(SsaInstr::GeU, builder, validator, alloc),
			Operator::I32LtS => make_i32_binop(SsaInstr::LtS, builder, validator, alloc),
			Operator::I32LtU => make_i32_binop(SsaInstr::LtU, builder, validator, alloc),
			Operator::I32LeS => make_i32_binop(SsaInstr::LeS, builder, validator, alloc),
			Operator::I32LeU => make_i32_binop(SsaInstr::LeU, builder, validator, alloc),
			Operator::I32Eq => make_i32_binop(SsaInstr::Eq, builder, validator, alloc),
			Operator::I32Ne => make_i32_binop(SsaInstr::Ne, builder, validator, alloc),

			Operator::I64GtS => make_i64_comp(SsaInstr::GtS, builder, validator, alloc),
			Operator::I64GtU => make_i64_comp(SsaInstr::GtU, builder, validator, alloc),
			Operator::I64GeS => make_i64_comp(SsaInstr::GeS, builder, validator, alloc),
			Operator::I64GeU => make_i64_comp(SsaInstr::GeU, builder, validator, alloc),
			Operator::I64LtS => make_i64_comp(SsaInstr::LtS, builder, validator, alloc),
			Operator::I64LtU => make_i64_comp(SsaInstr::LtU, builder, validator, alloc),
			Operator::I64LeS => make_i64_comp(SsaInstr::LeS, builder, validator, alloc),
			Operator::I64LeU => make_i64_comp(SsaInstr::LeU, builder, validator, alloc),
			Operator::I64Eq => make_i64_comp(SsaInstr::Eq, builder, validator, alloc),
			Operator::I64Ne => make_i64_comp(SsaInstr::Ne, builder, validator, alloc),

			Operator::I32Extend8S => make_i32_extend(SsaInstr::Extend8S, builder, validator, alloc),
			Operator::I32Extend16S => make_i32_extend(SsaInstr::Extend16S, builder, validator, alloc),

			Operator::I64Extend8S => make_i64_extend(SsaInstr::Extend8S, Type::I64, builder, validator, alloc),
			Operator::I64Extend16S => make_i64_extend(SsaInstr::Extend16S, Type::I64, builder, validator, alloc),
			Operator::I64Extend32S => make_i64_extend(SsaInstr::Extend32S, Type::I64, builder, validator, alloc),

			Operator::I64ExtendI32S => make_i64_extend(SsaInstr::Extend32S, Type::I32, builder, validator, alloc),
			Operator::I64ExtendI32U => make_i64_extend(SsaInstr::Extend32U, Type::I32, builder, validator, alloc),
			

			Operator::I32WrapI64 => {
				let src = validator.pop_value_ty(Type::I64.into());
				let dst = alloc.new_i32();
				if let UncertainVar::Known(src) = src {
					builder.current_block_mut().body.push(SsaInstr::Wrap(dst, src));
				}
				validator.push_value(dst);
			},

			&Operator::I32Load { memarg } => make_i32_load(SsaInstr::Load32S, memarg, builder, validator, alloc),
			&Operator::I32Load16S { memarg } => make_i32_load(SsaInstr::Load16S, memarg, builder, validator, alloc),
			&Operator::I32Load16U { memarg } => make_i32_load(SsaInstr::Load16U, memarg, builder, validator, alloc),
			&Operator::I32Load8U { memarg } => make_i32_load(SsaInstr::Load8U, memarg, builder, validator, alloc),
			&Operator::I32Load8S { memarg } => make_i32_load(SsaInstr::Load8S, memarg, builder, validator, alloc),
			
			&Operator::I64Load { memarg } => make_i64_load(SsaInstr::Load64, memarg, builder, validator, alloc),
			&Operator::I64Load32S { memarg } => make_i64_load(SsaInstr::Load32S, memarg, builder, validator, alloc),
			&Operator::I64Load32U { memarg } => make_i64_load(SsaInstr::Load32U, memarg, builder, validator, alloc),
			&Operator::I64Load16S { memarg } => make_i64_load(SsaInstr::Load16S, memarg, builder, validator, alloc),
			&Operator::I64Load16U { memarg } => make_i64_load(SsaInstr::Load16U, memarg, builder, validator, alloc),
			&Operator::I64Load8S { memarg } => make_i64_load(SsaInstr::Load8S, memarg, builder, validator, alloc),
			&Operator::I64Load8U { memarg } => make_i64_load(SsaInstr::Load8U, memarg, builder, validator, alloc),

			&Operator::I32Store { memarg } => make_i32_store(SsaInstr::Store32, memarg, builder, validator, alloc),
			&Operator::I32Store16 { memarg } => make_i32_store(SsaInstr::Store16, memarg, builder, validator, alloc),
			&Operator::I32Store8 { memarg } => make_i32_store(SsaInstr::Store8, memarg, builder, validator, alloc),

			&Operator::I64Store { memarg } => make_i64_store(SsaInstr::Store64, memarg, builder, validator, alloc),
			&Operator::I64Store32 { memarg } => make_i64_store(SsaInstr::Store32, memarg, builder, validator, alloc),
			&Operator::I64Store16 { memarg } => make_i64_store(SsaInstr::Store16, memarg, builder, validator, alloc),
			&Operator::I64Store8 { memarg } => make_i64_store(SsaInstr::Store8, memarg, builder, validator, alloc),

			&Operator::GlobalSet { global_index } => {
				let ty = wasm_file.global(global_index).ty;
				assert!(ty.mutable);
				let ty = ty.content_type;
				let src = validator.pop_value_ty(ty.into());
				if let UncertainVar::Known(src) = src {
					builder.current_block_mut().body.push(SsaInstr::GlobalSet(global_index, src));
				}
			}
			&Operator::GlobalGet { global_index } => {
				let ty = wasm_file.global(global_index).ty.content_type;
				let dst = alloc.new_typed(ty);
				builder.current_block_mut().body.push(SsaInstr::GlobalGet(dst, global_index));
				validator.push_value(dst);
			}

			&Operator::LocalSet { local_index } => {
				let ty = locals[local_index as usize];
				let src = validator.pop_value_ty(ty.into());
				if let UncertainVar::Known(src) = src {
					builder.current_locals[local_index as usize] = src;
				}
			}
			&Operator::LocalGet { local_index } => {
				let local_var = builder.current_locals[local_index as usize];
				validator.push_value(local_var);
			}
			&Operator::LocalTee { local_index } => {
				let ty = locals[local_index as usize];

				let src = validator.pop_value_ty(ty.into());
				validator.push_value(src);

				if let UncertainVar::Known(src) = src {
					builder.current_locals[local_index as usize] = src;
				}

				validator.pop_push_values(&[ty], &[src]);
			}
			&Operator::TypedSelect { ty } => {
				let cond = validator.pop_value_ty(Type::I32.into());
				let false_var = validator.pop_value_ty(ty.into());
				let true_var = validator.pop_value_ty(ty.into());

				let dst = alloc.new_typed(ty).into();

				if let Some((dst, true_var, false_var, cond)) = zip_vars!(dst, true_var, false_var, cond) {
					builder.current_block_mut().body.push(SsaInstr::Select { dst, true_var: true_var.into(), false_var: false_var.into(), cond });
				}

				validator.push_value(dst);
			}
			Operator::Select => {
				let cond = validator.pop_value_ty(Type::I32.into());
				let false_var = validator.pop_value();
				let true_var = validator.pop_value();

				assert!(true_var.ty().matches(false_var.ty()));

				let dst = if let UncertainType::Known(true_ty) = true_var.ty() {
					alloc.new_typed(true_ty).into()
				} else {
					UnknownVar
				};

				if let Some((dst, true_var, false_var, cond)) = zip_vars!(dst, true_var, false_var, cond) {
					builder.current_block_mut().body.push(SsaInstr::Select { dst, true_var: true_var.into(), false_var: false_var.into(), cond });
				}

				validator.push_value(dst);
			}
			Operator::Drop => {
				validator.pop_value();
			}
			&Operator::Call { function_index } => {
				let called_ty = wasm_file.func_type(function_index as usize);

				let params = validator.pop_values(&called_ty.params);
				let params: Option<Vec<TypedSsaVar>> = params.into_iter().map(Option::from).collect::<Option<Vec<_>>>();
				let returns = called_ty.returns.iter().map(|ty| alloc.new_typed(*ty)).collect::<Vec<_>>();

				validator.push_values(&returns);

				if let Some(params) = params {
					if !wasm_file.func_is_defined(function_index as usize) {
						let import = wasm_file.func_import(function_index as usize);
						match (import.module, import.field.unwrap()) {
							("env", "turtle_x") => {
								assert_eq!(params.len(), 1);
								assert_eq!(returns.len(), 0);
								builder.current_block_mut().body.push(SsaInstr::TurtleSetX(params[0].into()));
							}
							("env", "turtle_y") => {
								assert_eq!(params.len(), 1);
								assert_eq!(returns.len(), 0);
								builder.current_block_mut().body.push(SsaInstr::TurtleSetY(params[0].into()));
							}
							("env", "turtle_z") => {
								assert_eq!(params.len(), 1);
								assert_eq!(returns.len(), 0);
								builder.current_block_mut().body.push(SsaInstr::TurtleSetZ(params[0].into()));
							}
							("env", "turtle_set") => {
								assert_eq!(params.len(), 1);
								assert_eq!(returns.len(), 0);
								builder.current_block_mut().body.push(SsaInstr::TurtleSetBlock(params[0]));
							}
							("env", "turtle_get") => {
								assert_eq!(params.len(), 0);
								assert_eq!(returns.len(), 1);
								builder.current_block_mut().body.push(SsaInstr::TurtleGetBlock(returns[0]));
							}
							("env", "turtle_copy") => {
								assert_eq!(params.len(), 0);
								assert_eq!(returns.len(), 0);
								builder.current_block_mut().body.push(SsaInstr::TurtleCopy);
							}
							("env", "turtle_paste") => {
								assert_eq!(params.len(), 0);
								assert_eq!(returns.len(), 0);
								builder.current_block_mut().body.push(SsaInstr::TurtlePaste);
							}
							("env", "memset") => {
								assert_eq!(params.len(), 3);
								assert_eq!(returns.len(), 1);
								let instr = SsaInstr::Memset { dest: params[0], value: params[1], length: params[2], result: returns[0] };
								builder.current_block_mut().body.push(instr);
							}
							("env", "sleep" | "mc_sleep") => {
								assert_eq!(params.len(), 0);
								assert_eq!(returns.len(), 0);

								let next_block = builder.alloc_block();

								let target = JumpTarget { label: next_block, params: Vec::new() };
								builder.finish_block(SsaTerminator::ScheduleJump(target, 1));

								builder.set_block(next_block);
							}
							("env", "print") => {
								assert_eq!(params.len(), 1);
								assert_eq!(returns.len(), 0);

								builder.current_block_mut().body.push(SsaInstr::PrintInt(params[0]));
							}
							("env", "mc_putc") => {
								// TODO:
								println!("ignoring putc");
							}
							_ => todo!("{:?}", import),
						}
					} else {
						builder.current_block_mut().body.push(SsaInstr::Call {
							function_index,
							params,
							returns,
						});
					}
				}
			}
			&Operator::CallIndirect { index, table_index } => {
				let called_ty = wasm_file.types.func_type(index);

				let table_entry: Option<_> = validator.pop_value_ty(Type::I32.into()).into();

				let params = validator.pop_values(&called_ty.params);
				let params = params.into_iter().map(Option::from).collect::<Option<Vec<_>>>();
				let returns = called_ty.returns.iter().map(|ty| alloc.new_typed(*ty)).collect::<Vec<_>>();

				validator.push_values(&returns);

				if let Some((params, table_entry)) = params.zip(table_entry) {
					builder.current_block_mut().body.push(SsaInstr::CallIndirect {
						table_index,
						table_entry,
						params,
						returns,
					});
				}
			}
			Operator::Block { ty } => {
				let block = builder.alloc_block_with_locals(&locals, alloc);

				let start_types = wasm_file.types.start_types(*ty);
				let end_types = wasm_file.types.end_types(*ty);
				let control_op = ControlOp::Block(block);

				let start_vals = validator.pop_values(&start_types);

				validator.push_ctrl(control_op, &start_vals, start_types, end_types);
			}
			Operator::Loop { ty } => {
				let block = builder.alloc_block_with_locals(&locals, alloc);

				let start_types = wasm_file.types.start_types(*ty);
				let end_types = wasm_file.types.end_types(*ty);
				let control_op = ControlOp::Loop(block);

				let start_vals = validator.pop_values(&start_types);
				let start_vals_known = start_vals.iter().copied().map(Option::from).collect::<Option<Vec<TypedSsaVar>>>();

				validator.push_ctrl(control_op, &start_vals, start_types.clone(), end_types);

				if let Some(start_vals_known) = start_vals_known {
					let mut params = builder.current_locals.clone();
					params.extend(start_vals_known);
					let target = JumpTarget { label: block, params };
					builder.finish_block(SsaTerminator::Jump(target));
				} else {
					builder.finish_block(SsaTerminator::Unreachable);
				}

				builder.set_block(block);

				make_params(builder, validator, &start_types, alloc);
			}
			Operator::If { ty } => {
				let start_types = wasm_file.types.start_types(*ty);
				let end_types = wasm_file.types.end_types(*ty);

				let false_label = builder.alloc_block();
				let next_label = builder.alloc_block_with_locals(&locals, alloc);

				let cond: Option<_> = validator.pop_value_ty(Type::I32.into()).into();

				let start_vars = validator.pop_values(&start_types);

				let control_op = ControlOp::If { start_vars: start_vars.clone(), false_label, next_label };

				validator.push_ctrl(control_op, &start_vars, start_types, end_types);

				if let Some(cond) = cond {
					let true_label = builder.alloc_block();

					let true_target = JumpTarget {
						label: true_label,
						params: Vec::new(),
					};

					let false_target = JumpTarget {
						label: false_label,
						params: Vec::new(), 
					};

					builder.finish_block(SsaTerminator::BranchIf { cond, true_target, false_target });
					builder.set_block(true_label);
				} else {
					let true_label = builder.alloc_block();

					builder.finish_block(SsaTerminator::Unreachable);

					builder.set_block(true_label);
				}
			}
			Operator::Else => {
				let (end_vals, frame) = validator.pop_ctrl();

				let (start_vars, false_label, next_label) = if let ControlOp::If { start_vars, false_label, next_label } = frame.operator {
					(start_vars, false_label, next_label)
				} else {
					panic!();
				};

				let end_vals_known = end_vals.iter().copied().map(Option::from).collect::<Option<Vec<TypedSsaVar>>>();

				if let Some(end_vals_known) = end_vals_known {
					let mut params = builder.current_locals.clone();
					params.extend(end_vals_known);

					let target = JumpTarget {
						label: next_label,
						params,
					};

					builder.finish_block(SsaTerminator::Jump(target));
				} else {
					builder.finish_block(SsaTerminator::Unreachable);
				}

				let control_op = ControlOp::Else { next_label };

				validator.push_ctrl(control_op, &start_vars, frame.start_types, frame.end_types);

				builder.set_block(false_label);
			}
			Operator::End => {
				if matches!(validator.control_stack.top().unwrap().operator, ControlOp::If { .. }) {
					// TODO: Optimize for ifs without an else
					self.visit_operator(&Operator::Else);
				}

				let validator = &mut self.validator;
				let builder = &mut self.builder;
				let alloc = &mut self.alloc;

				let (end_vals, frame) = validator.pop_ctrl();

				match frame.operator {
					ControlOp::Loop(_) => {
						validator.push_values(&end_vals);
					},
					ControlOp::Block(label) => {
						let end_vals_known = end_vals.iter().map(|v| Option::from(*v)).collect::<Option<Vec<TypedSsaVar>>>();

						if let Some(end_vals_known) = end_vals_known {
							let mut params = builder.current_locals.clone();
							params.extend(end_vals_known);
							let target = JumpTarget { label, params };
							builder.finish_block(SsaTerminator::Jump(target));

						} else {
							builder.finish_block(SsaTerminator::Unreachable);
						}

						builder.set_block(label);

						validator.push_values(&end_vals);

						make_params(builder, validator, &frame.end_types, alloc);
					}
					ControlOp::If { .. } => unreachable!(),
					ControlOp::Else { next_label } => {
						let end_vals_known = end_vals.iter().copied().map(Option::from).collect::<Option<Vec<TypedSsaVar>>>();
						if let Some(end_vals) = end_vals_known {
							let mut params = builder.current_locals.clone();
							params.extend(end_vals);
							let target = JumpTarget { label: next_label, params };
							builder.finish_block(SsaTerminator::Jump(target));
						} else {
							builder.finish_block(SsaTerminator::Unreachable);
						}

						builder.set_block(next_label);

						validator.push_values(&end_vals);

						make_params(builder, validator, &frame.end_types, alloc);
					}
				}
			}
			&Operator::Return => {
				let depth = validator.control_stack.0.len() - 1;
				self.visit_operator(&Operator::Br { relative_depth: depth as u32 });
			}
			&Operator::Br { relative_depth } => {
				let target_frame = &validator.control_stack[TopIndex(relative_depth as usize)];
				let label_types = target_frame.label_types().to_owned();
				let target_label = target_frame.operator.target_label();

				let label_vals = validator.pop_values(&label_types).into_iter().map(Option::from).collect::<Option<Vec<TypedSsaVar>>>();

				validator.mark_unreachable();

				if let Some(label_vals) = label_vals {
					let mut params = builder.current_locals.clone();
					params.extend(label_vals);
					let target = JumpTarget { label: target_label, params };
					builder.finish_block(SsaTerminator::Jump(target));
				} else {
					builder.finish_block(SsaTerminator::Unreachable);
				}

				let new_block = builder.alloc_block();
				builder.set_block(new_block);
			}
			Operator::BrTable { table } => {
				let cond: Option<_> = validator.pop_value_ty(Type::I32.into()).into();

				let default_frame = &validator.control_stack[TopIndex(table.default() as usize)];
				let default_label = default_frame.operator.target_label();
				let default_label_types = default_frame.label_types().to_owned();

				let label_vals = validator.pop_values(&default_label_types).into_iter().map(Option::from).collect::<Option<Vec<TypedSsaVar>>>();

				if let (Some(cond), Some(label_vals)) = (cond, label_vals) {
					let mut params = builder.current_locals.clone();
					params.extend(label_vals);

					let default = JumpTarget {
						label: default_label, 
						params: params.clone()
					};

					let arms = table.targets().map(|arm| {
						let arm = arm.unwrap();
						let arm_frame = &validator.control_stack[TopIndex(arm as usize)];
						let arm_label_types = arm_frame.label_types();
						assert_eq!(arm_label_types, default_label_types);
						let label = arm_frame.operator.target_label();
						JumpTarget {
							label,
							params: params.clone(),
						}
					}).collect::<Vec<_>>();

					builder.finish_block(SsaTerminator::BranchTable { cond, default, arms });
				} else {
					// TODO: Still should do checks here, too

					builder.finish_block(SsaTerminator::Unreachable);
				}

				validator.mark_unreachable();

				let new_block = builder.alloc_block();
				builder.set_block(new_block);
			}
			&Operator::BrIf { relative_depth } => {
				let target_frame = &validator.control_stack[TopIndex(relative_depth as usize)];
				let label_types = target_frame.label_types().to_owned();
				let true_label = target_frame.operator.target_label();

				let cond: Option<_> = validator.pop_value_ty(Type::I32.into()).into();

				let label_vals = validator.pop_values(&label_types).into_iter().map(Option::from).collect::<Option<Vec<TypedSsaVar>>>();

				// TODO: Does this need its own locals???
				let next_block = builder.alloc_block_with_locals(&locals, alloc);

				if let (Some(cond), Some(label_vals)) = (cond, label_vals) {
					validator.push_values(&label_vals);

					let mut true_params = builder.current_locals.clone();
					true_params.extend(label_vals.iter().copied());

					let mut false_params = builder.current_locals.clone();
					false_params.extend(label_vals.iter().copied());

					assert_eq!(true_params, false_params);

					let true_target = JumpTarget {
						label: true_label,
						params: true_params,
					};


					let false_target = JumpTarget {
						label: next_block,
						params: false_params,
					};

					builder.finish_block(SsaTerminator::BranchIf { cond, true_target, false_target });
				} else {
					builder.finish_block(SsaTerminator::Unreachable);
				}

				builder.set_block(next_block);

				// TODO: There's only one way to reach this block, so this seems unnecessary,
				// but the block *has* to have parameters, right...?
				make_params(builder, validator, &label_types, alloc);
			}

			&Operator::MemoryGrow { mem, mem_byte } => {
				if mem != 0 || mem_byte != 0 {
					todo!()
				}

				// TODO:
				let _n = validator.pop_value_ty(Type::I32.into());
				self.visit_operator(&Operator::I32Const { value: -1 });
			}

			Operator::Nop => {},
			Operator::Unreachable => {
				validator.mark_unreachable();
			}
			_ => todo!("{:?}", op),
		}

	}
}

pub fn validate(wasm_file: &WasmFile, func: usize) -> SsaFunction {
	let func_ty = wasm_file.func_type(func);
	let func_body = wasm_file.func_body(func);

	let mut builder = SsaFuncBuilder::new(func);

	let mut alloc = SsaVarAlloc::new();

	let locals = wasm_file.func_locals(func);

	let start_block = builder.alloc_block_with_locals(&locals, &mut alloc);
	let end_block = builder.alloc_block_with_locals(&locals, &mut alloc);

	builder.set_block(start_block);

	builder.current_block_mut().params = Vec::new();

	let start_locals = builder.current_locals.clone();

	let params = start_locals.iter().copied().zip(func_ty.params.iter().copied());
	for (param_idx, (param_var, param_ty)) in params.into_iter().enumerate() {
		assert_eq!(param_var.ty(), param_ty);

		builder.current_block_mut().body.push(SsaInstr::ParamGet(param_var, param_idx as u32));
	}

	for local in start_locals.iter().skip(func_ty.params.len()) {
		match local.ty() {
			Type::I32 => {
				builder.current_block_mut().body.push(SsaInstr::I32Set(*local, 0));
			}
			Type::I64 => {
				builder.current_block_mut().body.push(SsaInstr::I64Set(*local, 0));
			}
			_ => todo!("{:?}", local),
		}
	}

	let mut validator = Validator::default();

	validator.push_ctrl(ControlOp::Block(end_block), &[], Box::new([]), func_ty.returns.clone());

	let mut state = ValidationState {
		wasm_file,
		func,
		builder,
		alloc,
		validator,
	};

	for op in func_body.operators.iter() {
		state.visit_operator(op);
	}

	assert_eq!(state.builder.current_block, 1);

	let return_vals = state.validator.value_stack.0.iter().map(|v| v.unwrap()).collect();
	state.builder.finish_block(SsaTerminator::Return(return_vals));

	println!("{:?}", func_ty);
	println!("{:?}", state.validator.control_stack);
	println!("{:?}\n", state.validator.value_stack);

	let (_, blocks) = state.builder.finish();

	for (_block_idx, (block_id, block)) in blocks.iter().enumerate() {
		println!("==== block {:?} ==== ", block_id);
		println!("parameters: {:?}", block.params);
		for instr in block.body.iter() {
			println!("{:?}", instr);
		}
		println!("{:?}\n", block.term);
	}

	/*let mut changed = true;
	while changed {
		changed = false;

		for idx in 0..blocks.len() {
			match blocks[idx].term {
				SsaTerminator::Unreachable => {},
				SsaTerminator::ScheduleJump(t, _) |
				SsaTerminator::Jump(t) => {
					let dest = blocks.iter().find(|(i, _)| *i == t.label).unwrap();
				}
				SsaTerminator::BranchIf { cond, true_target, false_target } => todo!(),
				SsaTerminator::BranchTable { cond, default, arms } => todo!(),
				SsaTerminator::Return(_) => todo!(),
			}
		}
	}*/

	/*let mut i = 0;
	while i < blocks.len() {
		if matches!(blocks[i].1.term, SsaTerminator::Unreachable) {
			blocks.remove(i);
			changed = true;
		} else {
			i += 1;
		}
	}*/

	SsaFunction::new(blocks, func_ty.params.clone(), func_ty.returns.clone())
}

pub fn wasm_to_ssa(wasm_file: &WasmFile) -> SsaProgram {
	let mut code = Vec::new();

	let mut local_types = HashMap::new();

	for func in 0..wasm_file.functions.functions.len() {
		if wasm_file.func_is_defined(func) {
			let ssa_func = validate(wasm_file, func);
			code.push(ssa_func);
			local_types.insert(func, wasm_file.func_locals(func));
		}

	}

	let globals = wasm_file.globals().iter().map(|global| {
		let val = eval_init_expr_single(&global.init_expr);
		assert_eq!(val.ty(), global.ty.content_type);
		val
	}).collect();

	let mut tables = wasm_file.tables.tables.iter().map(|table_ty| {
		if table_ty.element_type != Type::FuncRef {
			todo!()
		}

		Table {
			max: table_ty.maximum.map(|m| m as usize),
			elements: vec![None; table_ty.initial as usize],
		}
	}).collect::<Vec<_>>();

	for elem in wasm_file.elements.elements.iter() {
		if elem.ty != Type::FuncRef {
			todo!()
		}

		match elem.kind {
			ElementKind::Active { table_index, init_expr } => {
				let table = &mut tables[table_index as usize];

				let offset = eval_init_expr_single(&init_expr);
				let offset = offset.into_i32().unwrap();

				for (idx, item) in elem.items.get_items_reader().unwrap().into_iter().enumerate() {
					let item = item.unwrap();

					if let ElementItem::Func(item) = item {
						let index = idx + offset as usize;
						assert!(table.elements[index].is_none());
						table.elements[index] = Some(item as usize);
					} else {
						todo!()
					}
				}
			}
			ElementKind::Passive => todo!(),
			ElementKind::Declared => todo!(),
		}
	}

	let mut memory = wasm_file.memory.memory.iter().map(|mem_ty| {
		assert!(!mem_ty.memory64);
		assert!(!mem_ty.shared);

		Memory::new(mem_ty.initial as usize, mem_ty.maximum.map(|m| m as usize))
	}).collect::<Vec<_>>();

	for data in wasm_file.data.data.iter() {
		match data.kind {
			DataKind::Active { memory_index, init_expr } => {
				let offset = eval_init_expr_single(&init_expr);
				let offset = offset.into_i32().unwrap();

				let memory = &mut memory[memory_index as usize];

				let slice = &mut memory.data[offset as usize..][..data.data.len()];
				slice.copy_from_slice(data.data);
			}
			DataKind::Passive => todo!(),
		}
	}

	let exports = wasm_file.exports.exports.iter().filter_map(|export| {
		match export.kind {
			ExternalKind::Function => {
				let id = BlockId { func: export.index as usize, block: 0 };
				Some((export.field.to_owned(), id))
			}
			ExternalKind::Memory => None,
			ExternalKind::Global => None, // TODO:
			_ => todo!("{:?}", export.kind)
		}
	}).collect();

	for func in code.iter() {
		validate_ssa_jump_params(func);
	}

	let mut program = SsaProgram {
		local_types,
		globals,
		memory,
		tables,
		code,
		exports,
	};

	for func in program.code.iter_mut() {
		crate::ssa::const_prop::do_func_const_prop(func);
	}

	crate::ssa::dce::do_dead_code_elim(&mut program);

	for func in program.code.iter() {
		validate_ssa_jump_params(func);
	}
	
	program
}

pub fn validate_ssa_jump_params(func: &SsaFunction) {
	fn types_match(a: &[TypedSsaVar], b: &[TypedSsaVar]) -> bool {
		a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.ty() == b.ty())

	}

	for (source_id, source) in func.iter() {
		match &source.term {
			SsaTerminator::Unreachable => {},
			SsaTerminator::ScheduleJump(t, _) |
			SsaTerminator::Jump(t) => {
				let target = func.get(t.label);
				assert!(types_match(&t.params, &target.params), "{:?} {:?}", source_id, t.label);
			}
			SsaTerminator::BranchIf { cond: _, true_target, false_target } => {
				let t_target = func.get(true_target.label);
				let f_target = func.get(false_target.label);
				assert!(types_match(&t_target.params, &true_target.params), "{:?} {:?}", source_id, true_target.label);
				assert!(types_match(&f_target.params, &false_target.params), "{:?} {:?}", source_id, false_target.label);
			}
			SsaTerminator::BranchTable { cond: _, default, arms } => {
				let d_target = func.get(default.label);
				assert!(types_match(&d_target.params, &default.params), "{:?} {:?}", source_id, default.label);

				for arm in arms.iter() {
					let target = func.get(arm.label);
					assert!(types_match(&target.params, &arm.params), "{:?} {:?}", source_id, arm.label);
				}
			},
			SsaTerminator::Return(_) => {},
		}
	}
}