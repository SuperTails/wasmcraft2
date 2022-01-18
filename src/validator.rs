use std::ops::{Index, IndexMut};

use wasmparser::{Type, Operator, MemoryImmediate};

use crate::{wasm_file::WasmFile, ssa::{SsaBasicBlock, BlockId, SsaTerminator, TypedSsaVar, SsaInstr, SsaVarAlloc, JumpTarget}};

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

use UncertainVar::Unknown as UnknownVar;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UncertainType {
	Unknown,
	Known(Type),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ControlOp {
	Loop(BlockId),
	Block(BlockId),
	If { false_label: BlockId, next_label: BlockId },
	Else { next_label: BlockId },
}

impl ControlOp {
	pub fn target_label(&self) -> BlockId {
		match self {
			ControlOp::Loop(l) => *l,
			ControlOp::Block(l) => *l,
			ControlOp::If { false_label: _, next_label } => *next_label,
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
	pub fn push(&mut self, operator: ControlOp, start_vars: &[TypedSsaVar], end_types: Box<[Type]>, value_stack: &mut ValueStack) {
		let height = value_stack.0.len();

		value_stack.push_many(start_vars);

		let start_types = start_vars.iter().map(|v| v.ty()).collect();

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

struct SsaFuncBuilder {
	blocks: Vec<(SsaBasicBlock, bool, i32)>,
	counter: usize,
	current_block: usize,
	func: usize,
}

impl SsaFuncBuilder {
	pub fn new(func: usize) -> Self {
		SsaFuncBuilder {
			blocks: Vec::new(),
			current_block: 0,
			counter: 0,
			func,
		}
	}

	pub fn alloc_block(&mut self) -> BlockId {
		let block = self.blocks.len();
		self.blocks.push((SsaBasicBlock::default(), false, -1));
		BlockId { func: self.func, block }
	}

	pub fn set_block(&mut self, block_id: BlockId) {
		assert_eq!(self.func, block_id.func);
		self.current_block = block_id.block;
		assert_eq!(self.blocks[self.current_block].2, -1);
		self.blocks[self.current_block].2 = self.counter as i32;
		self.counter += 1;
	}

	pub fn current_block_mut(&mut self) -> &mut SsaBasicBlock {
		let id = BlockId { func: self.func, block: self.current_block };
		self.get_mut(id)
	}

	pub fn get(&self, block_id: BlockId) -> &SsaBasicBlock {
		assert_eq!(self.func, block_id.func);
		&self.blocks[block_id.block].0
	}

	pub fn get_mut(&mut self, block_id: BlockId) -> &mut SsaBasicBlock {
		assert_eq!(self.func, block_id.func);
		let (block, finished, _) = &mut self.blocks[block_id.block];
		assert!(!*finished);
		block
	}

	pub fn finish_block(&mut self, term: SsaTerminator) {
		let (block, finished, _) = &mut self.blocks[self.current_block];
		assert!(!*finished);

		block.term = term;
		*finished = true;
	}

	pub fn finish(self) -> (usize, Vec<(BlockId, SsaBasicBlock)>) {
		let mut blocks = self.blocks.into_iter()
			.enumerate()
			.map(|(idx, (block, finished, count))| {
				assert_ne!(count, -1);
				assert!(finished);
				let id = BlockId { func: self.func, block: idx };
				(count, id, block)
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
	
	pub fn push_ctrl(&mut self, operator: ControlOp, start_vars: &[TypedSsaVar], end_types: Box<[Type]>) {
		self.control_stack.push(operator, start_vars, end_types, &mut self.value_stack);
	}

	pub fn pop_ctrl(&mut self) -> (Vec<UncertainVar>, ControlFrame) {
		self.control_stack.pop(&mut self.value_stack)
	}

	pub fn mark_unreachable(&mut self) {
		self.control_stack.mark_unreachable(&mut self.value_stack);
	}
}

fn make_params(builder: &mut SsaFuncBuilder, validator: &mut Validator, t: &[Type], alloc: &mut SsaVarAlloc) {
	assert!(builder.current_block_mut().params.is_empty());

	if validator.value_stack.0.len() < t.len() {
		panic!("value stack: {:?} params: {:?}", validator.value_stack.0, t);
	}

	let suffix_idx = validator.value_stack.0.len() - t.len();
	let suffix = &mut validator.value_stack.0[suffix_idx..];

	assert!(suffix.iter().zip(t).all(|(s, t)| s.unwrap().ty() == *t));

	validator.pop_values(t);
	for &ty in t {
		let val = alloc.new_typed(ty);
		validator.push_value(val);
		builder.current_block_mut().params.push(val);
	}
}

pub fn validate(wasm_file: &WasmFile, func: usize) -> Vec<(BlockId, SsaBasicBlock)> {
	let func_ty = wasm_file.func_type(func);
	let func_body = wasm_file.func_body(func);
	let locals = wasm_file.func_locals(func);

	let mut builder = SsaFuncBuilder::new(func);

	let mut alloc = SsaVarAlloc::new();

	let start_block = builder.alloc_block();
	let end_block = builder.alloc_block();

	builder.set_block(start_block);

	let mut validator = Validator::default();

	validator.push_ctrl(ControlOp::Block(end_block), &[], func_ty.returns.clone());

	for op in func_body.operators.iter() {
		println!("{:?}", validator.value_stack);
		println!("{:?}", validator.control_stack);
		println!("{:?}", op);
		println!();

		fn make_i32_binop<F>(f: F, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(TypedSsaVar, TypedSsaVar, TypedSsaVar) -> SsaInstr
		{
			let rhs = validator.pop_value_ty(Type::I32.into()).unwrap();
			let lhs = validator.pop_value_ty(Type::I32.into()).unwrap();
			let dst = alloc.new_i32();
			validator.push_value(dst);
			builder.current_block_mut().body.push(f(dst, lhs, rhs));
		}

		fn make_i32_load<F>(f: F, memarg: MemoryImmediate, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(MemoryImmediate, TypedSsaVar, TypedSsaVar) -> SsaInstr,
		{
			let addr = validator.pop_value_ty(Type::I32.into()).unwrap();
			let dst = alloc.new_i32();
			validator.push_value(dst);
			builder.current_block_mut().body.push(f(memarg, dst, addr));
		}

		fn make_i64_load<F>(f: F, memarg: MemoryImmediate, builder: &mut SsaFuncBuilder, validator: &mut Validator, alloc: &mut SsaVarAlloc)
			where
				F: FnOnce(MemoryImmediate, TypedSsaVar, TypedSsaVar) -> SsaInstr,
		{
			let addr = validator.pop_value_ty(Type::I32.into()).unwrap();
			let dst = alloc.new_i64();
			validator.push_value(dst);
			builder.current_block_mut().body.push(f(memarg, dst, addr));
		}

		fn make_i32_store<F>(f: F, memarg: MemoryImmediate, builder: &mut SsaFuncBuilder, validator: &mut Validator, _: &mut SsaVarAlloc)
			where
				F: FnOnce(MemoryImmediate, TypedSsaVar, TypedSsaVar) -> SsaInstr,
		{
			let src = validator.pop_value_ty(Type::I32.into()).unwrap();
			let addr = validator.pop_value_ty(Type::I32.into()).unwrap();
			builder.current_block_mut().body.push(f(memarg, src, addr));
		}

		fn make_i64_store<F>(f: F, memarg: MemoryImmediate, builder: &mut SsaFuncBuilder, validator: &mut Validator, _: &mut SsaVarAlloc)
			where
				F: FnOnce(MemoryImmediate, TypedSsaVar, TypedSsaVar) -> SsaInstr,
		{
			let src = validator.pop_value_ty(Type::I64.into()).unwrap();
			let addr = validator.pop_value_ty(Type::I32.into()).unwrap();
			builder.current_block_mut().body.push(f(memarg, src, addr));
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
				let src = validator.pop_value_ty(Type::I32.into()).unwrap();
				let dst = alloc.new_i32();
				builder.current_block_mut().body.push(SsaInstr::Eqz(dst, src));
				validator.push_value(dst);
			}
			Operator::I32Add => make_i32_binop(SsaInstr::Add, &mut builder, &mut validator, &mut alloc),
			Operator::I32Sub => make_i32_binop(SsaInstr::Sub, &mut builder, &mut validator, &mut alloc),
			Operator::I32Mul => make_i32_binop(SsaInstr::Mul, &mut builder, &mut validator, &mut alloc),
			Operator::I32DivS => make_i32_binop(SsaInstr::DivS, &mut builder, &mut validator, &mut alloc),
			Operator::I32DivU => make_i32_binop(SsaInstr::DivU, &mut builder, &mut validator, &mut alloc),
			Operator::I32RemS => make_i32_binop(SsaInstr::RemS, &mut builder, &mut validator, &mut alloc),
			Operator::I32RemU => make_i32_binop(SsaInstr::RemU, &mut builder, &mut validator, &mut alloc),
			Operator::I32Shl => make_i32_binop(SsaInstr::Shl, &mut builder, &mut validator, &mut alloc),
			Operator::I32ShrS => make_i32_binop(SsaInstr::ShrS, &mut builder, &mut validator, &mut alloc),
			Operator::I32ShrU => make_i32_binop(SsaInstr::ShrU, &mut builder, &mut validator, &mut alloc),
			Operator::I32Xor => make_i32_binop(SsaInstr::Xor, &mut builder, &mut validator, &mut alloc),
			Operator::I32And => make_i32_binop(SsaInstr::And, &mut builder, &mut validator, &mut alloc),
			Operator::I32Or => make_i32_binop(SsaInstr::Or, &mut builder, &mut validator, &mut alloc),

			Operator::I32GtS => make_i32_binop(SsaInstr::GtS, &mut builder, &mut validator, &mut alloc),
			Operator::I32GtU => make_i32_binop(SsaInstr::GtU, &mut builder, &mut validator, &mut alloc),
			Operator::I32GeS => make_i32_binop(SsaInstr::GeS, &mut builder, &mut validator, &mut alloc),
			Operator::I32GeU => make_i32_binop(SsaInstr::GeU, &mut builder, &mut validator, &mut alloc),
			Operator::I32LtS => make_i32_binop(SsaInstr::LtS, &mut builder, &mut validator, &mut alloc),
			Operator::I32LtU => make_i32_binop(SsaInstr::LtU, &mut builder, &mut validator, &mut alloc),
			Operator::I32LeS => make_i32_binop(SsaInstr::LeS, &mut builder, &mut validator, &mut alloc),
			Operator::I32LeU => make_i32_binop(SsaInstr::LeU, &mut builder, &mut validator, &mut alloc),
			Operator::I32Eq => make_i32_binop(SsaInstr::Eq, &mut builder, &mut validator, &mut alloc),
			Operator::I32Ne => make_i32_binop(SsaInstr::Ne, &mut builder, &mut validator, &mut alloc),

			&Operator::I32Load { memarg } => make_i32_load(SsaInstr::Load, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I32Load16S { memarg } => make_i32_load(SsaInstr::Load16S, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I32Load16U { memarg } => make_i32_load(SsaInstr::Load16U, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I32Load8U { memarg } => make_i32_load(SsaInstr::Load8U, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I32Load8S { memarg } => make_i32_load(SsaInstr::Load8S, memarg, &mut builder, &mut validator, &mut alloc),
			
			&Operator::I64Load { memarg } => make_i64_load(SsaInstr::Load, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I64Load32S { memarg } => make_i64_load(SsaInstr::Load32S, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I64Load32U { memarg } => make_i64_load(SsaInstr::Load32U, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I64Load16S { memarg } => make_i64_load(SsaInstr::Load16S, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I64Load16U { memarg } => make_i64_load(SsaInstr::Load16U, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I64Load8S { memarg } => make_i64_load(SsaInstr::Load8S, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I64Load8U { memarg } => make_i64_load(SsaInstr::Load8U, memarg, &mut builder, &mut validator, &mut alloc),

			&Operator::I32Store { memarg } => make_i32_store(SsaInstr::Store, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I32Store16 { memarg } => make_i32_store(SsaInstr::Store16, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I32Store8 { memarg } => make_i32_store(SsaInstr::Store8, memarg, &mut builder, &mut validator, &mut alloc),

			&Operator::I64Store { memarg } => make_i64_store(SsaInstr::Store, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I64Store32 { memarg } => make_i64_store(SsaInstr::Store32, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I64Store16 { memarg } => make_i64_store(SsaInstr::Store16, memarg, &mut builder, &mut validator, &mut alloc),
			&Operator::I64Store8 { memarg } => make_i64_store(SsaInstr::Store8, memarg, &mut builder, &mut validator, &mut alloc),

			&Operator::LocalSet { local_index } => {
				let ty = locals[local_index as usize];
				let src = validator.pop_value_ty(ty.into()).unwrap();
				builder.current_block_mut().body.push(SsaInstr::LocalSet(local_index, src));
			}
			&Operator::LocalGet { local_index } => {
				let ty = locals[local_index as usize];
				let dst = alloc.new_typed(ty);
				validator.push_value(dst);
				builder.current_block_mut().body.push(SsaInstr::LocalGet(dst, local_index));
			}
			&Operator::LocalTee { local_index } => {
				let ty = locals[local_index as usize];
				let src = validator.pop_value_ty(ty.into()).unwrap();
				validator.push_value(src);

				builder.current_block_mut().body.push(SsaInstr::LocalSet(local_index, src));
				validator.pop_push_values(&[ty], &[src]);
			}
			Operator::Select => {
				let cond = validator.pop_value_ty(Type::I32.into()).unwrap();
				let false_var = validator.pop_value().unwrap();
				let true_var = validator.pop_value().unwrap();

				assert_eq!(true_var.ty(), false_var.ty());
				
				let dst = alloc.new_typed(true_var.ty());

				builder.current_block_mut().body.push(SsaInstr::Select { dst, true_var, false_var, cond });
				validator.push_value(dst);
			}
			Operator::Drop => {
				validator.pop_value();
			}
			&Operator::Call { function_index } => {
				let called_ty = wasm_file.func_type(function_index as usize);

				let params = validator.pop_values(&called_ty.params);
				let params = params.into_iter().map(|p| p.unwrap()).collect::<Vec<_>>();
				let returns = called_ty.returns.iter().map(|ty| alloc.new_typed(*ty)).collect::<Vec<_>>();

				validator.push_values(&returns);

				builder.current_block_mut().body.push(SsaInstr::Call {
					function_index,
					params,
					returns,
				});
			}
			Operator::Block { ty } => {
				let block = builder.alloc_block();

				let start_types = wasm_file.types.start_types(*ty);
				let end_types = wasm_file.types.end_types(*ty);
				let control_op = ControlOp::Block(block);

				let start_vals = validator.pop_values(&start_types).into_iter().map(|t| t.unwrap()).collect::<Vec<_>>();

				validator.push_ctrl(control_op, &start_vals, end_types);
			}
			Operator::Loop { ty } => {
				let block = builder.alloc_block();

				let start_types = wasm_file.types.start_types(*ty);
				let end_types = wasm_file.types.end_types(*ty);
				let control_op = ControlOp::Loop(block);

				let start_vals = validator.pop_values(&start_types).into_iter().map(|t| t.unwrap()).collect::<Vec<_>>();

				let target = JumpTarget { label: block, params: start_vals.clone() };

				validator.push_ctrl(control_op, &start_vals, end_types);
				builder.finish_block(SsaTerminator::Jump(target));
				builder.set_block(block);

				make_params(&mut builder, &mut validator, &start_types, &mut alloc);
			}
			Operator::End => {
				let (end_vals, frame) = validator.pop_ctrl();

				let end_vals = end_vals.into_iter().map(|v| v.unwrap()).collect::<Vec<_>>();

				match frame.operator {
					ControlOp::Loop(_) => {
						validator.push_values(&end_vals);
					},
					ControlOp::Block(label) => {
						let target = JumpTarget { label, params: end_vals.clone() };
						builder.finish_block(SsaTerminator::Jump(target));
						builder.set_block(label);

						validator.push_values(&end_vals);

						make_params(&mut builder, &mut validator, &frame.end_types, &mut alloc);
					}
					ControlOp::If { false_label: _, next_label: _ } => {
						todo!("insert an else");

						//validator.push_values(&end_vals);
					},
					ControlOp::Else { next_label } => {
						let target = JumpTarget { label: next_label, params: end_vals.clone() };
						builder.finish_block(SsaTerminator::Jump(target));
						builder.set_block(next_label);

						validator.push_values(&end_vals);

						make_params(&mut builder, &mut validator, &frame.end_types, &mut alloc);
					}
				}
			}
			&Operator::Br { relative_depth } => {
				let target_frame = &validator.control_stack[TopIndex(relative_depth as usize)];
				let label_types = target_frame.label_types().to_owned();
				let target_label = target_frame.operator.target_label();

				let label_vals = validator.pop_values(&label_types).into_iter().map(|v| v.unwrap()).collect::<Vec<_>>();
				validator.mark_unreachable();

				let target = JumpTarget { label: target_label, params: label_vals };

				builder.finish_block(SsaTerminator::Jump(target));

				let new_block = builder.alloc_block();
				builder.set_block(new_block);
			}
			Operator::BrTable { table } => {
				let cond = validator.pop_value_ty(Type::I32.into()).unwrap();

				let default_frame = &validator.control_stack[TopIndex(table.default() as usize)];
				let default_label = default_frame.operator.target_label();
				let default_label_types = default_frame.label_types().to_owned();

				let label_vals = validator.pop_values(&default_label_types).into_iter().map(|v| v.unwrap()).collect::<Vec<_>>();

				let default = JumpTarget {
					label: default_label, 
					params: label_vals.clone()
				};

				let arms = table.targets().map(|arm| {
					let arm = arm.unwrap();
					let arm_frame = &validator.control_stack[TopIndex(arm as usize)];
					let arm_label_types = arm_frame.label_types();
					assert_eq!(arm_label_types, default_label_types);
					let label = arm_frame.operator.target_label();
					JumpTarget {
						label,
						params: label_vals.clone(),
					}
				}).collect::<Vec<_>>();

				builder.finish_block(SsaTerminator::BranchTable { cond, default, arms });

				validator.mark_unreachable();

				let new_block = builder.alloc_block();
				builder.set_block(new_block);
			}
			&Operator::BrIf { relative_depth } => {
				let target_frame = &validator.control_stack[TopIndex(relative_depth as usize)];
				let label_types = target_frame.label_types().to_owned();
				let true_label = target_frame.operator.target_label();

				let cond = validator.pop_value_ty(Type::I32.into()).unwrap();

				let label_vals = validator.pop_values(&label_types).into_iter().map(|v| v.unwrap()).collect::<Vec<_>>();
				validator.push_values(&label_vals);

				let next_block = builder.alloc_block();

				let true_target = JumpTarget {
					label: true_label,
					params: label_vals.clone(),
				};

				let false_target = JumpTarget {
					label: next_block,
					params: label_vals,
				};

				builder.finish_block(SsaTerminator::BranchIf { cond, true_target, false_target });
				builder.set_block(next_block);
			}

			Operator::Nop => {},
			_ => todo!("{:?}", op),
		}
	}

	assert_eq!(builder.current_block, 1);

	builder.finish_block(SsaTerminator::Return);

	println!("{:?}", func_ty);
	println!("{:?}", validator.control_stack);
	println!("{:?}\n", validator.value_stack);

	let (_, blocks) = builder.finish();
	for (_block_idx, (block_id, block)) in blocks.iter().enumerate() {
		println!("==== block {:?} ==== ", block_id);
		println!("parameters: {:?}", block.params);
		for instr in block.body.iter() {
			println!("{:?}", instr);
		}
		println!("{:?}\n", block.term);
	}

	blocks
}