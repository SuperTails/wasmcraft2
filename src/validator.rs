use std::ops::{Index, IndexMut};

use wasmparser::{Type, Operator};

use None as Unknown;

use crate::wasm_file::WasmFile;

type UncertainType = Option<Type>;

#[derive(Default, Debug)]
struct ValueStack(Vec<UncertainType>);

impl ValueStack {
	pub fn push<T>(&mut self, ty: T)
		where T: Into<UncertainType>
	{
		self.0.push(ty.into())
	}

	pub fn push_many<T: Copy + Into<UncertainType>>(&mut self, tys: &[T]) {
		self.0.extend(tys.iter().map(|t| (*t).into()));
	}

	pub fn pop(&mut self, control_stack: &ControlStack) -> UncertainType {
		let ctrl_frame = control_stack.top().unwrap();

		if self.0.len() == ctrl_frame.height && ctrl_frame.unreachable {
			return Unknown;
		}

		assert_ne!(self.0.len(), ctrl_frame.height);

		self.0.pop().unwrap()
	}

	pub fn pop_ty<T: Into<UncertainType>>(&mut self, ty_to_pop: T, control_stack: &ControlStack) -> UncertainType {
		let ty_to_pop = ty_to_pop.into();
		let ty = self.pop(control_stack);
		assert!(ty == ty_to_pop || ty == Unknown || ty_to_pop == Unknown);
		ty
	}

	pub fn pop_many<T: Copy + Into<UncertainType>>(&mut self, tys: &[T], control_stack: &ControlStack) -> Vec<UncertainType> {
		let mut result = Vec::new();
		for &ty_to_pop in tys.iter().rev() {
			let ty_to_pop = ty_to_pop.into();
			result.push(self.pop_ty(ty_to_pop, control_stack));
		}
		result.reverse();
		result
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct BlockId {
	func: usize,
	block: usize
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
	pub fn push(&mut self, operator: ControlOp, start_types: Box<[Type]>, end_types: Box<[Type]>, value_stack: &mut ValueStack) {
		let height = value_stack.0.len();

		value_stack.push_many(&start_types);

		let frame = ControlFrame {
			operator,
			start_types,
			end_types,
			height,
			unreachable: false
		};

		self.0.push(frame);
	}

	pub fn pop(&mut self, value_stack: &mut ValueStack) -> ControlFrame {
		let frame = self.top().unwrap();
		let end_types = frame.end_types.iter().map(|t| Some(*t)).collect::<Vec<_>>();
		let height = frame.height;

		value_stack.pop_many(&end_types, self);
		assert_eq!(value_stack.0.len(), height);

		self.0.pop().unwrap()
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

#[derive(Debug)]
enum SsaTerminator {
	Unreachable,
	Jump(BlockId),
	BranchIf { true_label: BlockId, false_label: BlockId },
	Return,
}

struct SsaBasicBlock {
	body: Vec<String>,
	term: SsaTerminator,
}

impl Default for SsaBasicBlock {
	fn default() -> Self {
		SsaBasicBlock {
			body: Default::default(), term: SsaTerminator::Unreachable,
		}
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
		where T: Into<UncertainType>
	{
		self.value_stack.push(ty.into())
	}

	pub fn push_values<T>(&mut self, ty: &[T])
		where T: Copy + Into<UncertainType>
	{
		self.value_stack.push_many(ty)
	}

	pub fn pop_value(&mut self) -> UncertainType {
		self.value_stack.pop(&self.control_stack)
	}

	pub fn push_pop_values<T>(&mut self, input: &[T], output: &[T])
		where T: Copy + Into<UncertainType>
	{
		self.value_stack.pop_many(input, &self.control_stack);
		self.value_stack.push_many(output);
	}

	pub fn pop_value_ty(&mut self, ty_to_pop: UncertainType) -> UncertainType {
		self.value_stack.pop_ty(ty_to_pop, &self.control_stack)
	}

	pub fn pop_values<T: Copy + Into<UncertainType>>(&mut self, tys: &[T]) -> Vec<UncertainType> {
		self.value_stack.pop_many(tys, &self.control_stack)
	}
	
	pub fn push_ctrl(&mut self, operator: ControlOp, start_types: Box<[Type]>, end_types: Box<[Type]>) {
		self.control_stack.push(operator, start_types, end_types, &mut self.value_stack);
	}

	pub fn pop_ctrl(&mut self) -> ControlFrame {
		self.control_stack.pop(&mut self.value_stack)
	}

	pub fn mark_unreachable(&mut self) {
		self.control_stack.mark_unreachable(&mut self.value_stack);
	}
}

pub fn validate(wasm_file: &WasmFile, func: usize) {
	let func_ty = wasm_file.func_type(func);
	let func_body = wasm_file.func_body(func);
	let locals = wasm_file.func_locals(func);

	let mut builder = SsaFuncBuilder::new(func);

	let start_block = builder.alloc_block();
	let end_block = builder.alloc_block();

	builder.set_block(start_block);

	let mut validator = Validator::default();

	validator.push_ctrl(ControlOp::Block(end_block), Box::new([]), func_ty.returns.clone());

	for op in func_body.operators.iter() {
		println!("{:?}", validator.value_stack);
		println!("{:?}", validator.control_stack);
		println!("{:?}", op);
		println!();

		match op {
			Operator::Block { .. } |
			Operator::Loop { .. } |
			Operator::If { .. } |
			Operator::Else |
			Operator::End |
			Operator::Br { .. } |
			Operator::BrIf { .. } |
			Operator::BrTable { .. } |
			Operator::Return => {}

			_ => builder.current_block_mut().body.push(format!("{:?}", op)),
		}

		match op {
			Operator::I32Const { .. } => {
				validator.push_value(Type::I32);
			},
			Operator::I32Eqz => {
				validator.push_pop_values(&[Type::I32], &[Type::I32]);
			}
			Operator::I32Add |
			Operator::I32Sub |
			Operator::I32Mul |
			Operator::I32DivS |
			Operator::I32DivU |
			Operator::I32RemS |
			Operator::I32RemU |
			Operator::I32Shl |
			Operator::I32ShrS |
			Operator::I32ShrU |
			Operator::I32Xor |
			Operator::I32And |
			Operator::I32Or |
			Operator::I32GtS |
			Operator::I32GtU |
			Operator::I32LtS |
			Operator::I32LtU |
			Operator::I32LeS |
			Operator::I32LeU |
			Operator::I32GeS |
			Operator::I32GeU |
			Operator::I32Eq |
			Operator::I32Ne => {
				validator.push_pop_values(&[Type::I32, Type::I32], &[Type::I32]);
			}
			Operator::I32Load { .. } |
			Operator::I32Load16U { .. } |
			Operator::I32Load16S { .. } |
			Operator::I32Load8U { .. } |
			Operator::I32Load8S { .. } => {
				validator.push_pop_values(&[Type::I32], &[Type::I32]);
			}
			Operator::I32Store { .. } |
			Operator::I32Store16 { .. } |
			Operator::I32Store8 { .. } => { validator.pop_values(&[Type::I32, Type::I32]); }
			Operator::I64Load { .. } => { validator.push_pop_values(&[Type::I32], &[Type::I64]); }
			Operator::I64Store { .. } => { validator.pop_values(&[Type::I32, Type::I64]); }
			Operator::LocalSet { local_index } => { validator.pop_value_ty(Some(locals[*local_index as usize])); }
			Operator::LocalGet { local_index } => validator.push_value(locals[*local_index as usize]),
			Operator::LocalTee { local_index } => {
				let ty = locals[*local_index as usize];
				validator.push_pop_values(&[ty], &[ty]);
			}
			Operator::Select => {
				validator.pop_value_ty(Some(Type::I32));
				let t = validator.pop_value().unwrap();
				validator.pop_value_ty(Some(t));
				validator.push_value(t);
			}
			Operator::Drop => {
				validator.pop_value();
			}
			&Operator::Call { function_index } => {
				let called_ty = wasm_file.func_type(function_index as usize);
				validator.pop_values(&called_ty.params);
				validator.push_values(&called_ty.returns);
			}
			Operator::Block { ty } => {
				let block = builder.alloc_block();

				let start_types = wasm_file.types.start_types(*ty);
				let end_types = wasm_file.types.end_types(*ty);
				let control_op = ControlOp::Block(block);
				validator.push_ctrl(control_op, start_types, end_types);
			}
			Operator::Loop { ty } => {
				let block = builder.alloc_block();

				let start_types = wasm_file.types.start_types(*ty);
				let end_types = wasm_file.types.end_types(*ty);
				let control_op = ControlOp::Loop(block);
				validator.push_ctrl(control_op, start_types, end_types);
				builder.finish_block(SsaTerminator::Jump(block));
				builder.set_block(block);
			}
			Operator::End => {
				let frame = validator.pop_ctrl();
				validator.push_values(&frame.end_types);

				match frame.operator {
					ControlOp::Loop(_) => {},
					ControlOp::Block(b) => {
						builder.finish_block(SsaTerminator::Jump(b));
						builder.set_block(b);
					}
					ControlOp::If { false_label: _, next_label: _ } => {
						todo!("insert an else")
					},
					ControlOp::Else { next_label } => {
						builder.set_block(next_label);
					}
				}
			}
			&Operator::Br { relative_depth } => {
				let target_frame = &validator.control_stack[TopIndex(relative_depth as usize)];
				let label_types = target_frame.label_types().to_owned();
				let target_label = target_frame.operator.target_label();

				validator.pop_values(&label_types);
				validator.mark_unreachable();

				builder.finish_block(SsaTerminator::Jump(target_label));

				let new_block = builder.alloc_block();
				builder.set_block(new_block);
			}
			Operator::BrTable { table } => {
				validator.pop_value_ty(Some(Type::I32));

				let default_label_types = validator.control_stack[TopIndex(table.default() as usize)].label_types().to_owned();
				let arity = default_label_types.len();

				for arm in table.targets() {
					let arm = arm.unwrap();
					let arm_label_types = validator.control_stack[TopIndex(arm as usize)].label_types().to_owned();
					assert_eq!(arm_label_types.len(), arity);
					validator.push_pop_values(&arm_label_types, &arm_label_types);
				}

				validator.pop_values(&default_label_types);
				validator.mark_unreachable();
			}
			&Operator::BrIf { relative_depth } => {
				let target_frame = &validator.control_stack[TopIndex(relative_depth as usize)];
				let label_types = target_frame.label_types().to_owned();
				let true_label = target_frame.operator.target_label();

				validator.pop_value_ty(Some(Type::I32));
				validator.push_pop_values(&label_types, &label_types);

				let next_block = builder.alloc_block();
				builder.finish_block(SsaTerminator::BranchIf { true_label, false_label: next_block });
				builder.set_block(next_block);
			}
			_ => todo!("{:?}", op),
		}
	}

	assert_eq!(builder.current_block, 1);

	builder.finish_block(SsaTerminator::Return);

	println!("{:?}", func_ty);
	println!("{:?}", validator.control_stack);
	println!("{:?}", validator.value_stack);

	let (_, blocks) = builder.finish();

	for (_block_idx, (block_id, block)) in blocks.iter().enumerate() {
		println!("==== block {:?} ==== ", block_id);
		for instr in block.body.iter() {
			println!("{}", instr);
		}
		println!("{:?}\n", block.term);
	}
}