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

#[derive(Clone, Debug)]
struct ControlFrame<'a> {
	operator: Option<Operator<'a>>,
	start_types: Box<[Type]>,
	end_types: Box<[Type]>,
	height: usize,
	unreachable: bool,
}

impl<'a> ControlFrame<'a> {
	pub fn label_types(&self) -> &[Type] {
		if matches!(self.operator, Some(Operator::Loop { .. })) {
			&self.start_types
		} else {
			&self.end_types
		}
	}
}

struct TopIndex(usize);

#[derive(Default, Debug)]
struct ControlStack<'a>(Vec<ControlFrame<'a>>);

impl<'a> ControlStack<'a> {
	pub fn push(&mut self, operator: Option<Operator<'a>>, start_types: Box<[Type]>, end_types: Box<[Type]>, value_stack: &mut ValueStack) {
		let height = value_stack.0.len();

		value_stack.push_many(&start_types);

		let frame = ControlFrame {
			operator,
			start_types,
			end_types,
			height: value_stack.0.len(),
			unreachable: false
		};

		self.0.push(frame);
	}

	pub fn pop(&mut self, value_stack: &mut ValueStack) -> ControlFrame<'a> {
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

	pub fn top(&self) -> Option<&ControlFrame<'a>> {
		self.0.last()
	}
}

impl<'a> Index<TopIndex> for ControlStack<'a> {
    type Output = ControlFrame<'a>;

    fn index(&self, index: TopIndex) -> &Self::Output {
	assert!(!self.0.is_empty());
	let i = self.0.len() - 1 - index.0;
	&self.0[i]
    }
}

impl<'a> IndexMut<TopIndex> for ControlStack<'a> {
    fn index_mut(&mut self, index: TopIndex) -> &mut Self::Output {
	    assert!(!self.0.is_empty());
	    let i = self.0.len() - 1 - index.0;
	    &mut self.0[i]
    }
}

#[derive(Default, Debug)]
struct Validator<'a> {
	value_stack: ValueStack,
	control_stack: ControlStack<'a>,
}

impl<'a> Validator<'a> {
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
	
	pub fn push_ctrl(&mut self, operator: Option<Operator<'a>>, start_types: Box<[Type]>, end_types: Box<[Type]>) {
		self.control_stack.push(operator, start_types, end_types, &mut self.value_stack);
	}

	pub fn pop_ctrl(&mut self) -> ControlFrame<'a> {
		self.control_stack.pop(&mut self.value_stack)
	}
}

pub fn validate(wasm_file: &WasmFile, func: usize) {
	let func_ty = wasm_file.func_type(func);
	let func_body = wasm_file.func_body(func);
	let locals = wasm_file.func_locals(func);

	let mut validator = Validator::default();

	validator.push_ctrl(None, Box::new([]), func_ty.returns.clone());

	for op in func_body.operators.iter() {
		println!("{:?}", validator.value_stack);
		println!("{:?}", validator.control_stack);
		println!("{:?}", op);
		println!();

		match op {
			Operator::I32Const { .. } => validator.push_value(Type::I32),
			Operator::I32Add { .. } |
			Operator::I32GtS { .. } |
			Operator::I32GtU { .. } |
			Operator::I32LtS { .. } |
			Operator::I32LtU { .. } |
			Operator::I32Eq {.. } => validator.push_pop_values(&[Type::I32, Type::I32], &[Type::I32]),
			Operator::I32Load { .. } => validator.push_pop_values(&[Type::I32], &[Type::I32]),
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
			Operator::Block { ty } => {
				let start_types = wasm_file.types.start_types(*ty);
				let end_types = wasm_file.types.end_types(*ty);
				validator.push_ctrl(Some(op.clone()), start_types, end_types);
			}
			Operator::Loop { ty } => {
				let start_types = wasm_file.types.start_types(*ty);
				let end_types = wasm_file.types.end_types(*ty);
				validator.push_ctrl(Some(op.clone()), start_types, end_types);
			}
			Operator::End => {
				let frame = validator.pop_ctrl();
				validator.push_values(&frame.end_types);
			}
			&Operator::BrIf { relative_depth } => {
				validator.pop_value_ty(Some(Type::I32));
				let label_types = validator.control_stack[TopIndex(relative_depth as usize)].label_types().to_owned();
				validator.push_pop_values(&label_types, &label_types);
			}
			_ => todo!("{:?}", op),
		}
	}

	println!("{:?}", func_ty);
	println!("{:?}", validator.control_stack);
	println!("{:?}", validator.value_stack);
}