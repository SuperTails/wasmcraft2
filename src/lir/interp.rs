use std::collections::HashMap;

use wasmparser::Type;

use crate::{ssa::{interp::TypedValue, BlockId, Memory, Table}, jump_mode, JumpMode, lir::LirInstr};

use super::{Register, LirBasicBlock, DoubleRegister, LirProgram, LirTerminator, Condition, Half};

#[derive(Debug)]
pub struct Pc {
	block: BlockId,
	instr: usize,
}

#[derive(Default, Debug)]
pub struct CallStack(Vec<Pc>);

impl CallStack {
	pub fn is_empty(&self) -> bool {
		self.0.is_empty()
	}

	pub fn last_mut(&mut self) -> Option<&mut Pc> {
		self.0.last_mut()
	}

	pub fn incr(&mut self, code: &HashMap<BlockId, LirBasicBlock>) {
		let last = self.last_mut().unwrap();
		let block = code.get(&last.block).unwrap();
		if last.instr == block.body.len() {
			self.0.pop();
		} else {
			last.instr += 1;
		}
	}

	pub fn push(&mut self, pc: Pc) {
		self.0.push(pc);
	}
}

#[derive(Default)]
pub struct LocalStack(Vec<Vec<TypedValue>>);

impl LocalStack {
	pub fn push(&mut self, frame: &[Type]) {
		let frame_vals = frame.iter().map(|ty| {
			match ty {
				Type::I32 => TypedValue::I32(0),
				Type::I64 => TypedValue::I64(0),
				_ => todo!(),
			}
		}).collect();

		self.0.push(frame_vals);
	}

	pub fn pop(&mut self, frame: &[Type]) {
		let old_frame = self.0.pop().unwrap();

		assert_eq!(old_frame.len(), frame.len());
		assert!(old_frame.iter().zip(frame.iter()).all(|(o, f)| o.ty() == *f));
	}

	pub fn set(&mut self, id: u32, half: Half, val: i32) {
		let frame = self.0.last_mut().unwrap();

		match (half, &mut frame[id as usize]) {
			(Half::Lo, TypedValue::I32(i)) => {
				*i = val;
			}
			(Half::Lo, TypedValue::I64(i)) => {
				*i >>= 32;
				*i <<= 32;
				*i |= val as u32 as i64;
			}
			(Half::Hi, TypedValue::I32(_)) => panic!(),
			(Half::Hi, TypedValue::I64(i)) => {
				*i = ((val as u32 as i64) << 32) | (*i as u32 as i64);
			}
		}
	}

	pub fn get(&self, id: u32, half: Half) -> i32 {
		let frame = self.0.last().unwrap();

		match (half, frame[id as usize]) {
			(Half::Lo, TypedValue::I32(i)) => i,
			(Half::Lo, TypedValue::I64(i)) => i as i32,
			(Half::Hi, TypedValue::I32(_)) => panic!(),
			(Half::Hi, TypedValue::I64(i)) => (i >> 32) as i32,
		}
	}
}

#[derive(Default)]
pub struct RegContext(HashMap<Register, i32>);

impl RegContext {
	pub fn set(&mut self, reg: Register, val: i32) {
		assert!(!matches!(reg.double, DoubleRegister::Const(_)));

		self.0.insert(reg, val);
	}

	pub fn set_64(&mut self, reg: DoubleRegister, val: i64) {
		self.set(reg.lo(), val as i32);
		self.set(reg.hi(), (val >> 32) as i32);
	}

	pub fn get(&mut self, reg: Register) -> i32 {
		if let DoubleRegister::Const(c) = reg.double {
			return c;
		} else {
			// TODO: Remove this
			self.0.entry(reg).or_insert(0);
		}

		*self.0.get(&reg).unwrap_or_else(|| panic!("uninit reg {:?}", reg))
	}

	pub fn get_64(&mut self, reg: DoubleRegister) -> i64 {
		let lo = self.get(reg.lo());
		let hi = self.get(reg.hi());

		((hi as i64) << 32) | (lo as u32 as i64)
	}
}

pub struct GlobalList(Vec<TypedValue>);

impl GlobalList {
	pub fn new(data: Vec<TypedValue>) -> Self {
		GlobalList(data)
	}

	pub fn get(&self, id: u32, half: Half) -> i32 {
		match (half, self.0[id as usize]) {
			(Half::Lo, TypedValue::I32(i)) => i,
			(Half::Lo, TypedValue::I64(i)) => i as i32,
			(Half::Hi, TypedValue::I32(_)) => panic!(),
			(Half::Hi, TypedValue::I64(i)) => (i >> 32) as i32,
		}
	}

	pub fn set(&mut self, id: u32, half: Half, val: i32) {
		match (half, &mut self.0[id as usize]) {
			(Half::Lo, TypedValue::I32(i)) => {
				*i = val;
			}
			(Half::Lo, TypedValue::I64(i)) => {
				*i >>= 32;
				*i <<= 32;
				*i |= val as u32 as i64;
			}
			(Half::Hi, TypedValue::I32(_)) => panic!(),
			(Half::Hi, TypedValue::I64(i)) => {
				*i = ((val as u32 as i64) << 32) | (*i as u32 as i64);
			}
		}
	}
}

#[derive(Debug, Default)]
pub struct ReturnStack {
	stack: Vec<Option<BlockId>>,
}

impl ReturnStack {
	pub fn push(&mut self, block_id: Option<BlockId>) {
		self.stack.push(block_id)
	}

	pub fn pop(&mut self) -> Option<BlockId> {
		self.stack.pop().unwrap()
	}

	pub fn is_empty(&self) -> bool {
		self.stack.is_empty()
	}
}

pub struct LirInterpreter {
	globals: GlobalList,
	data_stack: Vec<i32>,
	local_stack: LocalStack,
	call_stack: CallStack,
	return_stack: ReturnStack,
	registers: RegContext,
	memory: Vec<Memory>,
	tables: Vec<Table>,
	returns: HashMap<usize, Box<[Type]>>,
	code: HashMap<BlockId, LirBasicBlock>,
	scheduled: Option<BlockId>,
}

impl LirInterpreter {
	pub fn new(program: LirProgram) -> Self {
		let LirProgram { globals, memory, tables, code, constants: _, exports: _ /* TODO: */ } = program;

		let globals = GlobalList::new(globals);

		let mut returns = HashMap::new();
		for func in code.iter() {
			returns.insert(func.func_id(), func.returns.clone());
		}

		let code = code.into_iter().flat_map(|func| func.code).collect();

		LirInterpreter {
			globals,
			data_stack: Vec::new(),
			local_stack: LocalStack::default(),
			call_stack: CallStack::default(),
			return_stack: ReturnStack::default(),
			registers: RegContext::default(),
			memory,
			tables,
			returns,
			code,
			scheduled: None,
		}
	}

	pub fn call(&mut self, func: usize, params: &[TypedValue]) {
		for (idx, param) in params.iter().enumerate() {
			match param {
				TypedValue::I32(v) => {
					self.registers.set(Register::param_lo(idx as u32), *v);
				}
				TypedValue::I64(v) => {
					self.registers.set_64(DoubleRegister::param(idx as u32), *v);
				}
			}
		}

		self.call_stack = CallStack(vec![Pc { block: BlockId { func, block: 0 }, instr: 0 }]);
	}

	pub fn run_until_halted(&mut self) -> Vec<TypedValue> {
		loop {
			if let Some(result) = self.step() {
				assert!(self.is_halted());
				return result;
			}
		}
	}

	pub fn is_halted(&self) -> bool {
		self.call_stack.is_empty() && self.scheduled.is_none()
	}

	pub fn check_cond(&mut self, cond: &Condition) -> bool {
		match cond {
			Condition::Matches(reg, range) => {
				let val = self.registers.get(*reg);
				range.contains(&val)
			}
			Condition::NotMatches(reg, range) => {
				let val = self.registers.get(*reg);
				!range.contains(&val)
			}
		}
	}

	pub fn exec_instr(&mut self, instr: &LirInstr) {
		fn do_assignop<F>(lhs: Register, rhs: Register, regs: &mut RegContext, f: F)
			where F: FnOnce(i32, i32) -> i32
		{
			let lhs_val = regs.get(lhs);
			let rhs_val = regs.get(rhs);
			regs.set(lhs, f(lhs_val, rhs_val));
		}

		fn do_unaryop<F>(dst: Register, src: Register, regs: &mut RegContext, f: F)
			where F: FnOnce(i32) -> i32
		{
			let val = regs.get(src);
			regs.set(dst, f(val));
		}

		fn do_unaryop64<F>(dst: DoubleRegister, src: DoubleRegister, regs: &mut RegContext, f: F)
			where F: FnOnce(i64) -> i64
		{
			let val = regs.get_64(src);
			regs.set_64(dst, f(val));
		}

		fn do_binaryop<F>(dst: Register, lhs: Register, rhs: Register, regs: &mut RegContext, f: F)
			where F: FnOnce(i32, i32) -> i32
		{
			let lhs = regs.get(lhs);
			let rhs = regs.get(rhs);
			regs.set(dst, f(lhs, rhs));
		}

		fn do_binaryop64<F>(dst: DoubleRegister, lhs: DoubleRegister, rhs: DoubleRegister, regs: &mut RegContext, f: F)
			where F: FnOnce(i64, i64) -> i64
		{
			let lhs = regs.get_64(lhs);
			let rhs = regs.get_64(rhs);
			regs.set_64(dst, f(lhs, rhs));
		}

		fn do_compareop64<F>(dst: Register, lhs: DoubleRegister, rhs: DoubleRegister, regs: &mut RegContext, f: F)
			where F: FnOnce(i64, i64) -> bool
		{
			let lhs = regs.get_64(lhs);
			let rhs = regs.get_64(rhs);
			regs.set(dst, f(lhs, rhs) as i32);
		}

		fn do_store(src: Register, addr: Register, bytes: usize, regs: &mut RegContext, mem: &mut [Memory])
		{
			let mem = &mut mem[0];

			let src = regs.get(src);
			let addr = regs.get(addr);

			let data = &src.to_le_bytes()[..bytes];

			mem.data[addr as usize..][..bytes].copy_from_slice(data);
		}

		fn do_load(dst: Register, addr: Register, bytes: usize, regs: &mut RegContext, mem: &mut [Memory]) {
			let addr = regs.get(addr);

			let mut data = [0; 4];
			data[..bytes].copy_from_slice(&mem[0].data[addr as usize..][..bytes]);
			let data = i32::from_le_bytes(data);

			regs.set(dst, data);
		}



		let mut incr_pc = true;

		match instr {
			&LirInstr::Set(dst, val) => self.registers.set(dst, val),
			&LirInstr::Assign(lhs, rhs) => {
				let val = self.registers.get(rhs);
				self.registers.set(lhs, val);
			}
			&LirInstr::Add(lhs, rhs) => do_assignop(lhs, rhs, &mut self.registers, |a, b| a.wrapping_add(b)),
			&LirInstr::Sub(lhs, rhs) => do_assignop(lhs, rhs, &mut self.registers, |a, b| a.wrapping_sub(b)),
			&LirInstr::Mul(lhs, rhs) => do_assignop(lhs, rhs, &mut self.registers, |a, b| a.wrapping_mul(b)),
			&LirInstr::DivS(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| a.wrapping_div(b)),
			&LirInstr::DivU(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| (a as u32).wrapping_div(b as u32) as i32),
			&LirInstr::RemS(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| a.wrapping_rem(b)),
			&LirInstr::RemU(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| (a as u32).wrapping_rem(b as u32) as i32),

			&LirInstr::MulTo64 (dst, lhs, rhs) => {
				let l = self.registers.get(lhs);
				let r = self.registers.get(rhs);

				self.registers.set_64(dst, (l as u32 as i64) * (r as u32 as i64));
			}

			&LirInstr::Add64 (dst, lhs, rhs) => do_binaryop64(dst, lhs, rhs, &mut self.registers, |a, b| a.wrapping_add(b)),
			&LirInstr::Sub64 (dst, lhs, rhs) => do_binaryop64(dst, lhs, rhs, &mut self.registers, |a, b| a.wrapping_sub(b)),
			&LirInstr::DivS64(dst, lhs, rhs) => do_binaryop64(dst, lhs, rhs, &mut self.registers, |a, b| a.wrapping_div(b)),
			&LirInstr::DivU64(dst, lhs, rhs) => do_binaryop64(dst, lhs, rhs, &mut self.registers, |a, b| (a as u64).wrapping_div(b as u64) as i64),
			&LirInstr::RemS64(dst, lhs, rhs) => do_binaryop64(dst, lhs, rhs, &mut self.registers, |a, b| a.wrapping_rem(b)),
			&LirInstr::RemU64(dst, lhs, rhs) => do_binaryop64(dst, lhs, rhs, &mut self.registers, |a, b| (a as u64).wrapping_rem(b as u64) as i64),
			&LirInstr::Shl (dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| a << b.rem_euclid(32)),
			&LirInstr::ShrS(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| a >> b.rem_euclid(32)),
			&LirInstr::ShrU(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| (a as u32 >> b.rem_euclid(32)) as i32),
			&LirInstr::Rotl(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| a.rotate_left(b.rem_euclid(32) as u32)),
			&LirInstr::Rotr(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| a.rotate_right(b.rem_euclid(32) as u32)),
			&LirInstr::Shl64 (dst, lhs, rhs) => do_binaryop64(dst, lhs, rhs, &mut self.registers, |a, b| a << b.rem_euclid(64)),
			&LirInstr::ShrS64(dst, lhs, rhs) => do_binaryop64(dst, lhs, rhs, &mut self.registers, |a, b| a >> b.rem_euclid(64)),
			&LirInstr::ShrU64(dst, lhs, rhs) => do_binaryop64(dst, lhs, rhs, &mut self.registers, |a, b| ((a as u64) >> b.rem_euclid(64)) as i64),
			&LirInstr::Rotl64(dst, lhs, rhs) => do_binaryop64(dst, lhs, rhs, &mut self.registers, |a, b| a.rotate_left(b.rem_euclid(64) as u32)),
			&LirInstr::Rotr64(dst, lhs, rhs) => do_binaryop64(dst, lhs, rhs, &mut self.registers, |a, b| a.rotate_right(b.rem_euclid(64) as u32)),
			&LirInstr::Xor(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| a ^ b),
			&LirInstr::And(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| a & b),
			&LirInstr::Or (dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| a | b),
			&LirInstr::PopcntAdd(dst, src) => {
				let src = self.registers.get(src);
				let old_dst = self.registers.get(dst);
				self.registers.set(dst, old_dst + src.count_ones() as i32);
			}

			// TODO: Check case with all zeros
			&LirInstr::Ctz(dst, src) => do_unaryop(dst, src, &mut self.registers, |a| a.trailing_zeros() as i32), 
			// TODO: Check case with all zeros
			&LirInstr::Clz(dst, src) => do_unaryop(dst, src, &mut self.registers, |a| a.leading_zeros() as i32),

			// TODO: Check case with all zeros
			&LirInstr::Ctz64(dst, src) => do_unaryop64(dst, src, &mut self.registers, |a| a.trailing_zeros() as i64),
			// TODO: Check case with all zeros
			&LirInstr::Clz64(dst, src) => do_unaryop64(dst, src, &mut self.registers, |a| a.leading_zeros() as i64),

			&LirInstr::Eqz(dst, src) => do_unaryop(dst, src, &mut self.registers, |a| (a == 0) as i32),
			&LirInstr::Eqz64(dst, src) => {
				let src = self.registers.get_64(src);
				self.registers.set(dst, (src == 0) as i32);
			}

			&LirInstr::GtS(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| (a > b) as i32),
			&LirInstr::GtU(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| ((a as u32) > (b as u32)) as i32),
			&LirInstr::GeS(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| (a >= b) as i32),
			&LirInstr::GeU(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| ((a as u32) >= (b as u32)) as i32),
			&LirInstr::LtS(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| (a < b) as i32),
			&LirInstr::LtU(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| ((a as u32) < (b as u32)) as i32),
			&LirInstr::LeS(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| (a <= b) as i32),
			&LirInstr::LeU(dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| ((a as u32) <= (b as u32)) as i32),
			&LirInstr::Eq (dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| (a == b) as i32),
			&LirInstr::Ne (dst, lhs, rhs) => do_binaryop(dst, lhs, rhs, &mut self.registers, |a, b| (a != b) as i32),

			&LirInstr::GtS64(dst, lhs, rhs) => do_compareop64(dst, lhs, rhs, &mut self.registers, |a, b| a > b),
			&LirInstr::GtU64(dst, lhs, rhs) => do_compareop64(dst, lhs, rhs, &mut self.registers, |a, b| (a as u64) > (b as u64)),
			&LirInstr::GeS64(dst, lhs, rhs) => do_compareop64(dst, lhs, rhs, &mut self.registers, |a, b| a >= b),
			&LirInstr::GeU64(dst, lhs, rhs) => do_compareop64(dst, lhs, rhs, &mut self.registers, |a, b| (a as u64) >= (b as u64)),
			&LirInstr::LtS64(dst, lhs, rhs) => do_compareop64(dst, lhs, rhs, &mut self.registers, |a, b| a < b),
			&LirInstr::LtU64(dst, lhs, rhs) => do_compareop64(dst, lhs, rhs, &mut self.registers, |a, b| (a as u64) < (b as u64)),
			&LirInstr::LeS64(dst, lhs, rhs) => do_compareop64(dst, lhs, rhs, &mut self.registers, |a, b| a <= b),
			&LirInstr::LeU64(dst, lhs, rhs) => do_compareop64(dst, lhs, rhs, &mut self.registers, |a, b| (a as u64) <= (b as u64)),
			&LirInstr::Eq64 (dst, lhs, rhs) => do_compareop64(dst, lhs, rhs, &mut self.registers, |a, b| a == b),
			&LirInstr::Ne64 (dst, lhs, rhs) => do_compareop64(dst, lhs, rhs, &mut self.registers, |a, b| a != b),

			&LirInstr::Trunc(arg, bits) => {
				let old_val = self.registers.get(arg);
				let new_val = match bits {
					32 => old_val,
					16 => old_val as u16 as i32,
					8 => old_val as u8 as i32,
					_ => panic!(),
				};
				self.registers.set(arg, new_val);
			}

			&LirInstr::SignExtend(_, _) => todo!(),

			&LirInstr::SignExtend8 (arg) => {
				let val = self.registers.get(arg) as i8 as i32;
				self.registers.set(arg, val);
			},
			&LirInstr::SignExtend16(arg) => {
				let val = self.registers.get(arg) as i16 as i32;
				self.registers.set(arg, val);
			}
			&LirInstr::SignExtend32(arg) => {
				let val = self.registers.get_64(arg) as i32 as i64;
				self.registers.set_64(arg, val);
			}

			&LirInstr::LocalSet(dst, dst_half, src) => self.local_stack.set(dst, dst_half, self.registers.get(src)),
			&LirInstr::LocalGet(dst, src, src_half) => self.registers.set(dst, self.local_stack.get(src, src_half)),

			&LirInstr::GlobalSet(dst, dst_half, src) => self.globals.set(dst, dst_half, self.registers.get(src)),
			&LirInstr::GlobalGet(dst, src, src_half) => self.registers.set(dst, self.globals.get(src, src_half)),

			&LirInstr::Store32(src, addr) => do_store(src, addr, 4, &mut self.registers, &mut self.memory),
			&LirInstr::Store16(src, addr) => do_store(src, addr, 2, &mut self.registers, &mut self.memory),
			&LirInstr::Store8(src, addr) => do_store(src, addr, 1, &mut self.registers, &mut self.memory),

			&LirInstr::Load32(dst, addr) => do_load(dst, addr, 4, &mut self.registers, &mut self.memory),
			&LirInstr::Load16(dst, addr) => do_load(dst, addr, 2, &mut self.registers, &mut self.memory),
			&LirInstr::Load8 (dst, addr) => do_load(dst, addr, 1, &mut self.registers, &mut self.memory),

			&LirInstr::Select { dst, true_reg, false_reg, cond } => {
				let cond = self.registers.get(cond);
				if cond != 0 {
					let val = self.registers.get(true_reg);
					self.registers.set(dst, val);
				} else {
					let val = self.registers.get(false_reg);
					self.registers.set(dst, val);
				}
			}
			&LirInstr::Call { func } => {
				incr_pc = false;

				self.call_stack.incr(&self.code);

				let new_pc = Pc { block: BlockId { func: func as usize, block: 0 }, instr: 0 };

				self.call_stack.0.push(new_pc);
			}
			&LirInstr::CallIndirect { table_index, table_entry } => {
				let table_entry = self.registers.get(table_entry);
				let table_entry = usize::try_from(table_entry).unwrap();

				let function_index = self.tables[table_index as usize].elements[table_entry].unwrap();

				let block = BlockId { func: function_index, block: 0 };

				incr_pc = false;

				self.call_stack.incr(&self.code);

				self.call_stack.0.push(Pc { block, instr: 0 });
			}

			LirInstr::Push(src) => for &src in src.iter() { self.data_stack.push(self.registers.get(src)) },
			LirInstr::Pop(dst) => for &dst in dst.iter().rev() { self.registers.set(dst, self.data_stack.pop().unwrap()) },

			super::LirInstr::IfCond { cond, instr } => {
				let cond = self.check_cond(cond);

				if cond {
					incr_pc = false;

					self.exec_instr(&*instr);
				}
			}

			LirInstr::PushLocalFrame(tys) => {
				self.local_stack.push(tys);
			}
			LirInstr::PopLocalFrame(tys) => {
				self.local_stack.pop(tys);
			}

			LirInstr::PushReturnAddr(block_id) => {
				self.return_stack.push(Some(*block_id));
			}

			LirInstr::TurtleSetX(_) |
			LirInstr::TurtleSetY(_) |
			LirInstr::TurtleSetZ(_) |
			LirInstr::TurtleSetBlock(_) => panic!("attempt to use minecraft IO from LIR interpreter"),
		}

		if incr_pc {
			self.call_stack.incr(&self.code);
		}
	}

	pub fn step(&mut self) -> Option<Vec<TypedValue>> {
		//println!("{:?}", self.call_stack);

		if self.call_stack.is_empty() {
			self.call_stack.push(Pc{ block: self.scheduled.take().unwrap(), instr: 0 });
		}

		let pc = self.call_stack.last_mut().expect("stepped while halted");

		let block = self.code.get(&pc.block).unwrap();

		if pc.instr == block.body.len() {
			match &block.term {
				LirTerminator::ScheduleJump(block_id, _delay) => {
					assert!(self.scheduled.is_none());

					self.scheduled = Some(*block_id);

					self.call_stack.incr(&self.code);

					None
				}
				LirTerminator::Jump(block_id) => {
					if jump_mode() == JumpMode::Direct {
						self.call_stack.incr(&self.code);

						self.call_stack.push(Pc { block: *block_id, instr: 0 });

						None
					} else {
						todo!()
					}
				}
				LirTerminator::JumpIf { true_label, false_label, cond } => {
					if jump_mode() == JumpMode::Direct {
						self.call_stack.incr(&self.code);

						let cond = self.registers.get(*cond);

						if cond != 0 {
							self.call_stack.push(Pc { block: *true_label, instr: 0 });
						} else {
							self.call_stack.push(Pc { block: *false_label, instr: 0 });
						}

						None
					} else {
						todo!()
					}
				}
				LirTerminator::JumpTable { arms, default, cond } => {
					if jump_mode() == JumpMode::Direct {
						self.call_stack.incr(&self.code);

						let cond = self.registers.get(*cond);

						if cond < 0 || cond as usize >= arms.len() {
							self.call_stack.push(Pc { block: default.unwrap(), instr: 0 });
						} else {
							self.call_stack.push(Pc { block: arms[cond as usize].unwrap(), instr: 0 });
						}

						None
					} else {
						todo!()
					}
				}
				LirTerminator::Return => {
					let returns = self.returns.get(&pc.block.func).unwrap();

					self.call_stack.incr(&self.code);

					if self.call_stack.is_empty() {
						let return_vals = returns.iter().enumerate().map(|(idx, return_ty)| {
							match *return_ty {
								Type::I32 => {
									TypedValue::I32(self.registers.get(Register::return_lo(idx as u32)))
								}
								Type::I64 => {
									TypedValue::I64(self.registers.get_64(DoubleRegister::return_reg(idx as u32)))
								}
								_ => todo!(),
							}
						}).collect();

						Some(return_vals)
					} else {
						None
					}
				}
				LirTerminator::ReturnToSaved => {
					let returns = self.returns.get(&pc.block.func).unwrap();

					let mut return_addr = self.return_stack.pop();

					self.call_stack.incr(&self.code);

					if self.call_stack.is_empty() {
						assert!(return_addr.is_none());

						let return_vals = returns.iter().enumerate().map(|(idx, return_ty)| {
							match *return_ty {
								Type::I32 => {
									TypedValue::I32(self.registers.get(Register::return_lo(idx as u32)))
								}
								Type::I64 => {
									TypedValue::I64(self.registers.get_64(DoubleRegister::return_reg(idx as u32)))
								}
								_ => todo!(),
							}
						}).collect();

						Some(return_vals)
					} else {
						let return_addr = return_addr.unwrap();

						self.call_stack.incr(&self.code);

						self.call_stack.push(Pc { block: return_addr, instr: 0 });

						None
					}
				}
			}
		} else {
			let instr = block.body[pc.instr].clone();

			self.exec_instr(&instr);

			None
		}
	}
}