use command_parser::CommandParse;
use datapack_common::functions::{command_components::NbtPath, Function};
use clap::{Parser, clap_derive::ValueEnum};
use lir::LirProgram;
use ssa::SsaProgram;
use wasm_file::WasmFile;

use crate::{validator::wasm_to_ssa, ssa::lir_emitter, lir::interp::LirInterpreter};

pub mod wasm_file;
pub mod validator;
pub mod ssa;
pub mod lir;
pub mod pack_emitter;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CodegenStage {
	Ssa,
	Lir,
	Datapack,
}

const CODEGEN_STAGE: CodegenStage = CodegenStage::Datapack;

const PERSIST_PROGRAM: bool = false;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum RegAllocMode {
    /// No-op regalloc. Faster compilation, slower output.
    Noop,
    /// Full regalloc. Slower compilation, faster output.
    Full,
}

#[derive(Parser, Debug)]
pub struct Args {
    /// Path to the binary WebAssembly file.
    #[clap(value_parser, value_hint = clap::ValueHint::FilePath)]
    input: std::path::PathBuf,

    /// Run the generated output under the interpreter.
    #[clap(short = 'r', long, action)]
    run_output: bool,

	/// Overall optimization level.
	#[clap(short = 'O', value_parser = clap::value_parser!(u8).range(0..=1), default_value_t)]
	opt_level: u8,

    /// Which form of register allocation to use.
    #[clap(short = 'a', long, value_enum)]
    regalloc: Option<RegAllocMode>,

	/// Perform SSA constant propogation
	#[clap(short = 'a', long = "do-const-prop", action = clap::ArgAction::Set)]
	do_const_prop: Option<bool>,

	/// Perform SSA dead code elimination.
	#[clap(short = 'e', long = "do-dce", action = clap::ArgAction::Set)]
	do_dead_code_elim: Option<bool>,

	/// Prevent the datapack from actually being saved to disk.
	#[clap(long, action)]
	no_persist_output: bool,

	/// Prints some parts of the input WebAssembly to stdout
	#[clap(long, action)]
	dump_wasm: bool,

	/// Prints the compiled LIR functions to stdout
	#[clap(long, action)]
	dump_lir: bool,

	/// Prints the compiled datapack functions to stdout
	#[clap(long, action)]
	dump_datapack: bool,

    /// Path to place the output datapack.
    /// Any files previously in this directory will be deleted.
    #[clap(short = 'o', value_parser, value_hint = clap::ValueHint::DirPath)]
    output: std::path::PathBuf,
}

pub struct CompileContext {
    /// Path to the binary WebAssembly file.
	input: std::path::PathBuf,
    /// Path to place the output datapack.
    /// Any files previously in this directory will be deleted.
    output: std::path::PathBuf,

    /// Run the generated output under the interpreter.
    run_output: bool,
	/// Save the datapack to disk.
	persist_output: bool,

    /// Which form of register allocation to use.
    regalloc: RegAllocMode,
	/// Perform SSA constant propogation
	do_const_prop: bool,
	/// Perform SSA dead code elimination
	do_dead_code_elim: bool,

	/// Print some parts of the input WebAssembly to stdout
	dump_wasm: bool,
	/// Print the compiled LIR functions to stdout
	dump_lir: bool,
	/// Print the compiled datapack functions to stdout
	dump_datapack: bool,
}

impl CompileContext {
	pub fn new(args: Args) -> Self {
		let default_regalloc: RegAllocMode;
		let default_const_prop: bool;
		let default_dead_code_elim: bool;

		match args.opt_level {
			0 => {
				default_regalloc = RegAllocMode::Noop;
				default_const_prop = false;
				default_dead_code_elim = false;
			}
			1 => {
				default_regalloc = RegAllocMode::Full;
				default_const_prop = true;
				default_dead_code_elim = true;
			}
			_ => unreachable!(),
		}

		let regalloc = args.regalloc.unwrap_or(default_regalloc);
		let do_const_prop = args.do_const_prop.unwrap_or(default_const_prop);
		let do_dead_code_elim = args.do_dead_code_elim.unwrap_or(default_dead_code_elim);

		CompileContext {
			input: args.input, output: args.output,
			run_output: args.run_output, persist_output: !args.no_persist_output,
			regalloc, do_const_prop, do_dead_code_elim,
			dump_wasm: args.dump_wasm, dump_lir: args.dump_lir, dump_datapack: args.dump_datapack
		}
	}

	pub fn new_from_opt(opt_level: u8) -> Self {
		let regalloc: RegAllocMode;
		let do_const_prop: bool;
		let do_dead_code_elim: bool;

		match opt_level {
			0 => {
				regalloc = RegAllocMode::Noop;
				do_const_prop = false;
				do_dead_code_elim = false;
			}
			1 => {
				regalloc = RegAllocMode::Full;
				do_const_prop = true;
				do_dead_code_elim = true;
			}
			_ => panic!("{:?}", opt_level),
		}

		CompileContext {
			input: Default::default(), output: Default::default(),
			run_output: true, persist_output: true,
			regalloc, do_const_prop, do_dead_code_elim,
			dump_wasm: false, dump_lir: false, dump_datapack: false,
		}

	}

	pub fn compute_wasm_file<'a>(&self, bytes: &'a [u8]) -> WasmFile<'a> {
		let file = wasm_file::WasmFile::from(bytes);

		if self.dump_wasm {
			println!("WebAssembly file:");
			println!("{:?}", file.types);
			println!("{:?}", file.globals);
			println!("{:?}", file.memory);
			println!("{:?}", file.exports);
			println!("{:?}", file.imports);
			println!("{:?}", file.tables);
			println!("{:?}", file.functions);
			println!("{:?}", file.bodies.len());
		}

		file
	}

	pub fn compute_ssa(&self, wasm_file: &WasmFile) -> SsaProgram {
		wasm_to_ssa(self, wasm_file)
	}

	pub fn compute_lir(&self, ssa_program: SsaProgram) -> LirProgram {
		lir_emitter::convert(self, ssa_program)
	}

	pub fn compute_datapack(&self, lir_program: &LirProgram) -> Vec<Function> {
		pack_emitter::emit_program(lir_program)
	}
}

pub fn run(args: Args) {
	let ctx = CompileContext::new(args);

	let bytes = std::fs::read(&ctx.input).unwrap();

	let file = ctx.compute_wasm_file(&bytes);
	
	let ssa_program = ctx.compute_ssa(&file);

	if CODEGEN_STAGE == CodegenStage::Ssa {
		if ctx.run_output {
			let start_idx = file.exports.find_func("_start").unwrap();

			let func = ssa_program.code.iter().find(|f| f.func_id == start_idx).unwrap();
			dbg!(func.code.len());

			let mut interp = ssa::interp::SsaInterpreter::new(ssa_program);

			interp.call(start_idx, vec![]);

			interp.run_until_halted();
		}

		return;
	}

	let lir_program = ctx.compute_lir(ssa_program);

	if CODEGEN_STAGE == CodegenStage::Lir {
		if ctx.run_output {
			todo!("run the LIR interpreter");
		}

		return;
	}

	let datapack = ctx.compute_datapack(&lir_program);
	
	/*for (idx, lir_func) in lir_program.code.iter().enumerate() {
		println!("\n\n============ Function {} ============", idx);
		for (block_id, block) in lir_func.code.iter() {
			println!("\n----------- Block {:?} ---------", block_id);
			for instr in block.body.iter() {
				println!("    {:?}", instr);
			}
			println!("    {:?}", block.term);
		}
	}*/

	if ctx.dump_datapack {
		for func in datapack.iter() {
			println!("-------- func {} --------", func.id);
			for cmd in func.cmds.iter() {
				println!("\t{}", cmd);
			}
			println!();
		}
	}

	if ctx.persist_output {
		pack_emitter::persist_program(std::path::Path::new(&ctx.output), &datapack);
	}

	if ctx.run_output {
		run_datapack_output(datapack);
	}
}

fn run_datapack_output(datapack: Vec<Function>) {
	let mut indiv_time = vec![0; datapack.len()];
	let mut intrin_cum_times = vec![0; datapack.len()];
	let mut intrin_visited = vec![false; datapack.len()];
	let intrin_funcs = datapack.iter().map(|func| func.id.namespace == "intrinsic").collect::<Vec<_>>();


	let mut interp = datapack_vm::Interpreter::new(datapack, 0);


	let (_, func_name) = datapack_common::functions::command_components::FunctionIdent::parse_from_command("wasmrunner:init").unwrap();

	let interp_idx = interp.get_func_idx(&func_name);
	interp.set_pos(interp_idx);

	interp.run_to_end().unwrap();

	let (_, func_name) = datapack_common::functions::command_components::FunctionIdent::parse_from_command("wasmrunner:_start").unwrap();

	let interp_idx = interp.get_func_idx(&func_name);
	interp.set_pos(interp_idx);

	while !interp.halted() {
		for v in intrin_visited.iter_mut() {
			*v = false;
		}

		if interp.total_commands_run > 5_000_000 { // Don't start recording time until we enter the hot loop.
			if let Some((func_idx, _)) = interp.call_stack_raw().iter().rev().next() {
				indiv_time[*func_idx] += 1;
			}

			for &(func_idx, _) in interp.call_stack_raw().iter().rev() {
				if !intrin_visited[func_idx] {
					intrin_visited[func_idx] = true;
					intrin_cum_times[func_idx] += 1;
				}

				if !intrin_funcs[func_idx] {
					break
				}
			}
		}

		let result = interp.step();
		if let Err(result) = result {
			println!("ERR: {:?}", result);
			break
		}

		if interp.call_stack_depth() == 0 {
			let name = "wasm:returnstack".to_string();
			let path: NbtPath = command_parser::parse_command("stack.data").unwrap();
			if interp.nbt_storage.contains_path(&name, &path) {
				println!("return stack was not empty");
				break;
			}
		}

		if interp.call_stack_depth() == 1 {
			let top = interp.call_stack_top().unwrap().0;
			if top.id.namespace == "wasmrunner" && top.id.path == "wasm_8_3" {
				println!("Panicked!");
				break;
			}
		}
	}

	println!("Call stack:");
	for entry in interp.call_stack() {
		print!("{{ {}, {} }},", entry.0.id, entry.1);
	}
	println!();

	println!("NBT Storage:");
	for (key, value) in interp.nbt_storage.0.iter() {
		println!("{key}: {value}");
	}

	let mut traces = indiv_time.iter().enumerate().map(|(i, t)| (&interp.program[i], *t)).collect::<Vec<_>>();
	traces.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
	let total: usize = traces.iter().map(|(_, c)| *c).sum();

	println!("\nTOTAL: {}", total);
	for (func, count) in traces {
		println!("{}: {}", func.id, count);
		if count < 1000 {
			break;
		}
	}

	println!("\nIntrinsic cumulative times:");
	let mut intrin_cum_times = intrin_cum_times.iter().enumerate().map(|(f, c)| (&interp.program[f], *c)).collect::<Vec<_>>();
	intrin_cum_times.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
	for (func, count) in intrin_cum_times.iter() {
		if *count < 1000 {
			break;
		}
		println!("{}: {}", func.id, count);
	}

	println!("Ticks run: {}", interp.tick);

}

// TODO: Test mixed-tick tables

#[derive(Debug, PartialEq, Eq)]
pub enum JumpMode {
	Direct,
}

pub fn jump_mode() -> JumpMode {
	JumpMode::Direct
}