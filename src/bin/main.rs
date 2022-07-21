use clap::Parser;
use wasm_runner::Args;

fn main() {
    let args = Args::parse();

    wasm_runner::run(args);
}
