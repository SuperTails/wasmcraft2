use clap::Parser;
use wasmcraft::Args;

fn main() {
    let args = Args::parse();

    wasmcraft::run(args);
}
