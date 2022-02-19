fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    let path = &*args[0];

    let out = args.get(1).map(|s| &**s).unwrap_or("../out");

    wasm_runner::run(path, out);
}
