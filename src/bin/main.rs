fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    let path = &*args[0];

    wasm_runner::run(path);
}
