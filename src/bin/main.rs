fn print_usage_string() {
    eprintln!("Usage:");
    eprintln!("cargo run --release -- <PATH_TO_WASM_FILE> [PATH_TO_DATAPACK]");
    eprintln!("(Datapack is placed by default at ../out)");
}

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    let (input, output) = match &args[..] {
        [] => {
            eprintln!("No paths provided!");
            print_usage_string();
            return;
        }
        [input] => {
            (input.as_str(), "../out")
        }
        [input, output] => {
            (input.as_str(), output.as_str())
        }
        _ => {
            eprintln!("Too many arguments!");
            print_usage_string();
            return;
        }
    };

    wasm_runner::run(input, output);
}
