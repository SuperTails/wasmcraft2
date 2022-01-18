mod wasm_file;
mod validator;
mod ssa;

pub fn run(path: &str) {
	let bytes = std::fs::read(path).unwrap();

	let file = wasm_file::WasmFile::from(&bytes[..]);

	println!("{:?}", file.types);
	println!("{:?}", file.globals);
	println!("{:?}", file.memory);
	println!("{:?}", file.exports);
	println!("{:?}", file.imports);
	println!("{:?}", file.data);
	println!("{:?}", file.tables);
	println!("<elements>");
	println!("{:?}", file.functions);
	//println!("{:?}", file.bodies);
	println!("{:?}", file.bodies.len());

	for i in 5..=5 {
		validator::validate(&file, i);
	}
}