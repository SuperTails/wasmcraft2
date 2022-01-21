enum Half {
	Hi,
	Lo,
}

enum Register {
	Work(u32),
	Temp(u32),
}

struct HalfRegister {
	reg: Register,
	half: Half,
}

enum LirInstr {
	I32Add(HalfRegister, HalfRegister),
	I32Sub(HalfRegister, HalfRegister),
	I32Mul(HalfRegister, HalfRegister),
}