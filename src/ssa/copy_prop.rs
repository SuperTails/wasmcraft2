
// Notation from https://inst.eecs.berkeley.edu/~cs164/sp11/lectures/lecture37-2x2.pdf
enum CopyValue {
	/// No value has reached here yet
	Bottom,
	Definite(TypedSsaVar),
	Top,
}