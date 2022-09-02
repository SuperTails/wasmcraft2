use wasmparser::{Data, Element, Export, FuncType, Global, Import, MemoryType, Operator, Parser, Payload, TableType, BlockType, ExternalKind, GlobalType, ConstExpr, ValType, Type, TypeRef};

use crate::ssa::interp::TypedValue;

#[derive(Debug, Default)]
pub struct DataList<'a> {
    pub data: Vec<Data<'a>>,
}

impl<'a> DataList<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_data(&mut self, d: Data<'a>) {
        self.data.push(d);
    }
}

#[derive(Debug, Default)]
pub struct TableList {
    pub tables: Vec<TableType>,
}

impl TableList {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_table(&mut self, ty: TableType) {
        self.tables.push(ty);
    }
}

#[derive(Default)]
pub struct ElementList<'a> {
    pub elements: Vec<Element<'a>>,
}

impl<'a> ElementList<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_element(&mut self, elem: Element<'a>) {
        self.elements.push(elem);
    }
}

#[derive(Debug, Default)]
pub struct GlobalList<'a> {
    pub globals: Vec<Global<'a>>,
}

impl<'a> GlobalList<'a> {
    pub fn new() -> Self {
        GlobalList::default()
    }

    pub fn add_global(&mut self, global: Global<'a>) {
        self.globals.push(global);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FuncImport<'a> {
    pub module: &'a str,
    pub field: &'a str,
    pub ty: u32,
}

#[derive(Debug, Clone, Copy)]
struct GlobalImport<'a> {
    pub module: &'a str,
    pub field: &'a str,
    pub content_type: ValType,
    pub mutable: bool,
}

#[derive(Debug, Default)]
pub struct ImportList<'a> {
    func_imports: Vec<FuncImport<'a>>,
    global_imports: Vec<GlobalImport<'a>>,
}

impl<'a> ImportList<'a> {
    pub fn new() -> Self {
        ImportList::default()
    }

    pub fn add_import(&mut self, i: Import<'a>) {
        match i.ty {
            TypeRef::Func(ty) => {
                self.func_imports.push(FuncImport { module: i.module, field: i.name, ty })
            }
            TypeRef::Global(GlobalType { content_type, mutable }) => {
                self.global_imports.push(GlobalImport { module: i.module, field: i.name, content_type, mutable })
            }
            _ => todo!("{:?}", i),
        }
    }
}

#[derive(Debug, Default)]
pub struct ExportList<'a> {
    pub exports: Vec<Export<'a>>,
}

impl<'a> ExportList<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_export(&mut self, export: Export<'a>) {
        self.exports.push(export);
    }

    pub fn find_func(&self, name: &str) -> Option<usize> {
        for export in self.exports.iter() {
            if export.name == name {
                assert!(matches!(export.kind, ExternalKind::Func));
                return Some(export.index as usize);
            }
        }

        None
    }
}

#[derive(Debug, Default)]
pub struct MemoryList {
    pub memory: Vec<MemoryType>,
}

impl MemoryList {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_memory(&mut self, memory: MemoryType) {
        self.memory.push(memory);
    }
}

#[derive(Debug)]
pub struct FunctionBody<'a> {
    pub operators: Vec<Operator<'a>>,
    pub locals: Vec<(u32, ValType)>,
}

#[derive(Debug, Default)]
pub struct TypeList {
    pub types: Vec<Type>,
}

impl TypeList {
    pub fn new() -> Self {
        TypeList::default()
    }

    pub fn add_type(&mut self, ty: Type) {
        self.types.push(ty);
    }

    pub fn func_type(&self, idx: u32) -> &FuncType {
        let Type::Func(f) = &self.types[idx as usize];

        f
    }

    pub fn start_types(&self, ty: BlockType) -> Box<[ValType]> {
        match ty {
            BlockType::Empty => Box::new([]),
            BlockType::Type(_) => Vec::new().into_boxed_slice(),
            BlockType::FuncType(i) => {
                let ty = &self.func_type(i);
                ty.params.clone()
            }
        }
    }

    pub fn end_types(&self, ty: BlockType) -> Box<[ValType]> {
        match ty {
            BlockType::Empty => Box::new([]),
            BlockType::Type(t) => vec![t].into_boxed_slice(),
            BlockType::FuncType(i) => {
                let ty = &self.func_type(i);
                ty.returns.clone()
            }
        }
    }
}


#[derive(Debug, Clone, Default)]
pub struct FunctionList {
    pub functions: Vec<u32>,
}

impl FunctionList {
    pub fn new() -> Self {
        FunctionList::default()
    }

    pub fn add_function(&mut self, function: u32) {
        self.functions.push(function)
    }
}


pub struct WasmFile<'a> {
    pub types: TypeList,
    pub globals: GlobalList<'a>,
    pub memory: MemoryList,
    pub exports: ExportList<'a>,
    pub imports: ImportList<'a>,
    pub data: DataList<'a>,
    pub tables: TableList,
    pub elements: ElementList<'a>,
    /// Includes imported functions
    pub functions: FunctionList,
    pub bodies: Vec<FunctionBody<'a>>,
}

impl<'a> WasmFile<'a> {
    pub fn func_body(&self, func_idx: usize) -> &FunctionBody<'a> {
        let import_count = self.imports.func_imports.len();
        if !self.func_is_defined(func_idx) {
            panic!("tried to get the body of an imported function ({})", func_idx)
        } else {
            &self.bodies[func_idx - import_count]
        }
    }

    pub fn func_is_defined(&self, func_idx: usize) -> bool {
        let import_count = self.imports.func_imports.len();
        assert!(func_idx <= import_count + self.bodies.len());
        func_idx >= import_count
    }

    pub fn defined_funcs(&self) -> impl Iterator<Item=usize> {
        let import_count = self.imports.func_imports.len();
        import_count..import_count + self.bodies.len()
    }

    pub fn func_type_idx(&self, func_idx: usize) -> u32 {
        self.functions.functions[func_idx]
    }

    pub fn func_type(&self, func_idx: usize) -> &FuncType {
        let type_idx = self.func_type_idx(func_idx);
        self.types.func_type(type_idx)
    }

    pub fn func_locals(&self, func_idx: usize) -> Vec<ValType> {
        let mut result = Vec::new();

        let func_ty = self.func_type(func_idx);
        result.extend(func_ty.params.iter().copied());

        let body = self.func_body(func_idx);
        for &(count, local_ty) in body.locals.iter() {
            for _ in 0..count {
                result.push(local_ty);
            }
        }

        result
    }

    pub fn func_import(&self, func_idx: usize) -> FuncImport {
        self.imports.func_imports[func_idx]
    }

    pub fn find_func(&self, name: &str) -> Option<usize> {
        self.exports.find_func(name)
    }

    pub fn global(&self, index: u32) -> Global {
        if !self.imports.global_imports.is_empty() {
            todo!()
        }

        self.globals.globals[index as usize]
    }

    pub fn globals(&self) -> &[Global] {
        if !self.imports.global_imports.is_empty() {
            todo!()
        }

        &self.globals.globals
    }
}

impl<'a> From<&'a [u8]> for WasmFile<'a> {
    fn from(file: &'a [u8]) -> WasmFile<'a> {
        let mut types = TypeList::new();
        let mut globals = GlobalList::new();
        let mut memory = MemoryList::new();
        let mut exports = ExportList::new();
        let mut imports = ImportList::new();
        let mut tables = TableList::new();
        let mut data = DataList::new();
        let mut elements = ElementList::new();

        let mut func_reader = None;

        let mut codes = Vec::new();

        for payload in Parser::new(0).parse_all(file) {
            let payload = payload.unwrap();
            match payload {
                Payload::Version { .. } => {}
                Payload::ImportSection(i) => {
                    for import in i {
                        let import = import.unwrap();
                        imports.add_import(import);
                    }
                }
                Payload::DataSection(d) => {
                    for d in d {
                        let d = d.unwrap();
                        data.add_data(d);
                    }
                }
                Payload::ExportSection(e) => {
                    for export in e {
                        let export = export.unwrap();
                        exports.add_export(export);
                    }
                }
                Payload::TypeSection(t) => {
                    for ty in t {
                        let ty = ty.unwrap();

                        types.add_type(ty);
                    }
                }
                Payload::GlobalSection(g) => {
                    for global in g {
                        let global = global.unwrap();

                        globals.add_global(global);
                    }
                }
                Payload::MemorySection(m) => {
                    for mem in m {
                        let mem = mem.unwrap();

                        memory.add_memory(mem);
                    }
                }
                Payload::FunctionSection(f) => {
                    assert!(func_reader.is_none());
                    func_reader = Some(f);
                }
                Payload::CodeSectionStart { .. } => {}
                Payload::CodeSectionEntry(e) => {
                    let operators = e.get_operators_reader().unwrap()
                        .into_iter()
                        .map(|o| o.unwrap())
                        .collect::<Vec<_>>();
                    let locals = e.get_locals_reader().unwrap()
                        .into_iter()
                        .map(|l| l.unwrap())
                        .collect::<Vec<_>>();

                    codes.push(FunctionBody { operators, locals });
                }
                Payload::TableSection(t) => {
                    for table in t {
                        let table = table.unwrap();
                        tables.add_table(table);
                    }
                }
                Payload::ElementSection(e) => {
                    for elem in e {
                        let elem = elem.unwrap();
                        elements.add_element(elem);
                    }
                }
                Payload::End(_) => {}
                _other => {
                    println!("TODO: Unknown section {:?}", _other);
                }
            }
        }

        let mut functions = FunctionList::new(); 

        for f in imports.func_imports.iter() {
            functions.add_function(f.ty);
        }

        if let Some(func_reader) = func_reader {
            for func in func_reader {
                let func = func.unwrap();

                functions.add_function(func);
            }
        }

        println!("{:?}", exports);

        WasmFile { functions, memory, globals, exports, imports, types, tables, data, elements, bodies: codes }
    }
}

pub fn eval_const_expr(init_expr: &ConstExpr) -> Vec<TypedValue> {
    let ops = init_expr.get_operators_reader().into_iter().map(|o| o.unwrap()).collect::<Vec<_>>();

    match &ops[..] {
        &[Operator::I32Const { value }, Operator::End] => {
            vec![TypedValue::I32(value)]
        }
        ops => todo!("{:?}", ops)
    }
}

pub fn eval_const_expr_single(init_expr: &ConstExpr) -> TypedValue {
    let result = eval_const_expr(init_expr);
    assert_eq!(result.len(), 1);
    result.into_iter().next().unwrap()
}

