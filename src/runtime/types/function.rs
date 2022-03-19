


struct Function {
    signature: Signature,
    module: ModuleID,
    chunk_id: ChunkID,
}


struct Signature {
    required: Box<[Parameter]>,
    default: Box<[Parameter]>,
    variadic: Option<Parameter>,
}

impl Signature {
    pub fn min_arity(&self) -> usize { self.required.len() }
    
    pub fn max_arity(&self) -> Option<usize> {
        if self.is_variadic() { None }
        else { Some(self.required.len() + self.default.len()) }
    }
}

struct Parameter {
    name: StringSymbol,
    access: Access,
    default: Option<ChunkID>,
}