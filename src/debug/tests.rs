#![cfg(test)]

use crate::source::{ModuleSource};
use super::symbol::{DebugSymbol, DebugSymbolResolver};

#[test]
fn debug_symbols_test_symbol_resolution() {
    let text = r#"example
        code this example
        another example"#;
    
    let module = ModuleSource::String(text.to_string());
    
    let symbols = vec![
        DebugSymbol::try_from((0, 7)).unwrap(),
        DebugSymbol::try_from((0, 20)).unwrap(),
        DebugSymbol::try_from((13, 24)).unwrap(),
    ];
    
    let symbol_table = module.resolve_symbols(symbols.iter()).unwrap();
    
    for (k, v) in symbol_table.iter() {
        match v {
            Ok(symbol) => println!("{:?} => {}", k, symbol),
            _ => println!("{:?} => {:?}", k, v),
        }
    }
}
