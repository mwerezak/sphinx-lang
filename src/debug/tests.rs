#![cfg(test)]

use crate::source::{ModuleSource, SourceText};
use super::symbol::{DebugSymbol, ResolvedSymbol, DebugSymbolResolver};

#[test]
fn debug_symbols_test_symbol_resolution() {
    let text = r#"example
        code this example
        another example"#;
    
    let module = ModuleSource::String(text.to_string());
    
    let symbols = vec![
        DebugSymbol::from((0, 7)),
        DebugSymbol::from((0, 20)),
        DebugSymbol::from((13, 24)),
    ];
    
    let symbol_table = module.resolve_symbols(symbols.iter()).unwrap();
    
    for (k, v) in symbol_table.iter() {
        match v {
            Ok(symbol) => println!("{:?} => {}", k, symbol),
            _ => println!("{:?} => {:?}", k, v),
        }
    }
}
