//! output/error reporting and formatting

use core::iter;
use core::fmt::{self, Formatter};
use std::error::Error;

use crate::utils;
use crate::debug::SourceError;
use crate::debug::symbol::{ResolvedSymbol, DebugSymbolResolver};

pub fn print_source_errors<E>(resolver: &impl DebugSymbolResolver, errors: &[E]) where E: SourceError {
    let symbols = errors.iter().filter_map(|err| err.debug_symbol());
    
    let resolved_table = resolver.resolve_symbols(symbols).unwrap();
    
    // resolve errors and collect into vec
    let mut render_errors = errors.iter().filter_map(
        |error| match error.debug_symbol() {
            None => Some(RenderError(error, None)),
            Some(symbol) => {
                let resolved = resolved_table.lookup(symbol).unwrap();
                match resolved {
                    Ok(resolved) => Some(RenderError(error, Some(resolved))),
                    Err(resolve_error) => {
                        println!("{}", error);
                        println!("Could not resolve symbol: {}", resolve_error);
                        None
                    }
                }
            },
        })
        .collect::<Vec<RenderError<E>>>();
    
    // sort errors by line number
    render_errors.sort_by_key(|render| render.1.map_or_else(
        || (1, 0), |resolved| (0, resolved.lineno())
    ));
    
    for render in render_errors.iter() {
        println!("{}", render);
    }
}


pub struct RenderError<'e, 's, E>(pub &'e E, pub Option<&'s ResolvedSymbol>) where E: Error;

impl<E> fmt::Display for RenderError<'_, '_, E> where E: Error {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let RenderError(error, source_lines) = self;
        
        if let Some(source_lines) = source_lines {
            write!(fmt, "{}.\n\n{}", error, source_lines)
        } else {
            write!(fmt, "{}.", error)
        }
    }
}

impl fmt::Display for ResolvedSymbol {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_source_lines(fmt, self)
    }
}

fn fmt_source_lines(fmt: &mut Formatter<'_>, symbol: &ResolvedSymbol) -> fmt::Result {
    let mut start_idx = 0;
    for (num, raw_line) in symbol.iter_whole_lines().enumerate() {
        let end_index = start_idx + raw_line.len(); // of current line
        
        let margin = format!("{: >3}", num + symbol.lineno());
        let source_line = raw_line.trim_end();
        
        let start_col =
            if symbol.start() < start_idx { 0 } // started on a previous line
            else { symbol.start() };
        
        let end_col =
            if symbol.end() > end_index { source_line.len() } // ends on a next line
            else { symbol.end() - start_idx };
        
        let mut marker = String::new();
        marker.extend(iter::repeat(' ').take(margin.len()));
        marker.push_str("     ");
        
        marker.extend(iter::repeat(' ').take(start_col));
        marker.extend(iter::repeat('^').take(usize::max(end_col - start_col, 1))); // for single index symbols
        
        writeln!(fmt, "{}|    {}", margin, source_line)?;
        writeln!(fmt, "{}", marker)?;
        
        start_idx += raw_line.len();
    }
    
    Ok(())
}

fn fmt_source_line_single(fmt: &mut Formatter<'_>, symbol: &ResolvedSymbol) -> fmt::Result {
    let margin = format!("{: >3}|    ", symbol.lineno());
    let source_line = render_symbol_single_line(symbol).to_string();
    
    let start_col = symbol.start_col();
    let end_col =
        if !symbol.is_multiline() {
            symbol.end_col()
        } else {
            source_line.len()
        };
    
    let mut marker = String::new();
    marker.extend(iter::repeat(' ').take(margin.len() + start_col));
    marker.extend(iter::repeat('^').take(usize::max(end_col - start_col, 1)));
    
    writeln!(fmt, "{}{}", margin, source_line)?;
    writeln!(fmt, "{}\n", marker)?;
    
    Ok(())
}


fn render_symbol_lines(symbol: &ResolvedSymbol) -> impl fmt::Display + '_ {
    utils::make_display(|fmt| fmt_symbol_lines(fmt, symbol))
}

// format a symbol, including the surrounding text on the same lines, and trim trailing whitespace
fn fmt_symbol_lines(fmt: &mut Formatter<'_>, symbol: &ResolvedSymbol) -> fmt::Result {
    for line in symbol.iter_whole_lines() {
        fmt.write_str(line.trim_end())?;
        fmt.write_str("\n")?;
    }
    Ok(())
}

// if the symbol isn't multiline, this produces the same output as render_symbol_lines()
// if it is multiline, this renders the first line of the symbol followed by "..."
fn render_symbol_single_line(symbol: &ResolvedSymbol) -> impl fmt::Display + '_ {
    utils::make_display(|fmt| fmt_symbol_single_line(fmt, symbol))
}

fn fmt_symbol_single_line(fmt: &mut fmt::Formatter<'_>, symbol: &ResolvedSymbol) -> fmt::Result { 
    if let Some(first_line) = symbol.iter_whole_lines().next() {
        if symbol.is_multiline() {
            write!(fmt, "{}...", first_line.trim_end())?;
        } else {
            fmt.write_str(first_line.trim_end())?;
        }
    }
    Ok(())
}
