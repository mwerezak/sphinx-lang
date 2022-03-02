//! output/error reporting and formatting

use std::fmt;
use std::iter;
use std::fmt::Formatter;
use crate::utils;
use crate::parser::ParserError;
use crate::debug::symbol::ResolvedSymbol;


pub fn render_parser_error<'a>(error: &'a ParserError, symbol: &'a ResolvedSymbol) -> impl fmt::Display + 'a {
    utils::delegate_fmt(|fmt| fmt_parser_error(fmt, error, symbol))
}

pub fn fmt_parser_error(fmt: &mut Formatter<'_>, error: &ParserError, symbol: &ResolvedSymbol) -> fmt::Result {
    //TODO figure out a good way to present multiline errors
    
    // Write error message
    let message = utils::title_case_string(&error.to_string());
    write!(fmt, "{}.\n\n", message)?;
    
    // Write source line
    let margin = format!("{: >3}|    ", symbol.lineno());
    let source_line = render_symbol_single_line(symbol).to_string();
    
    let start_col = symbol.start_col();
    let end_col =
        if !symbol.is_multiline() {
            symbol.end_col()
        } else {
            source_line.len() - start_col
        };
    
    let mut marker = String::new();
    marker.extend(iter::repeat(' ').take(margin.len() + start_col));
    marker.extend(iter::repeat('^').take(usize::min(end_col - start_col, 1)));
    
    writeln!(fmt, "{}{}", margin, source_line)?;
    writeln!(fmt, "{}\n", marker)?;
    
    Ok(())
}


pub fn render_symbol_lines<'a>(symbol: &'a ResolvedSymbol) -> impl fmt::Display + 'a {
    utils::delegate_fmt(|fmt| fmt_symbol_lines(fmt, symbol))
}

// format a symbol, including the surrounding text on the same lines, and trim trailing whitespace
pub fn fmt_symbol_lines(fmt: &mut Formatter<'_>, symbol: &ResolvedSymbol) -> fmt::Result {
    for line in symbol.iter_whole_lines() {
        fmt.write_str(line.trim_end())?;
        fmt.write_str("\n")?;
    }
    Ok(())
}

// if the symbol isn't multiline, this produces the same output as render_symbol_lines()
// if it is multiline, this renders the first line of the symbol followed by "..."
pub fn render_symbol_single_line<'a>(symbol: &'a ResolvedSymbol) -> impl fmt::Display + 'a {
    utils::delegate_fmt(|fmt| fmt_symbol_single_line(fmt, symbol))
}

fn fmt_symbol_single_line(fmt: &mut fmt::Formatter<'_>, symbol: &ResolvedSymbol) -> fmt::Result { 
    if let Some(first_line) = symbol.iter_lines().nth(0) {
        if symbol.is_multiline() {
            write!(fmt, "{}...", first_line.trim_end())?;
        } else {
            fmt.write_str(first_line.trim_end())?;
        }
    }
    Ok(())
}
