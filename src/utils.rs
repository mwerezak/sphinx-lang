use std::fmt::{Display, Write, Formatter, self};
use std::io;
use std::io::BufRead;
use std::collections::VecDeque;

// useful for writing string literals, to ensure that a gigantic string doesnt swamp the output
pub fn trim_str(target: &impl AsRef<str>, maxlen: usize) -> impl Display + '_ {
    TrimStr {
        target: target.as_ref(),
        maxlen,
    }
}

// captures the arguments to trim_str(), to implement trimming in fmt() without requiring an extra string buffer
struct TrimStr<'s> {
    target: &'s str,
    maxlen: usize,
}

// note, this gives us a blanket implementation of ToString as well, so if you do want a new string buffer you can get that too
impl Display for TrimStr<'_> {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        if self.target.len() > self.maxlen {
            write!(fmt, "{}...", &self.target[..(self.maxlen-3)])
        } else {
            fmt.write_str(self.target)
        }
    }
}

pub fn title_case(s: &impl AsRef<str>) -> impl Display + '_ {
    TitleCase { target: s.as_ref() }
}

struct TitleCase<'s> {
    target: &'s str,
}

impl Display for TitleCase<'_> {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        let mut prev = None;
        for next in self.target.chars() {
            if next.is_alphabetic() && prev.map_or(true, char::is_whitespace) {
                for c in next.to_uppercase() {
                    fmt.write_char(c)?;
                }
            } else {
                fmt.write_char(next)?;
            }
            
            prev.replace(next);
        }
        Ok(())
    }
}

pub fn fmt_join<'a>(sep: impl Display + 'a, items: &'a [impl Display]) -> impl Display + 'a {
    DisplayJoin { sep, items }
}

struct DisplayJoin<'a, S, D> where S: Display, D: Display {
    sep: S,
    items: &'a [D],
}

impl<S, D> Display for DisplayJoin<'_, S, D> where S: Display, D: Display {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        for item in self.items.iter().take(self.items.len() - 1) {
            item.fmt(fmt)?;
            self.sep.fmt(fmt)?;
        }
        if let Some(item) = self.items.last() {
            item.fmt(fmt)?;
        }
        Ok(())
    }
}


// This struct is born out of a desire to read a file into unicode characters 
// without pulling the entire file into a buffer

#[derive(Debug)]
pub struct ReadChars<R> where R: BufRead {
    read: R,
    linebuf: String,
    charbuf: VecDeque<char>,
}

impl<R> ReadChars<R> where R: BufRead {
    pub fn new(read: R) -> Self {
        ReadChars {
            read,
            linebuf: String::new(),
            charbuf: VecDeque::new(),
        }
    }
}

impl<R> Iterator for ReadChars<R> where R: BufRead {
    type Item = io::Result<char>;
    
    fn next(&mut self) -> Option<io::Result<char>> {
        let next = self.charbuf.pop_front().map(Ok);
        if next.is_some() {
            return next;
        }
        
        // refill linebuf with the next line
        
        self.linebuf.clear();
        
        let mut safety = 0;
        while self.linebuf.is_empty() && safety < 0xFFFF {
            
            match self.read.read_line(&mut self.linebuf) {
                Err(error) => return Some(Err(error)),
                Ok(0) => return None, // EOF
                _ => { safety += 1 },
            }
            
        }
        
        self.charbuf.extend(self.linebuf.chars());
        self.charbuf.pop_front().map(Ok)
    }
}



// Formatter that uses a closure
// Useful to avoid a lot of boilerplate when there are multiple ways to Display a struct

pub fn make_display<F>(fmt_func: F) -> impl fmt::Display where F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result {
    FnFormatter { fmt_func }
}

struct FnFormatter<F> where F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result {
    fmt_func: F,
}

impl<F> fmt::Display for FnFormatter<F> where F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self.fmt_func)(fmt)
    }
}


// Formats an error that may have a message and/or a source error
pub fn format_error(fmt: &mut fmt::Formatter<'_>, title: &str, message: Option<&str>, source: Option<&dyn std::error::Error>) -> fmt::Result {
    // empty messages are formatted the same as no message
    let message =
        if let Some("") = message { None }
        else { message };
    
    match (message, source) {
        (None, None) => fmt.write_str(title),
        (None, Some(error)) => write!(fmt, "{}: {}", title, error),
        (Some(message), None) => write!(fmt, "{}: {}", title, message),
        (Some(message), Some(error)) => write!(fmt, "{}: {}: {}", title, message, error),
    }
}