use std::fmt;
use std::io;
use std::io::BufRead;
use std::collections::VecDeque;

// useful for writing string literals, to ensure that a gigantic string doesnt swamp the output
pub fn trim_str(target: &str, maxlen: usize) -> TrimStr<'_> {
    TrimStr {
        target,
        maxlen,
    }
}

// captures the arguments to trim_str(), to implement trimming in fmt() without requiring an extra string buffer
pub struct TrimStr<'s> {
    target: &'s str,
    maxlen: usize,
}

// note, this gives us a blanket implementation of ToString as well, so if you do want a new string buffer you can get that too
impl<'s> fmt::Display for TrimStr<'s> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.target.len() > self.maxlen {
            write!(fmt, "{}...", &self.target[..(self.maxlen-3)])
        } else {
            fmt.write_str(self.target)
        }
    }
}

pub fn title_case_string(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
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

pub fn delegate_fmt<F>(fmt_func: F) -> impl fmt::Display where F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result {
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