use std::fmt;
use std::io;
use std::io::BufRead;
use std::path::Path;
use std::collections::VecDeque;

// useful for writing string literals, to ensure that a gigantic string doesnt swamp the output
pub fn trim_str<'s>(s: &'s str, maxlen: usize) -> TrimStr<'s> {
    TrimStr {
        target: s.as_ref(),
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


// This struct is born out of a desire to read a file into unicode characters 
// without pulling the entire file into a buffer

struct ReadChars<R> where R: BufRead {
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
        if !self.linebuf.is_empty() {
            return self.charbuf.pop_front().map(|c| Ok(c));
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
        self.charbuf.pop_front().map(|c| Ok(c))
    }
}