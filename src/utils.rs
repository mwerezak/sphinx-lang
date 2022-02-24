use std::fmt;

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