use crate::lexer::Token;
use crate::lexer::rules::{MatchResult, LexerRule, TokenError};

// supports escape sequences that consist of a single-character tag (e.g. \t) and an optional fixed-length argument (e.g. \u0FFE, \xFE)
pub trait EscapeSequence: Send + Sync {
    fn tag(&self) -> char;
    fn arglen(&self) -> u8;
    
    // produce a string that will replace the escape sequence in the source literal
    fn transform(&self, arg: &str) -> Result<String, StringEscapeError>;
}

// simple escape sequences like \n -> 0x0A or \t -> 0x09
pub struct CharMapEscape {
    tag: char,
    output: &'static str,
}

impl CharMapEscape {
    pub fn new(tag: char, output: &'static str) -> Self {
        CharMapEscape { tag, output }
    }
}

impl EscapeSequence for CharMapEscape {
    fn tag(&self) -> char { self.tag }
    fn arglen(&self) -> u8 { 0 }
    fn transform(&self, _arg: &str) -> Result<String, StringEscapeError> { 
        Ok(self.output.to_string())
    }
}

// \x00 \xFF
pub struct HexByteEscape {}

impl Default for HexByteEscape {
    fn default() -> Self { Self::new() }
}

impl HexByteEscape {
    pub fn new() -> Self { HexByteEscape { } }
}

const HEX_ESCAPE_TAG: char = 'x';
impl EscapeSequence for HexByteEscape {
    fn tag(&self) -> char { HEX_ESCAPE_TAG }
    fn arglen(&self) -> u8 { 2 }
    fn transform(&self, arg: &str) -> Result<String, StringEscapeError> { 
        debug_assert!(arg.len() == 2);
        
        let create_error = || StringEscapeError::new(StringEscapeErrorKind::InvalidEscapeArg, self.tag(), Some(arg.to_string()));
        
        let value = u8::from_str_radix(arg, 16)
            .map_err(|_err| create_error())?;
        
        match char::from_u32(value.into()) {
            Some(ch) => Ok(ch.to_string()),
            None => Err(create_error()),
        }
    }
}

// TODO unicode escapes


const ESCAPE_CHAR: char = '\\';
const SINGLE_QUOTE: char = '\'';
const DOUBLE_QUOTE: char = '"';
const RAW_PREFIX: char = 'r';

#[derive(Clone)]
struct ActiveEscape {
    escape: &'static dyn EscapeSequence,
    argbuf: String,
}

impl core::ops::Deref for ActiveEscape {
    type Target = dyn EscapeSequence;
    fn deref(&self) -> &'static Self::Target { self.escape }
}

#[derive(Clone)]
pub struct StringLiteralRule {
    raw_buf: String,
    escaped_buf: String,
    quote: Option<char>,
    closed: bool,
    
    raw: bool,
    escape: Option<ActiveEscape>, // the currently active escape sequence, if any
    error: Option<StringEscapeError>, // hold the first error to occur when processing an escape
    
    escapes: Vec<&'static dyn EscapeSequence>,
}

impl StringLiteralRule {
    pub fn new(escapes: impl Iterator<Item=&'static dyn EscapeSequence>) -> Self {
        StringLiteralRule {
            raw_buf: String::new(),
            escaped_buf: String::new(),
            quote: None,
            closed: false,
            raw: false,
            
            escape: None,
            error: None,
            
            escapes: escapes.collect(),
        }
    }
    
    fn lookup_escape_for_tag(&self, tag: char) -> Option<&'static dyn EscapeSequence> {
        self.escapes.iter()
            .find(|escape| tag == escape.tag())
            .copied()
    }
    
    // can't take a reference to the escape or argument by parameter since it would cause borrow issues
    // so we take an index to get the escape
    // and we just have to get the argument directly from self
    fn process_escape(&mut self, escape: &'static dyn EscapeSequence, arg: &str) {
        match escape.transform(arg) {
            Ok(output) => self.escaped_buf.push_str(output.as_str()),
            Err(err) => self.error = Some(err),
        };
    }
}

impl LexerRule for StringLiteralRule {
    
    fn reset(&mut self) {
        self.raw_buf.clear();
        self.escaped_buf.clear();
        self.quote = None;
        self.closed = false;
        self.raw = false;
        
        self.escape = None;
        self.error = None;
    }
    
    fn current_state(&self) -> MatchResult {
        match self.quote {
            None if self.closed => MatchResult::NoMatch,  // did not find initial quote
            None => MatchResult::IncompleteMatch,  // initial state
            
            Some(..) if self.closed && self.escape.is_none() => MatchResult::CompleteMatch,
            Some(..) => MatchResult::IncompleteMatch,
        }
    }
    
    fn try_match(&mut self, prev: Option<char>, next: char) -> MatchResult {
        if self.closed {
            return MatchResult::NoMatch;  // dont accept any further input
        }
        
        // if we haven't read the first quote yet
        if self.quote.is_none() {
            return match next {
                
                RAW_PREFIX => MatchResult::IncompleteMatch,
                
                SINGLE_QUOTE | DOUBLE_QUOTE => {
                    self.quote = Some(next);
                    if let Some(RAW_PREFIX) = prev {
                        self.raw = true;
                    }
                    
                    MatchResult::IncompleteMatch
                },
                
                _ => MatchResult::NoMatch,
                
            };
        }
        
        // note: if there was an error, skip all escape handling and keep reading as if a raw string
        if !self.raw && self.error.is_none() {
            
            // if we are already in an escape sequence
            if let Some(ref mut active) = self.escape {
                if active.argbuf.len() < active.arglen().into() {
                    active.argbuf.push(next);
                    
                    self.raw_buf.push(next);
                    return MatchResult::IncompleteMatch;
                }
                
                // if we get here, argbuf already has enough characters without adding the next one
                // process the escape and then do not return so that the next char is processed as normal
                
                let active = self.escape.take().unwrap(); // take out of self.escape as we are done with it
                self.process_escape(active.escape, active.argbuf.as_str());
                
            // check for escape sequence start
            } else if let Some(ESCAPE_CHAR) = prev {
                
                if let Some(escape) = self.lookup_escape_for_tag(next) {
                    self.escape = Some(ActiveEscape { escape, argbuf: String::new() });
                } else {
                    self.error = Some(StringEscapeError::new(
                        StringEscapeErrorKind::InvalidEscapeTag, next, None
                    ));
                }
                
                self.raw_buf.push(next);
                return MatchResult::IncompleteMatch;
            }
            
        }
        
        // check for terminating quote
        if next == self.quote.unwrap() {
            self.closed = true;
            return MatchResult::CompleteMatch;
        }
        
        // inside the string
        if next != ESCAPE_CHAR {
            self.escaped_buf.push(next);
        }
        self.raw_buf.push(next);
        
        MatchResult::IncompleteMatch
    }
    
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        
        if let Some(ref error) = self.error {
            Err(Box::new(error.clone().with_raw(self.raw_buf.clone())))
        } else if self.raw {
            Ok(Token::StringLiteral(self.raw_buf.clone()))
        } else {
            Ok(Token::StringLiteral(self.escaped_buf.clone()))
        }
        
    }

}


#[derive(Debug, Clone)]
pub enum StringEscapeErrorKind {
    InvalidEscapeTag,
    InvalidEscapeArg,
}

#[derive(Debug, Clone)]
pub struct StringEscapeError {
    kind: StringEscapeErrorKind,
    tag: char,
    arg: Option<String>,
    raw: Option<String>,
}

impl StringEscapeError {
    fn new(kind: StringEscapeErrorKind, tag: char, arg: Option<String>) -> Self {
        StringEscapeError { kind, tag, arg, raw: None }
    }
    
    fn with_raw(mut self, raw: String) -> Self {
        self.raw = Some(raw); self
    }
}

impl std::error::Error for StringEscapeError { }

impl core::fmt::Display for StringEscapeError { 
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let render_escape = format!("{}{}{}", 
            ESCAPE_CHAR, self.tag, self.arg.as_ref().map_or("", |arg| arg.as_str())
        );
        
        match self.kind {
            StringEscapeErrorKind::InvalidEscapeTag => write!(fmt, "unrecognized escape sequence '{}'", render_escape),
            StringEscapeErrorKind::InvalidEscapeArg => write!(fmt, "invalid escape sequence '{}'", render_escape),
        }
    }
}