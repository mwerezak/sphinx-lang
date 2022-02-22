use crate::language;
use crate::lexer::Token;
use crate::lexer::rules::{MatchResult, LexerRule, CharClass, TokenError};
use crate::lexer::rules::strmatcher::StrMatcher;

// supports escape sequences that consist of a single-character tag (e.g. \t) and an optional fixed-length argument (e.g. \u0FFE, \xFE)
pub trait EscapeSequence {
    fn tag(&self) -> char;
    fn arglen(&self) -> u8;
    
    // produce a string that will replace the escape sequence in the source literal
    fn transform(&self, arg: &str) -> Result<String, StringEscapeErrorKind>;
}

// simple escape sequences like \n -> 0x0A or \t -> 0x09
pub struct CharMapEscape {
    tag: char,
    output: &'static str,
}

impl CharMapEscape {
    fn new(tag: char, output: &'static str) -> Self {
        CharMapEscape { tag, output }
    }
}

impl EscapeSequence for CharMapEscape {
    fn tag(&self) -> char { self.tag }
    fn arglen(&self) -> u8 { 0 }
    fn transform(&self, _arg: &str) -> Result<String, StringEscapeErrorKind> { 
        Ok(self.output.to_string())
    }
}

// \x00 \xFF
pub struct HexByteEscape {}

const HEX_ESCAPE_TAG: char = 'x';
impl EscapeSequence for HexByteEscape {
    fn tag(&self) -> char { HEX_ESCAPE_TAG }
    fn arglen(&self) -> u8 { 2 }
    fn transform(&self, arg: &str) -> Result<String, StringEscapeErrorKind> { 
        debug_assert!(arg.len() == 2);
        
        let value = u8::from_str_radix(arg, 16)
            .map_err(|_err| StringEscapeErrorKind::InvalidEscapeArg)?;
        
        match char::from_u32(value as u32) {
            Some(ch) => Ok(ch.to_string()),
            None => Err(StringEscapeErrorKind::InvalidEscapeArg),
        }
    }
}

// TODO unicode escapes


const ESCAPE_CHAR: char = '\\';
const SINGLE_QUOTE: char = '\'';
const DOUBLE_QUOTE: char = '"';
const RAW_PREFIX: char = 'r';

pub struct StringLiteralRule {
    raw_buf: String,
    escaped_buf: String,
    quote: Option<char>,
    closed: bool,
    
    raw: bool,
    escape: Option<&'static dyn EscapeSequence>, // active escape sequence, argbuf
    argbuf: Option<String>, // option so we can take it into the escape sequence without borrow issues
    error: Option<StringEscapeErrorKind>, // hold the first error to occur when processing an escape
    
    escapes: Vec<&'static dyn EscapeSequence>,
}

impl StringLiteralRule {
    fn new<E>(escapes: E) -> Self where E: IntoIterator<Item=&'static dyn EscapeSequence> {
        StringLiteralRule {
            raw_buf: String::new(),
            escaped_buf: String::new(),
            quote: None,
            closed: false,
            raw: false,
            
            escape: None,
            argbuf: None,
            error: None,
            
            escapes: escapes.into_iter().collect(),
        }
    }
    
    fn find_escape(&self, tag: char) -> Option<&'static dyn EscapeSequence> {
        self.escapes.iter()
        .find(|escape| tag == escape.tag())
        .map(|seq| *seq)
    }
    
    fn process_escape(&mut self, escape: &dyn EscapeSequence, arg: &str) {
        match escape.transform(arg) {
            Ok(out) => self.escaped_buf.push_str(out.as_str()),
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
        
        self.argbuf = None;
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
            if let Some(escape) = self.escape {
                let argbuf = self.argbuf.as_mut().unwrap();
                argbuf.push(next);
                if argbuf.len() == (escape.arglen() as usize) {
                    let arg = self.argbuf.take().unwrap();
                    self.process_escape(escape, arg.as_str());
                    self.escape = None;
                }
                
                self.raw_buf.push(next);
                return MatchResult::IncompleteMatch;
            }
            
            // escape sequence start
            if let Some(ESCAPE_CHAR) = prev {
                match self.find_escape(next) {
                    None => self.error = Some(StringEscapeErrorKind::InvalidEscapeTag),
                    Some(escape) => {
                        if escape.arglen() == 0 {
                            // if the escape doesnt take any arg we can process it now
                            self.process_escape(escape, "");
                        } else {
                            // otherwise set up for the next chars
                            self.escape = Some(escape);
                        }
                    }
                };
                
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
        self.raw_buf.push(next);
        self.escaped_buf.push(next);
        return MatchResult::IncompleteMatch;
    }
    
    fn get_token(&self) -> Result<Token, TokenError> {
        debug_assert!(self.current_state().is_complete_match());
        
        if let Some(errorkind) = self.error {
            Err(Box::new(StringEscapeError::new(errorkind, self.raw_buf.clone())))
        } else if self.raw {
            Ok(Token::StringLiteral(self.raw_buf.clone()))
        } else {
            Ok(Token::StringLiteral(self.escaped_buf.clone()))
        }
        
    }

}


#[derive(Debug, Clone, Copy)]
pub enum StringEscapeErrorKind {
    InvalidEscapeTag,
    InvalidEscapeArg,
}

#[derive(Debug)]
pub struct StringEscapeError {
    kind: StringEscapeErrorKind,
    raw: String,
}

impl StringEscapeError {
    fn new(kind: StringEscapeErrorKind, raw: String) -> Self {
        StringEscapeError { kind, raw }
    }
}

impl std::error::Error for StringEscapeError { }

impl std::fmt::Display for StringEscapeError { 
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unimplemented!()
    }
}