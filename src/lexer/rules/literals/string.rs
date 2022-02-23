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
    pub fn new(tag: char, output: &'static str) -> Self {
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

impl HexByteEscape {
    pub fn new() -> Self { HexByteEscape { } }
}

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
    escape: Option<usize>, // active escape sequence, argbuf
    argbuf: Option<String>, // option so we can take it into the escape sequence without borrow issues
    error: Option<StringEscapeErrorKind>, // hold the first error to occur when processing an escape
    
    escapes: Vec<Box<dyn EscapeSequence>>,
}

impl StringLiteralRule {
    pub fn new(escapes: Vec<Box<dyn EscapeSequence>>) -> Self {
        StringLiteralRule {
            raw_buf: String::new(),
            escaped_buf: String::new(),
            quote: None,
            closed: false,
            raw: false,
            
            escape: None,
            argbuf: None,
            error: None,
            
            escapes,
        }
    }
    
    fn find_eid_for_tag(&self, tag: char) -> Option<usize> {
        self.escapes.iter().position(|esc| tag == esc.tag())
    }
    
    fn lookup_escape(&self, eid: usize) -> &Box<dyn EscapeSequence> {
        // let (_, slice) = self.escapes.split_at(eid);
        // let (item, _) = slice.split_first().unwrap();
        
        // item
        
        &self.escapes[eid]
    }
    
    // can't take a reference to the escape or argument by parameter since it would cause borrow issues
    // so we take an index to get the escape
    // and we just have to get the argument directly from self
    fn process_escape(&mut self, eid: usize) {
        let escape = &self.escapes[eid];
        let arg = {
            match self.argbuf.take() {
                Some(s) => s,
                None => String::new(),
            }
        };
        
        match escape.transform(arg.as_str()) {
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
            if let Some(eid) = self.escape {
                let argbuf = self.argbuf.as_ref().unwrap();
                let escape = self.lookup_escape(eid);
                
                if argbuf.len() < (escape.arglen() as usize) {
                    let argbuf = self.argbuf.as_mut().unwrap();
                    argbuf.push(next);
                    
                    self.raw_buf.push(next);
                    return MatchResult::IncompleteMatch;
                }
                
                // if we get here, argbuf already has enough characters without adding the next one
                // process the escape and then do not return so that the next char is processed as normal
                self.process_escape(eid);
                self.escape = None;
                self.argbuf = None;
                
            // check for escape sequence start
            } else if let Some(ESCAPE_CHAR) = prev {
                
                if let Some(eid) = self.find_eid_for_tag(next) {
                    self.escape = Some(eid);
                    self.argbuf = Some(String::new());
                } else {
                    self.error = Some(StringEscapeErrorKind::InvalidEscapeTag);
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
    fn fmt(&self, _fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unimplemented!()
    }
}