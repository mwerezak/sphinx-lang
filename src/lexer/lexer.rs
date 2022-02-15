// Token Types

pub enum Token<'s> {
    Delim(Delimiter),
    OpSym(OpSymbol),
    Keyword(Keyword),
    
    StringLiteral(&'s str),
    IntegerLiteral(u32),
    FloatLiteral(f32),
    
    EOF,
}

pub enum Delimiter {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenSquare,
    CloseSquare,
    Comma,
    Colon,
    Semicolon,
}

pub enum OpSymbol {
    Add, Sub, Mul, Div, Mod,
    BitAnd, BitOr, BitXor, LShift, RShift,
    LT, LE, GT, GE, EQ, NE,
    And, Or, Not,
    Assign, Access,
}

pub enum Keyword {
    Var, Begin, 
    If, Then, Elif, Else,
    For, While, Do,
    Fun, Class,
    Self_, Super, True, False, Nil,
    Echo,
    End,
}

// Token Data

// uses lifetime of the source text
pub struct TokenData<'s> {
    token: Token<'s>,
    lexeme: &'s str,
    lineno: u64,
}

// Lexer Rules

pub enum LexerMatch {
    NoMatch,
    IncompleteMatch,
    CompleteMatch,
}

pub trait LexerRule {
    fn match_state(&self) -> LexerMatch;
    fn feed(&mut self, ch: char) -> LexerMatch;
    fn reset(&mut self);
}

// Lexer Errors

pub struct LexerError {
    message: String,
    filename: String,
    index: usize,
    lineno: u64,
}

// Lexer Builder

pub struct LexerBuilder {
    rules: Vec<Box<dyn LexerRule>>,
}

impl LexerBuilder {
    pub fn new() -> Self {
        LexerBuilder {
            rules: Vec::new(),
        }
    }
    
    pub fn add_rule<R>(&mut self, rule: R) -> &mut Self
    where R: LexerRule + 'static {
        
        self.rules.push(Box::new(rule));
        return self;
    }
    
    pub fn build(self, filename: &str, source: &str) -> Lexer {
        Lexer { 
            filename: String::from(filename),
            source: String::from(source),
            current: 0,
            token_start: 0,
            lineno: 1,
            rules: self.rules,
        }
    }
}

// Lexer

pub struct Lexer {
    filename: String,
    source: String,
    
    current: usize,
    token_start: usize,
    lineno: u64,
    
    rules: Vec<Box<dyn LexerRule>>,
}

impl Lexer {
    // fn next_char(&self) -> Option<char> {
    
    pub fn next_token(&mut self) -> Result<TokenData<'_>, LexerError> {
        unimplemented!();
        
        // grab the next char, and feed it to all the rules
        // any rules that no longer match are discarded
        //
        // if exactly one rule left, stop iterating and just fill out that one
        // if nothing left, consider rules that were completed on the last iteration...
        //    if there are none, error (could not parse symbol)
        //    if there are more than one, error (ambiguous symbol)
        //    if there is exactly one, stop iterating and emit a token
        //
        // otherwise...
        //    any rules that match completely are moved to a separate Vec for the next iteration
        //    advance current to the next char
        
    }
    
    fn token_data<'s>(&'s self, token: Token<'s>) -> TokenData<'s> {
        let lexeme = &self.source[self.token_start..self.current];
        TokenData {
            token, lexeme, lineno: self.lineno,
        }
    }
}

