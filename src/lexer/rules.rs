use crate::lexer::Token;

// Lexer Rules

pub enum LexerMatch {
    IncompleteMatch,
    CompleteMatch,
    NoMatch,
}

pub trait LexerRule {
    fn current_state(&self) -> LexerMatch;
    
    fn feed(&mut self, ch: char) -> LexerMatch;
    
    // like feed, but only modifies the LexerRule state if would match
    // return the match state if ch was passed to feed()
    fn try_match(&mut self, ch: char) -> LexerMatch;
    
    fn reset(&mut self);
    fn get_token(&self) -> Option<Token>;
}