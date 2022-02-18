#![cfg(test)]

macro_rules! assert_next_token {
    
    // assert_next_token!(<lexer>, token { <match body> }, "failure message")
    ( $lexer:expr, token $token_body:tt $(, $msg:expr )? ) => {
        
        let token = $lexer.next_token().unwrap();
        
        println!("token: {:?}", token);
        assert!(matches!(token, TokenOut $token_body) $(, $msg )?)
    };
    
    // assert_next_token!(<lexer>, error { <match body> }, "failure message")
    ( $lexer:expr, error $error_body:tt $(, $msg:expr )? ) => {
        
        let error = $lexer.next_token().unwrap_err();
        
        println!("error: {:?}", error);
        assert!(matches!(error, LexerError $error_body)  $(, $msg )?)
    };
}

// assert_token_sequence!(<lexer>,
//      <list of items for assert_next_token!()>
// );
macro_rules! assert_token_sequence {
    ( $lexer:expr, $( $item:tt  $body:tt $( $msg:expr )? ),* $( , )? ) => {
        
        $( assert_next_token!($lexer, $item $body $(, $msg )? ); )*
        
    };
}


mod comments;
mod lexerrules;