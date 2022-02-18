#![cfg(test)]

macro_rules! assert_next_token {
    
    // assert_next_token!(<lexer>, token { <match body> } [if <guard>] , "failure message")
    ( $lexer:expr, token $token_body:tt $( if $guard:expr )? $(, $msg:expr )? ) => {
        
        let token = $lexer.next_token().unwrap();
        
        println!("token: {:?}", token);
        assert!(matches!(token, TokenMeta $token_body $( if $guard)? ) $(, $msg )?)
    };
    
    // assert_next_token!(<lexer>, error { <match body> }, "failure message")
    ( $lexer:expr, error $error_body:tt $( if $guard:expr )? $(, $msg:expr )? ) => {
        
        let error = $lexer.next_token().unwrap_err();
        
        println!("error: {:?}", error);
        assert!(matches!(error, LexerError $error_body $( if $guard)? ) $(, $msg )?)
    };
}

// assert_token_sequence!(<lexer>,
//      <list of items for assert_next_token!()>
// );
macro_rules! assert_token_sequence {
    ( 
        $lexer:expr, 
        $( 
            $item:tt $( if $guard:expr )? =>
            $body:tt 
            $( $msg:expr )? 
            
        ),* $( , )? 
    ) => {
        
        // println!("{}", stringify!(
             $( assert_next_token!($lexer, $item $body $( if $guard )? $(, $msg )? ); )*
        // ));
    };
}

// Test Modules
mod comments;
mod lexerrules;
mod literals;