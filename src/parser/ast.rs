use std::fmt;


pub type AstNodeObj = Box<dyn AstNode>;

pub trait AstNode: fmt::Debug + fmt::Display { }

