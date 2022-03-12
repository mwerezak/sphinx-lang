
use crate::runtime::strings::StringSymbol;
use crate::parser::primary::{Primary, AccessItem, Atom};
use crate::runtime::types::operator::BinaryOp;
use crate::parser::expr::{Expr, ExprMeta};


// LValues

#[derive(Debug, Clone)]
pub enum LValue {
    Identifier(StringSymbol),
    Attribute(Box<AttributeTarget>), // receiver, attribute name
    Index(Box<IndexTarget>), // receiver, index expression
    Tuple(Box<[LValue]>),
}

#[derive(Debug, Clone)]
pub struct AttributeTarget {
    pub receiver: Primary,
    pub name: StringSymbol,
}

#[derive(Debug, Clone)]
pub struct IndexTarget {
    pub receiver: Primary,
    pub index: ExprMeta,
}

// Assignments

#[derive(Debug, Clone)]
pub struct Assignment {
    pub lhs: LValue,
    pub op: Option<BinaryOp>, // e.g. for +=, -=, *=, ...
    pub rhs: Expr,
    pub global: bool,
}

// Declarations

#[derive(Debug, Clone)]
pub enum DeclType {
    Immutable,
    Mutable,
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub decl: DeclType,
    pub lhs: LValue,
    pub init: Expr,
}

// Convert expressions to LValues...

/*
    lvalue ::= identifier | primary index-access | primary member-access ;

    lvalue-expression ::= lvalue | lvalue-list | "(" lvalue ")" ;   (* basically just lvalues, and tuples of lvalues *)
    lvalue-list ::= lvalue-expression ( "," lvalue-expression )* ;
*/

impl TryFrom<Atom> for LValue {
    type Error = ();
    fn try_from(atom: Atom) -> Result<Self, Self::Error> {
        match atom {
            Atom::Identifier(name) => Ok(LValue::Identifier(name)),
            Atom::Group(expr) => (*expr).try_into(),
            _ => Err(())
        }
    }
}

impl TryFrom<Primary> for LValue {
    type Error = ();
    fn try_from(mut primary: Primary) -> Result<Self, Self::Error> {
        // remove the last item so that primary will eval to the reciever
        let tail = primary.path_mut().pop();
        
        let lvalue = match tail {
            Some(AccessItem::Attribute(name)) 
                => LValue::Attribute(Box::new(AttributeTarget { receiver: primary, name })),
            Some(AccessItem::Index(index)) 
                => LValue::Index(Box::new(IndexTarget { receiver: primary, index })),
            _ => return Err(()),
        };
        
        Ok(lvalue)
    }
}

impl TryFrom<Expr> for LValue {
    type Error = ();
    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Atom(atom) => atom.try_into(),
            
            Expr::Primary(primary) => primary.try_into(),
            
            Expr::Tuple(expr_list) => {
                let mut lvalue_list = Vec::<LValue>::new();
                
                for expr in expr_list.into_vec().into_iter() {
                    let lvalue = expr.take_variant().try_into()?;
                    lvalue_list.push(lvalue);
                }
                
                Ok(Self::Tuple(lvalue_list.into_boxed_slice()))
            },
            
            _ => Err(()),
        }
    }
}
