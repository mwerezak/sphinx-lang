
use crate::runtime::strings::InternSymbol;
use crate::parser::primary::{Primary, AccessItem, Atom};
use crate::runtime::types::operator::BinaryOp;
use crate::parser::expr::{ExprVariant, Expr};

#[derive(Debug, Clone)]
pub enum LValue {
    Identifier(InternSymbol),
    Attribute(Primary, InternSymbol), // receiver, attribute name
    Index(Primary, Expr), // receiver, index expression
    Tuple(Vec<LValue>),
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub lhs: LValue,
    pub op: Option<BinaryOp>, // e.g. for +=, -=, *=, ...
    pub rhs: ExprVariant,
}

#[derive(Debug, Clone)]
pub enum DeclType {
    Immutable,
    Mutable,
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub decl: DeclType,
    pub lhs: LValue,
    pub init: ExprVariant,
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
            Some(AccessItem::Attribute(name)) => LValue::Attribute(primary, name),
            Some(AccessItem::Index(index)) => LValue::Index(primary, index),
            _ => return Err(()),
        };
        
        Ok(lvalue)
    }
}

impl TryFrom<ExprVariant> for LValue {
    type Error = ();
    fn try_from(expr: ExprVariant) -> Result<Self, Self::Error> {
        match expr {
            ExprVariant::Atom(atom) => atom.try_into(),
            
            ExprVariant::Primary(primary) => primary.try_into(),
            
            ExprVariant::Tuple(expr_list) => {
                let mut lvalue_list = Vec::<LValue>::new();
                
                for expr in expr_list.into_iter() {
                    let lvalue = expr.take_variant().try_into()?;
                    lvalue_list.push(lvalue);
                }
                
                Ok(Self::Tuple(lvalue_list))
            },
            
            _ => Err(()),
        }
    }
}
