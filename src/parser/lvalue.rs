
use crate::language::InternSymbol;
use crate::parser::primary::{Primary, AccessItem, Atom};
use crate::parser::operator::BinaryOp;
use crate::parser::expr::{Expr, ExprMeta};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssignType {
    AssignLocal,
    AssignNonLocal,
    DeclImmutable,
    DeclMutable,
}

#[derive(Debug, Clone)]
pub enum LValue {
    Identifier(InternSymbol),
    Attribute(Box<AttributeTarget>), // receiver, attribute name
    Index(Box<IndexTarget>), // receiver, index expression
    Tuple(Box<[LValue]>),
    PackItem(Option<Box<LValue>>),
    
    Modifier {
        modifier: AssignType,
        lvalue: Box<LValue>,
    },
}

// LValue Data

#[derive(Debug, Clone)]
pub struct AttributeTarget {
    pub receiver: Primary,
    pub name: InternSymbol,
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
    pub assign: AssignType,
    pub op: Option<BinaryOp>, // e.g. for +=, -=, *=, ...
    pub rhs: Expr,
}

// Convert expressions to LValues...

/*
    lvalue ::= identifier | primary index-access | primary member-access ;

    lvalue-expression ::= lvalue | lvalue-list | "(" lvalue ")" ;   (* basically just lvalues, and tuples of lvalues *)
    lvalue-list ::= lvalue-expression ( "," lvalue-expression )* ;
*/

pub struct IntoLValueError;

impl TryFrom<Atom> for LValue {
    type Error = IntoLValueError;
    fn try_from(atom: Atom) -> Result<Self, Self::Error> {
        match atom {
            Atom::Identifier(name) => Ok(LValue::Identifier(name)),
            
            Atom::Group { modifier, inner } => {
                let lvalue = (*inner).try_into()?;

                if let Some(modifier) = modifier {
                    Ok(Self::Modifier {
                        modifier,
                        lvalue: Box::new(lvalue),
                    })
                } else {
                    Ok(lvalue)
                }
            },

            _ => Err(IntoLValueError)
        }
    }
}

impl TryFrom<Primary> for LValue {
    type Error = IntoLValueError;
    fn try_from(primary: Primary) -> Result<Self, Self::Error> {
        // remove the last item so that primary will eval to the reciever
        let (atom, mut path) = primary.take();
        let tail = path.pop();
        let receiver = Primary::new(atom, path);
        
        let lvalue = match tail {
            Some(AccessItem::Attribute(name)) 
                => LValue::Attribute(Box::new(AttributeTarget { receiver, name })),
            Some(AccessItem::Index(index)) 
                => LValue::Index(Box::new(IndexTarget { receiver, index })),
            _ => return Err(IntoLValueError),
        };
        
        Ok(lvalue)
    }
}

impl TryFrom<Expr> for LValue {
    type Error = IntoLValueError;
    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Atom(atom) => atom.try_into(),
            
            Expr::Primary(primary) => primary.try_into(),
            
            Expr::Ellipsis(Some(expr)) => {
                let inner = LValue::try_from(*expr)?;
                Ok(Self::PackItem(Some(Box::new(inner))))
            }
            Expr::Ellipsis(None) => Ok(Self::PackItem(None)),
            
            Expr::Tuple(items) if !items.is_empty() => {
                let mut lvalue_items = Vec::new();
                for expr in items.into_vec().into_iter() {
                    let lvalue = LValue::try_from(expr.take_variant())?;
                    lvalue_items.push(lvalue);
                }
                
                Ok(Self::Tuple(lvalue_items.into_boxed_slice()))
            },
            
            _ => Err(IntoLValueError),
        }
    }
}
