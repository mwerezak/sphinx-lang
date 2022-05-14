
use crate::language::InternSymbol;
use crate::parser::primary::{Primary, AccessItem, Atom};
use crate::parser::operator::BinaryOp;
use crate::parser::expr::{Expr, ExprMeta};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MatchAction {
    AssignLocal,
    AssignNonLocal,
    DeclImmutable,
    DeclMutable,
}

// TODO rename to Pattern?
#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(InternSymbol),
    Attribute(Box<AttributePattern>), // receiver, attribute name
    Index(Box<IndexPattern>), // receiver, index expression
    Tuple(Box<[Pattern]>),
    Pack(Option<Box<Pattern>>),
    
    Modifier {
        modifier: MatchAction,
        pattern: Box<Pattern>,
    },
}

// Pattern Data

#[derive(Debug, Clone)]
pub struct AttributePattern {
    pub receiver: Primary,
    pub name: InternSymbol,
}

#[derive(Debug, Clone)]
pub struct IndexPattern {
    pub receiver: Primary,
    pub index: ExprMeta,
}

// Assignments

#[derive(Debug, Clone)]
pub struct Assignment {
    pub lhs: Pattern,
    pub action: MatchAction,
    pub op: Option<BinaryOp>, // e.g. for +=, -=, *=, ...
    pub rhs: Expr,
}

// Convert expressions into Patterns...

/*
    pattern ::= identifier | primary index-access | primary member-access ;

    pattern-expression ::= pattern | pattern-list | "(" pattern ")" ;   (* basically just lvalues, and tuples of lvalues *)
    pattern-list ::= pattern-expression ( "," pattern-expression )* ;
*/

pub struct IntoPatternError;

impl TryFrom<Atom> for Pattern {
    type Error = IntoPatternError;
    fn try_from(atom: Atom) -> Result<Self, Self::Error> {
        match atom {
            Atom::Identifier(name) => Ok(Pattern::Identifier(name)),
            
            Atom::Group { modifier, inner } => {
                let pattern = (*inner).try_into()?;

                if let Some(modifier) = modifier {
                    Ok(Self::Modifier {
                        modifier,
                        pattern: Box::new(pattern),
                    })
                } else {
                    Ok(pattern)
                }
            },

            _ => Err(IntoPatternError)
        }
    }
}

impl TryFrom<Primary> for Pattern {
    type Error = IntoPatternError;
    fn try_from(primary: Primary) -> Result<Self, Self::Error> {
        // remove the last item so that primary will eval to the reciever
        let (atom, mut path) = primary.take();
        let tail = path.pop();
        let receiver = Primary::new(atom, path);
        
        let pattern = match tail {
            Some(AccessItem::Attribute(name)) 
                => Pattern::Attribute(Box::new(AttributePattern { receiver, name })),
            Some(AccessItem::Index(index)) 
                => Pattern::Index(Box::new(IndexPattern { receiver, index })),
            _ => return Err(IntoPatternError),
        };
        
        Ok(pattern)
    }
}

impl TryFrom<Expr> for Pattern {
    type Error = IntoPatternError;
    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Atom(atom) => atom.try_into(),
            
            Expr::Primary(primary) => primary.try_into(),
            
            Expr::Unpack(Some(expr)) => {
                let inner = Pattern::try_from(*expr)?;
                Ok(Self::Pack(Some(Box::new(inner))))
            }
            Expr::Unpack(None) => Ok(Self::Pack(None)),
            
            Expr::Tuple(items) if !items.is_empty() => {
                let mut lvalue_items = Vec::new();
                for expr in items.into_vec().into_iter() {
                    let pattern = Pattern::try_from(expr.take_variant())?;
                    lvalue_items.push(pattern);
                }
                
                Ok(Self::Tuple(lvalue_items.into_boxed_slice()))
            },
            
            _ => Err(IntoPatternError),
        }
    }
}
