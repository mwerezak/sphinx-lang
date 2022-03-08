
use crate::runtime::strings::InternSymbol;
use crate::parser::primary::{Primary, AccessItem, Atom};
use crate::parser::operator::BinaryOp;
use crate::parser::expr::ExprVariant;

#[derive(Debug, Clone)]
pub enum LValue {
    Identifier(InternSymbol),
    Primary(Box<Primary>), // type annotation
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

impl TryFrom<Primary> for LValue {
    type Error = ();
    fn try_from(primary: Primary) -> Result<Self, Self::Error> {
        if let Some(tail) = primary.iter_path().last() {
            if matches!(tail, AccessItem::Attribute(..) | AccessItem::Index(..)) {
                return Ok(LValue::Primary(Box::new(primary)));
            }
            return Err(());
        }
        
        match primary.take_atom() {
            Atom::Group(expr) => (*expr).try_into(),
            Atom::Identifier(name) => Ok(LValue::Identifier(name)),
            _ => Err(())
        }
    }
}

impl TryFrom<ExprVariant> for LValue {
    type Error = ();
    fn try_from(expr: ExprVariant) -> Result<Self, Self::Error> {
        match expr {
            ExprVariant::Primary(primary) => (*primary).try_into(),
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
