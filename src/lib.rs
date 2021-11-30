//! This crate allows you to use expression templates with your custom type.
//! It is inspired by [boost::yap](https://www.boost.org/doc/libs/1_74_0/doc/html/yap.html).
//! Expressions can be evaluated or manipulated using transforms, which match subexpressions, 
//! and transform matched expressions, do any kind of calculations in between and return anything
//! that implements the operations of the used expression.
//! 
//! # Example usage
//! ```rust
//! use xpr::*; 
//!
//! // An expression 
//! let y = -Xpr::new(2)*Xpr::new(21);
//! ```

use std::any::Any;
use std::marker::PhantomData;
use std::ops::{Neg, Mul};

/// This represents an expression characterized only by its
/// output type. It must be generated from an `Xpr` instance
/// and allows downcasting
struct Expr<'a ,O> {
    expr: &'a dyn Any ,
    phantom: PhantomData<O>
}
impl<'a, O> Expr<'a, O>
where O: 'static
{

    /// given two operand types L and R, this function downcasts
    /// an instance `Expr<O>` to a reference of its contained `Xpr<O,L,R>`
    fn downcast<L,R>(&self) -> Option<&Xpr<O,L,R>>
    where L: 'static,
          R: 'static
    {
        self.expr.downcast_ref::<Xpr<O,L,R>>()
    }

    fn as_expression(&self) -> Expression<'a> {
        Expression { expr: self.expr }
    }
}

/// This represents any expression where both the output and 
/// operand types have been erased. It must be generated from 
/// an `Expr<O>` instance and allows downcasting
pub struct Expression<'a> {
    expr: &'a dyn Any
}
impl<'a> Expression<'a>
{

    /// given an output type, this function downcasts
    /// an instance `Expression` to its contained `Expr<O>`
    fn downcast<O>(&self) -> Option<&Expr<O>>
    where O: 'static
    {
        self.expr.downcast_ref::<Expr<O>>()
    }

    /// given an output type O and two operand types
    /// L and R, this function downcasts an instance
    /// `Expression` to its contained `Xpr<O,L,R>`
    fn to_xpr<O,L,R>(&self) -> Option<&Xpr<O,L,R>>
    where O: 'static,
          L: 'static,
          R: 'static
    {
        match self.downcast::<O>() {
            Some(x) => x.downcast::<L,R>(),
            _ => None
        }
    }
}

/// The `Terminal` struct represents a leaf expression in an expression tree
pub struct Terminal<T>(T);

/// The `UnaryOp` represents any unary operation that takes an input parameter
/// of type I and returns a O
pub struct UnaryOp<'a,O,I>{
    input: Box::<Expr<'a, I>>,
    phantom: PhantomData<O>
}
impl<'a,O,I> UnaryOp<'a,O,I> {
    pub fn new(input : Box<Expr<'a, I>>) -> Self
    {
        UnaryOp{ input, phantom: PhantomData }
    }
}

/// This enum represents all supported unary operations that take an input
/// of type I and return a O
pub enum UnaryOps<'a,O,I> {
    Neg(UnaryOp<'a,O,I>)
}

/// The `BinaryOp` represents any binary operation that takes input parameters
/// of type Expr<L> and Expr<R> and returns a O
pub struct BinaryOp<'a,O,L,R> {
    left: Box::<Expr<'a,L>>,
    right: Box::<Expr<'a,R>>,
    phantom: PhantomData<O>
}
impl<'a,O,L,R> BinaryOp<'a,O,L,R> {
    pub fn new(left : Box<Expr<'a,L>>, right: Box<Expr<'a,R>>) -> Self {
        BinaryOp { left, right, phantom: PhantomData }
    }
}

/// This enum represents all supported binary operations that take operands of
/// type L and R and return an O
pub enum BinaryOps<'a,O,L,R> {
    Mul(BinaryOp<'a,O,L,R>)
}

/// The enum `Xpr` represents all supported expressions. It is parametrized by an 
/// output type O, as well as the outputs types of all operands
pub enum Xpr<'a,O,L,R> {
    /// represents a leaf expression of type O
    Terminal(Terminal<O>),
    /// represents any unary operation that takes an L parameter and returns an O
    UnaryOp(UnaryOps<'a,O,L>),
    /// represents any binary operation that takes operands L and R and returns an O
    BinaryOp(BinaryOps<'a,O,L,R>)
}

impl<'a,O,L,R> Xpr<'a,O,L,R>
where O: Mul<Output = O> + Neg<Output = O> + Copy + 'static,
      L: 'static,
      R: 'static
{
    /// evaluates the expression
    pub fn eval(&self) -> Result<O,&str> {
        self.transform(|x| {
            // To Do: Need to downcast Box<dyn Expression> to Xpr<O,L,R>
            match x.to_xpr::<O,L,R>() {
                Some(&Xpr::Terminal(Terminal(v))) => Some(v),
                _ => None
            }
        }).ok_or("Error evaluating expression")
    }

    pub fn transform<F,T>(&self, mut f: F) -> Option<T>
    where F: FnMut(Box<Expression>) -> Option<T>,
          T: Mul<Output = T> + Neg<Output = T>
    {
        // CASE 1/3: Match! return f(self)
        if let Some(v) = f(Box::new(self.as_expression())) { return Some(v); };

        match self {
            // CASE 2/3: We have reached a leaf-expression, no match!
            Xpr::Terminal(_) => None, 
            // CASE 3/3: Recurse and apply operation to result
            Xpr::UnaryOp(op) => match op {
                UnaryOps::Neg(x) => { x.transform(f).map(|y| -y) }
            },
            Xpr::BinaryOp(op) => match op {
                BinaryOps::Mul(x) => { x.transform(f).map(|(a,b)| a*b) }
            }
        }
    }

    fn as_expr(&self) -> Expr<O>
    {
        Expr { expr: self, phantom: PhantomData }
    }

    fn as_expression(&self) -> Expression {
        Expression { expr: self }
    }
}

impl<'a,O> Xpr<'a,O,O,O>
{
    /// A factory function to create a terminal without specifying the two 
    /// arbitrary operand types of `Xpr`
    pub fn new(o: O) -> Self {
        Xpr::Terminal(Terminal(o))
    }
}

impl<'a,O,I> UnaryOp<'a,O,I>
where I: 'static
{
    fn transform<F,FO>(&self, mut f: F) -> Option<FO>
    where F: FnMut(Box<Expression>) -> Option<FO>
    {
        // To Do: Need to upcast dyn ExpressionWithOutput<Output = I> to Expression
        f(Box::new(self.input.as_expression()))
    }
}

impl<'a,O,L,R> BinaryOp<'a,O,L,R>
where L: 'static,
      R: 'static
{
    fn transform<F,FO>(&self, mut f: F) -> Option<(FO,FO)>
    where F: FnMut(Box<Expression>) -> Option<FO>
    {
        // To Do: Need to upcast to Expression
        match (f(Box::new(self.left.as_expression())), f(Box::new(self.right.as_expression()))) 
        {
            (Some(l), Some(r)) => Some((l,r)),
            _ => None
        }
    }
}

// impl<I,X,Y,O> Neg for Xpr<I,X,Y> 
// where X: 'static,
//       Y: 'static,
//       I: Neg<Output = O> + 'static
// {
//     type Output = Xpr<O,I,Y>;

//     /// neg consumes an Xpr instances e and returns -e
//     fn neg(self) -> Self::Output {
//         Xpr::UnaryOp(UnaryOps::Neg(UnaryOp::new(Box::new(self))))
//     }
// }

// impl<OL,X,Y,OR,R,O> Mul<R> for Xpr<OL,X,Y>
// where R : Expr<OR> + 'static,
//       OL: Mul<OR, Output = O> + 'static,
//       X: 'static,
//       Y: 'static
// {
//     type Output = Xpr::<O,OL,OR>;
//     /// mul consumes two Xpr instances l and r and returns l*r
//     fn mul(self, other: R) -> Self::Output {
//         Xpr::BinaryOp(BinaryOps::Mul(BinaryOp::new(Box::new(self), Box::new(other))))
//     }
// }


#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_terminal() {
        let x = Xpr::new(42);

        // let ret = x.transform(&mut |e| {


        //     // transform borrows f which is FnMut as &mut
        //     // f owns Box of borrowed x as e which is Box<&dyn Expression>
        //     // reference to `dyn Expression` is not 'static

        //     // first check if it is a terminal
        //     match expression_cast::<i32>(*e) {
        //         Some(Xpr::Term(Terminal(_))) => Some(0),
        //         Some(Xpr::Ops(Ops::Neg(z))) => Some(1),
        //         _ => None
        //     }
        // }
        // );
    }
}
