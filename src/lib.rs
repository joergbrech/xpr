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

use std::marker::PhantomData;
use std::ops::{Neg, Mul};

/// This trait has an associated output type which is useful to differentiate
/// operands in operations
pub trait ExpressionWithOutput {
    type Output;
}


/// This is a supertrait of `Expression, that erases the Output type. This is 
/// useful for transformations that can be applied to any expression.
pub trait Expression{}
impl<T,O> Expression for T where T: ExpressionWithOutput<Output = O>{}

/// The `Terminal` struct represents a leaf expression in an expression tree
pub struct Terminal<T>(T);

impl<T> ExpressionWithOutput for Terminal<T>
{
    type Output = T;
}

/// The `UnaryOp` represents any unary operation that takes an input parameter
/// of type I and returns a O
pub struct UnaryOp<O,I>{
    input: Box::<dyn ExpressionWithOutput<Output = I>>,
    phantom: PhantomData<O>
}
impl<O,I> UnaryOp<O,I> {
    pub fn new(input : Box<dyn ExpressionWithOutput<Output = I>>) -> Self
    {
        UnaryOp{ input, phantom: PhantomData }
    }
}

impl<O,I> ExpressionWithOutput for UnaryOp<O,I> {
    type Output = O;
}

/// This enum represents all supported unary operations that take an input
/// of type I and return a O
pub enum UnaryOps<O,I> {
    Neg(UnaryOp<O,I>)
}

impl<O,I> ExpressionWithOutput for UnaryOps<O,I> {
    type Output = O;
}

/// The `BinaryOp` represents any binary operation that takes input parameters
/// of type L and R and returns a O
pub struct BinaryOp<O,L,R> {
    left: Box::<dyn ExpressionWithOutput<Output = L>>,
    right: Box::<dyn ExpressionWithOutput<Output = R>>,
    phantom: PhantomData<O>
}
impl<O,L,R> BinaryOp<O,L,R> {
    pub fn new(left : Box<dyn ExpressionWithOutput<Output = L>>, right: Box<dyn ExpressionWithOutput<Output = R>>) -> Self {
        BinaryOp { left, right, phantom: PhantomData }
    }
}

impl<O,L,R> ExpressionWithOutput for BinaryOps<O,L,R> {
    type Output = O;
}

/// This enum represents all supported binary operations that take operands of
/// type L and R and return an O
pub enum BinaryOps<O,L,R> {
    Mul(BinaryOp<O,L,R>)
}

/// The enum `Xpr` represents all supported expressions. It is parametrized by an 
/// output type O, as well as the outputs types of all operands
pub enum Xpr<O,L,R> {
    /// represents a leaf expression of type O
    Terminal(Terminal<O>),
    /// represents any unary operation that takes an L parameter and returns an O
    UnaryOp(UnaryOps<O,L>),
    /// represents any binary operation that takes operands L and R and returns an O
    BinaryOp(BinaryOps<O,L,R>)
}

impl<O,L,R> ExpressionWithOutput for Xpr<O,L,R>
{
    type Output = O;
}

impl<O,L,R> Xpr<O,L,R>
where O: Mul<Output = O> + Neg<Output = O>
{
    /// evaluates the expression
    pub fn eval(&self) -> Result<O,&str> {
        self.transform(|x| {
            // To Do: Need to downcast to Xpr<O,L,R>
            match x {
                Xpr::Terminal(v) => Some(v.0),
                _ => None
            }
        }).ok_or("Error evaluating expression")
    }

    fn transform<F,T>(&self, f: F) -> Option<T>
    where F: FnMut(Box<&dyn Expression>) -> Option<T>,
          T: Mul<Output = T> + Neg<Output = T>
    {
        // CASE 1/3: Match! return f(self)
        if let Some(v) = f(Box::new(self)) { return Some(v); };

        match self {
            // CASE 2/3: We have reached a leaf-expression, no match!
            Xpr::Terminal(_) => None, 
            // CASE 3/3: Recurse and apply operation to result
            Xpr::UnaryOp(op) => match op {
                UnaryOps::Neg(x) => { x.transform(f).map(|y| -y) }
                _ => None
            },
            Xpr::BinaryOp(op) => match op {
                BinaryOps::Mul(x) => { x.transform(f).map(|(a,b)| a*b) }
            }
            _ => None
        }
    }
}

impl<O> Xpr<O,O,O>
{
    /// A factory function to create a terminal without specifying the two 
    /// arbitrary operand types of `Xpr`
    pub fn new(o: O) -> Self {
        Xpr::Terminal(Terminal(o))
    }
}

impl<O,I> UnaryOp<O,I>
{
    fn transform<F,FO>(&self, f: F) -> Option<FO>
    where F: FnMut(Box<&dyn Expression>) -> Option<FO>
    {
        // To Do: Need to upcast to Expression
        f(self.input)
    }
}

impl<O,L,R> BinaryOp<O,L,R>
{
    fn transform<F,FO>(&self, f: F) -> Option<(FO,FO)>
    where F: FnMut(Box<&dyn Expression>) -> Option<FO>
    {
        // To Do: Need to upcast to Expression
        match (f(self.left), f(self.right)) {
            (Some(l), Some(r)) => Some((l,r)),
            _ => None
        }
    }
}

impl<I,X,Y,O> Neg for Xpr<I,X,Y> 
where X: 'static,
      Y: 'static,
      I: Neg<Output = O> + 'static
{
    type Output = Xpr<O,I,Y>;

    /// neg consumes an Xpr instances e and returns -e
    fn neg(self) -> Self::Output {
        Xpr::UnaryOp(UnaryOps::Neg(UnaryOp::new(Box::new(self))))
    }
}

impl<OL,X,Y,OR,R,O> Mul<R> for Xpr<OL,X,Y>
where R : ExpressionWithOutput<Output = OR> + 'static,
      OL: Mul<OR, Output = O> + 'static,
      X: 'static,
      Y: 'static
{
    type Output = Xpr::<O,OL,OR>;
    /// mul consumes two Xpr instances l and r and returns l*r
    fn mul(self, other: R) -> Self::Output {
        Xpr::BinaryOp(BinaryOps::Mul(BinaryOp::new(Box::new(self), Box::new(other))))
    }
}


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
