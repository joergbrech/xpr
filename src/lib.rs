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
//! let y = ---Xpr::Terminal(42);
//! 
//! // A transform that counts the number of nestings
//! let mut count = 0;
//! y.transform(|x| {
//!     match x {
//!         Xpr::Neg(_) => {
//!             count+=1;
//!             None
//!         }
//!         _ => Some(0) // must return something. It doesn't matter what here.
//!     }
//! });
//! assert_eq!(count, 3);
//! 
//! // a transform that replaces the terminal in y with E::Terminal(5)
//! let expr = y.transform(|x| {
//!    match *x {
//!        Xpr::Terminal(_) => Some(Xpr::Terminal(5)),
//!        _ => None
//!    } 
//! }).unwrap();
//! 
//! // a transform that evaluates the expression
//! let result = expr.transform(|x| {
//!     match *x {
//!         Xpr::Terminal(v) => Some(v),
//!         _ => None
//!     }
//! }).unwrap();
//! assert_eq!(result, -5);
//!
//! // In fact, Xpr::eval() is just syntactic sugar around this tranform
//! assert_eq!(expr.eval().unwrap(), result);
//! ```

use std::any::Any;
use std::ops::{Neg,Mul};

/// The `Expression` just has an associated output type
pub trait Expression : Any {
    type Output;
}

// An enum parametrized by its output type O, as well as the outputs types of 
// the left and right operand expressions
pub enum Xpr<O,L,R> {
    Terminal(O),
    Neg(Box<dyn Expression<Output = L>>),
    Mul(Box<dyn Expression<Output = L>>, Box<dyn Expression<Output = R>>)
}

impl<O,L,R> Expression for Xpr<O,L,R>
where O: 'static,
      L: 'static,
      R: 'static
{
    type Output = O;
}

impl<O> Xpr<O,O,O>
{
    /// A factory function to create a terminal without specifying the two 
    /// arbitrary operand types of `Xpr`
    pub fn term(o: O) -> Self {
        Xpr::Terminal(o)
    }
}

impl<O,L,R> Xpr<O,L,R> 
{

    /// evaluates the expression
    pub fn eval(&self) -> Result<O,&str> {
        self.transform(|x| {
            match x {
                Xpr::Terminal(v) => Some(*v),
                _ => None
            }
        }).ok_or("Error evaluating expression")
    }

    fn transform<F>(&self, mut f: F) -> Option<O>
    where F: FnMut(&Xpr<O,L,R>) -> Option<O>,
    {
        // CASE 1/3: Match! return f(self)
        if let Some(v) = f(self) { return Some(v); };

        match self {
            // CASE 2/3: We have reached a leaf-expression, no match!
            Xpr::Terminal(_) => None, 
            // CASE 3/3: Recurse and apply operation to result
            Xpr::Neg(x) => {
                // To Do: downcast to Xpr using any
                let y : Box<&dyn Any> = Box::new(x);
                let z = y.downcast_ref::<Xpr<O,L,R>>().unwrap();
                z.transform(f).map(|y| -y)
            }
        }
    }

}

impl<I,X,Y,O> Neg for Xpr<I,X,Y> 
where X: 'static,
      Y: 'static,
      I: Neg<Output = O> + 'static
{
    type Output = Xpr<O,I,Y>;
    fn neg(self) -> Self::Output {
        Xpr::Neg(Box::new(self))
    }
}

impl<OL,X,Y,OR,R,O> Mul<R> for Xpr<OL,X,Y>
where R : Expression<Output = OR> + 'static,
      OL: Mul<OR, Output = O> + 'static,
      X: 'static,
      Y: 'static
{
    type Output = Xpr::<O,OL,OR>;
    fn mul(self, other: R) -> Self::Output {
        Xpr::Mul(Box::new(self), Box::new(other))
    }
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
