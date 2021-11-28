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


use std::ops::Neg;

/// The enum Xpr contains all operations supported by this crate. Xpr::Terminal represents 
/// a leaf expression.
pub enum Xpr<U> {
    Terminal(U),
    Neg(Box<Xpr<U>>)
}

impl<U> Xpr<U> 
where U: Neg<Output = U>  + Copy
{

    /// evaluates the expression
    pub fn eval(&self) -> Result<U,&str> {
        self.transform(|x| {
            match x {
                Xpr::Terminal(v) => Some(*v),
                _ => None
            }
        }).ok_or("Error evaluating expression")
    }

}

impl<U> Neg for Xpr<U> {
    type Output = Xpr<U>;
    fn neg(self) -> Self::Output {
        Xpr::Neg(Box::new(self))
    }
}

/// Transform is trait, that lets us transform
/// subexpressions in an expression
pub trait Transform<U = Self> {

    fn transform<R,F>(&self, _f: F) -> Option<R>
    where F: FnMut(&Xpr<U>) -> Option<R>,
          R: Neg<Output = R>
    {
        None
    }
}

impl<U> Transform<U> for Xpr<U> 
{
    fn transform<R,F>(&self, mut f: F) -> Option<R>
    where F: FnMut(&Xpr<U>) -> Option<R>,
          R: Neg<Output = R>
    {
        // CASE 1/3: Match! return f(self)
        if let Some(v) = f(self) { return Some(v); };

        match self {
            Xpr::Terminal(_) => None, // CASE 2/3: We have reached a leaf-expression, no match!
            Xpr::Neg(x) => {      // CASE 3/3: Recurse and apply operation to result
                x.transform(f).map(|y| -y)
            }
        }
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
