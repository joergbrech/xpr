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
//! let y = -Xpr::new(2)*Xpr::new(-21);
//! assert_eq!(y.eval(), Ok(42));
//! ```

fn cast_optional_any<T: 'static>(x: Option<Box<dyn Any>>) -> Option<T>
{
    match x {
        Some(x) => match x.downcast_ref::<T>() {
            Some(x) => return Some(*x),
            None => None
        },
        _ => None
    }
}

use std::any::Any;

trait Eval {
    type Output: 'static;

    fn transform(&self, f: &mut dyn FnMut(AnyXpr) -> Option<Box<dyn Any>>) -> Option<Box<dyn Any>>;

    fn eval(&self) -> Result<Self::Output,&str> {

        let mut evaluator = |e: AnyXpr| -> Option<Box<dyn Any>> {
            match e.as_xpr::<Self::Output>() {
                Some(&Xpr::Terminal(x)) => Some(Box::new(x)),
                _ => None
            }
        };

        cast_optional_any(self.transform(&mut evaluator)).ok_or("Error evaluation expression")
    }
}

//***********************************************************************//

enum Xpr<T: Sized> {
    Terminal(T),
    Mul(Box<dyn Eval<Output = T>>)
}
impl<T> Xpr<T> 
where T: 'static
{
    fn new(value: T) -> Self 
    {
        Xpr::<T>::Terminal(value)
    }

    fn as_anyxpr(&self) -> AnyXpr {
        AnyXpr { expr: self }
    }

    fn transform_as<R: 'static>(&self, f: &mut dyn FnMut(AnyXpr) -> Option<Box<dyn Any>>) -> Option<R>
    {
        cast_optional_any(self.transform(f))
    }
}

impl<L,R> std::ops::Mul<Xpr<R>> for Xpr<L>
where L: 'static + std::ops::Mul<R>,
      R: 'static
{
    type Output = Xpr<<L as std::ops::Mul<R>>::Output>;
    fn mul(self, other : Xpr<R>) -> Self::Output
    {
        Xpr::Mul( Box::new(Mul{ left: Box::new(self), right: Box::new(other) } ))
    }
}

impl<T: 'static> Eval for Xpr<T>
{
    type Output = T;

    fn transform(&self, f: &mut dyn FnMut(AnyXpr) -> Option<Box<dyn Any>>) -> Option<Box<dyn Any>>
    {
        if let Some(v) = f(self.as_anyxpr()) { return Some(v); }
        
        match self {
            Xpr::Terminal(_) => None,
            Xpr::Mul(x) => x.transform(f)
        }
    }
}

//***********************************************************************//

struct Mul<L,R>
{
    left: Box<dyn Eval<Output=L>>,
    right: Box<dyn Eval<Output=R>>
}
impl<L,R> Eval for Mul<L,R>
where L: 'static + std::ops::Mul<R>,
      R: 'static,
      <L as std::ops::Mul<R>>::Output: 'static
{
    type Output = <L as std::ops::Mul<R>>::Output;

    fn transform(&self, f: &mut dyn FnMut(AnyXpr) -> Option<Box<dyn Any>>) -> Option<Box<dyn Any>>
    {
        if let Some(l) = cast_optional_any::<L>(self.left.transform(f)) {
            if let Some(r) = cast_optional_any::<R>(self.right.transform(f)) {
                return Some(Box::new(l*r));
            }
        }
        None
    }
}

//***********************************************************************//

struct AnyXpr<'a> {
    expr: &'a dyn std::any::Any
}
impl<'a> AnyXpr<'a>
{
    /// given an output type T, this function downcasts 
    /// to its contained `Xpr<T>`
    fn as_xpr<T>(&'a self) -> Option<&'a Xpr<T>>
    where T: 'static
    {
        self.expr.downcast_ref::<Xpr<T>>()
    }
}

//***********************************************************************//

#[cfg(test)] 
mod tests {
    use super::*;


    #[test]
    fn test_mul_eval_vs_transform() {
        let mut evaluator = |e: AnyXpr| -> Option<Box<dyn Any>> {
            match e.as_xpr::<i32>() {
                Some(&Xpr::Terminal(x)) => Some(Box::new(x)),
                _ => None
            }
        };
    
        let x = Xpr::new(3)*Xpr::new(2)*Xpr::new(7);
        assert_eq!(x.eval(), Ok(42));
        assert_eq!(x.transform(&mut evaluator), Some(42))
    }

}
