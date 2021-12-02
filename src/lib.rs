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

use std::any::Any;

pub trait Eval {
    type Output: 'static + Copy;

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

fn cast_optional_any<T: 'static + Copy>(x: Option<Box<dyn Any>>) -> Option<T>
{
    match x {
        Some(x) => match x.downcast_ref::<T>() {
            Some(x) => return Some(*x),
            None => None
        },
        _ => None
    }
}

//***********************************************************************//

pub enum Xpr<T: Sized> {
    Terminal(T),
    Neg(Box<dyn Eval<Output = T>>),
    Mul(Box<dyn Eval<Output = T>>)
}
impl<T> Xpr<T> 
where T: 'static + Copy
{
    pub fn new(value: T) -> Self 
    {
        Xpr::<T>::Terminal(value)
    }

    pub fn as_anyxpr(&self) -> AnyXpr {
        AnyXpr { expr: self }
    }

    pub fn transform_as<R: 'static + Copy>(&self, f: &mut dyn FnMut(AnyXpr) -> Option<Box<dyn Any>>) -> Option<R>
    {
        cast_optional_any(self.transform(f))
    }
}

impl<T: 'static + Copy> Eval for Xpr<T>
{
    type Output = T;

    fn transform(&self, f: &mut dyn FnMut(AnyXpr) -> Option<Box<dyn Any>>) -> Option<Box<dyn Any>>
    {
        if let Some(v) = f(self.as_anyxpr()) { return Some(v); }
        
        match self {
            Xpr::Terminal(_) => None,
            Xpr::Mul(x) => x.transform(f),
            Xpr::Neg(x) => x.transform(f)
        }
    }
}

//***********************************************************************//

struct Neg<T>
{
    input: Box<dyn Eval<Output=T>>,
}
impl<T> Eval for Neg<T>
where T: 'static + Copy + std::ops::Neg,
      <T as std::ops::Neg>::Output: Copy
{
    type Output = <T as std::ops::Neg>::Output;

    fn transform(&self, f: &mut dyn FnMut(AnyXpr) -> Option<Box<dyn Any>>) -> Option<Box<dyn Any>>
    {
        if let Some(l) = cast_optional_any::<T>(self.input.transform(f)) {
            return Some(Box::new(-l));
        }
        None
    }
}

impl<T> std::ops::Neg for Xpr<T>
where T: 'static + Copy + std::ops::Neg,
      <T as std::ops::Neg>::Output: Copy
{
    type Output = Xpr<<T as std::ops::Neg>::Output>;
    fn neg(self) -> Self::Output
    {
        Xpr::Neg( Box::new(Neg{ input: Box::new(self) } ))
    }
}

//***********************************************************************//

struct Mul<L,R>
{
    left: Box<dyn Eval<Output=L>>,
    right: Box<dyn Eval<Output=R>>
}
impl<L,R> Eval for Mul<L,R>
where L: 'static + Copy + std::ops::Mul<R>,
      R: 'static + Copy,
      <L as std::ops::Mul<R>>::Output: 'static + Copy
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

impl<L,R> std::ops::Mul<Xpr<R>> for Xpr<L>
where L: 'static + Copy + std::ops::Mul<R>,
      R: 'static + Copy,
      <L as std::ops::Mul<R>>::Output: Copy
{
    type Output = Xpr<<L as std::ops::Mul<R>>::Output>;
    fn mul(self, other : Xpr<R>) -> Self::Output
    {
        Xpr::Mul( Box::new(Mul{ left: Box::new(self), right: Box::new(other) } ))
    }
}

//***********************************************************************//

pub struct AnyXpr<'a> {
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
        assert_eq!(x.transform_as::<i32>(&mut evaluator), Some(42))
    }

}
