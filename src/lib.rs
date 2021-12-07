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
//! // create a folder, which is any struct implementing `Fold`
//! struct Folder;
//! impl Fold for Folder {
//!     type Output = i32;
//!     fn fold<T>(&mut self, e: &Xpr<T>) -> Option<Self::Output> {
//!         match e {
//!             Xpr::Term(_) => { println!("I am a terminal!"); Some(42) },
//!             Xpr::Neg(_) =>  { println!("I am a negation!"); None },
//!             Xpr::Add(_) =>  { println!("I am an addition!"); Some(41) },
//!             Xpr::Mul(_) =>  { println!("I am a multiplication!"); None },
//!         }
//!     }
//! }
//!
//! // create an expression
//! let x = -Xpr::new(5)*(Xpr::new(7) + Xpr::new(-10));
//! println!("{:?}",x)
//! 
//! x.transform(&Folder{});
//! ```

use std::ops;
use std::fmt;

pub trait Transform<F>
where F: Fold
{
    type Output;
    fn transform(&self, _: &F) -> Option<Self::Output>
    {
        None
    }
}

pub trait Fold
{
    type Output;
    fn fold<T>(&mut self, _: &Xpr<T>) -> Option<Self::Output>;
}

#[derive(Clone,Copy,Debug)]
pub enum Xpr<T> {
    Term(T),
    Neg(T),
    Add(T),
    Mul(T)
}

impl<T> Xpr<XprTerm<T>> {
    pub fn new(t: T) -> Self {
        Xpr::Term(XprTerm(Box::new(t)))
    }
}

impl<T,F> Transform<F> for Xpr<T>
where 
T: Transform<F>,
F: Fold
{
    type Output = <T as Transform<F>>::Output;
    fn transform(&self, f: &F) -> Option<Self::Output>
    {
        //if let Some(v) = f.fold(self) { return Some(v); }

        match self {
            Xpr::Term(x) => x.transform(f),
            Xpr::Neg(x) => x.transform(f),
            Xpr::Add(x) => x.transform(f),
            Xpr::Mul(x) => x.transform(f)
        }
    }
}

pub struct XprTerm<T>(Box<T>);
impl<T,F> Transform<F> for XprTerm<T> 
where F: Fold
{
    type Output = <F as Fold>::Output;
}
impl<T> fmt::Debug for XprTerm<T>
where 
    T: fmt::Debug
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct XprNeg<T>{
    input: Box<T>
}
impl<T,F> Transform<F> for XprNeg<T> 
where 
    T: Transform<F>,
    F: Fold,
    <T as Transform<F>>::Output: ops::Neg
{
    type Output = <<T as Transform<F>>::Output as ops::Neg>::Output;
    fn transform(&self, f: &F) -> Option<Self::Output>
    {
        self.input.transform(f).map(|x| -x)
    }
}
impl<T> fmt::Debug for XprNeg<T>
where T: fmt::Debug
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.input.fmt(f)
    }
}

pub struct XprAdd<T>{ 
    operands: Box<T>
}
impl<L,R,F> Transform<F> for XprAdd<(L,R)> 
where
    L: Transform<F>,
    R: Transform<F>,
    F: Fold,
    <L as Transform<F>>::Output: ops::Add<<R as Transform<F>>::Output>
{
    type Output = <<L as Transform<F>>::Output as ops::Add<<R as Transform<F>>::Output>>::Output;
    fn transform(&self, f: &F) -> Option<Self::Output>
    {
        if let Some(l) = self.operands.0.transform(f) {
            if let Some(r) = self.operands.1.transform(f) {
                return Some(l+r);
            }
        }
        None
    }
}
impl<L,R> fmt::Debug for XprAdd<(L,R)>
where 
    L: fmt::Debug,
    R: fmt::Debug
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("")
         .field(&self.operands.0)
         .field(&self.operands.1)
         .finish()
    }
}

pub struct XprMul<T>{ 
    operands: Box<T>
}
impl<L,R,F> Transform<F> for XprMul<(L,R)>
where 
    L: Transform<F>,
    R: Transform<F>,
    F: Fold,
    <L as Transform<F>>::Output: ops::Mul<<R as Transform<F>>::Output>
{
    type Output = <<L as Transform<F>>::Output as ops::Mul<<R as Transform<F>>::Output>>::Output;
    fn transform(&self, f: &F) -> Option<Self::Output>
    {
        if let Some(l) = self.operands.0.transform(f) {
            if let Some(r) = self.operands.1.transform(f) {
                return Some(l*r)
            }
        }
        None
    }
}
impl<L,R> fmt::Debug for XprMul<(L,R)>
where 
    L: fmt::Debug,
    R: fmt::Debug
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("")
         .field(&self.operands.0)
         .field(&self.operands.1)
         .finish()
    }
}

// implement operations for Xpr

impl<L> ops::Neg for Xpr<L> {
    type Output = Xpr<XprNeg<Xpr<L>>>;
    fn neg(self) -> Self::Output {
        Xpr::Neg(XprNeg{ input: Box::new(self)})
    }
}

impl<L,R> ops::Add<Xpr<R>> for Xpr<L> {
    type Output = Xpr<XprAdd<(Xpr<L>,Xpr<R>)>>;
    fn add(self, other: Xpr<R>) -> Self::Output {
        Xpr::Add(XprAdd{ operands: Box::new((self,other))})
    }
}

impl<L,R> ops::Mul<Xpr<R>> for Xpr<L> {
    type Output = Xpr<XprMul<(Xpr<L>,Xpr<R>)>>;
    fn mul(self, other: Xpr<R>) -> Self::Output {
        Xpr::Mul(XprMul{ operands: Box::new((self,other))})
    }
}

#[cfg(test)]
mod tests
{
    // use super::*;

    // struct Evaluator;
    // impl Fold for Evaluator {
    //     type Output = i32;
    //     fn fold<i32>(&mut self, e: &Xpr<i32>) -> Option<Self::Output> {
    //         match e {
    //             Xpr::Term(x) => { Some(x) },
    //             _ => None
    //         }
    //     }
    // }

    // #[test]
    // fn test() {

    // }
}