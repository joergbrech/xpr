//! This crate allows you to use expression templates with your custom type.
//! It is inspired by [boost::yap](https://www.boost.org/doc/libs/1_74_0/doc/html/yap.html).
//! Expressions can be evaluated or manipulated using transforms, which match subexpressions,
//! and transform matched expressions, do any kind of calculations in between and return anything
//! that implements the operations of the used expression.
//!

use std::ops;
use std::fmt;

pub trait Transform<F>
where F: Fold
{
    fn transform(&self, _: &mut F) -> <F as Fold>::Output;
}

pub trait Fold
{
    type Output;

    fn fold_term<T>(&mut self, _: &Term<T>) -> Self::Output;
    fn fold_neg<T>(&mut self, _: &Neg<T>) -> Self::Output;
    fn fold_add<L,R>(&mut self, _: &Add<L,R>) -> Self::Output;
    fn fold_mul<L,R>(&mut self, _: &Mul<L,R>) -> Self::Output;
}

pub enum Xpr<L,R> {
    Term(Term<L>),
    Neg(Neg<L>),
    Add(Add<L,R>),
    Mul(Mul<L,R>)
}
impl<L,R> fmt::Debug for Xpr<L,R>
where 
    L: fmt::Debug,
    R: fmt::Debug
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Xpr::Term(x) => x.fmt(f),
            Xpr::Neg(x) => x.fmt(f),
            Xpr::Add(x) => x.fmt(f),
            Xpr::Mul(x) => x.fmt(f)
        }
    }
}

impl<T> Xpr<T,()> {
    pub fn new(t: T) -> Self {
        Xpr::<T,()>::Term(Term(Box::new(t)))
    }
}

impl<L,R,F> Transform<F> for Xpr<L,R>
where 
L: Transform<F>,
R: Transform<F>,
F: Fold
{
    fn transform(&self, f: &mut F) -> <F as Fold>::Output
    {

        match self {
            Xpr::Term(x) => x.transform(f),
            Xpr::Neg(x) => x.transform(f),
            Xpr::Add(x) => x.transform(f),
            Xpr::Mul(x) => x.transform(f)
        }
    }
}

#[derive(Debug)]
pub struct Term<T>(Box<T>);
impl<T,F> Transform<F> for Term<T> 
where F: Fold
{
    fn transform(&self, f: &mut F) -> <F as Fold>::Output
    {
        f.fold_term(self)
    }
}

#[derive(Debug)]
pub struct Neg<T>(Box<T>);
impl<T,F> Transform<F> for Neg<T> 
where 
    T: Transform<F>,
    F: Fold
{
    fn transform(&self, f: &mut F) -> <F as Fold>::Output
    {
        f.fold_neg(self)
    }
}

#[derive(Debug)]
pub struct Add<L,R>(Box<L>,Box<R>);
impl<L,R,F> Transform<F> for Add<L,R> 
where
    L: Transform<F>,
    R: Transform<F>,
    F: Fold
{
    fn transform(&self, f: &mut F) -> <F as Fold>::Output
    {
        f.fold_add(self)
    }
}

#[derive(Debug)]
pub struct Mul<L,R>(Box<L>,Box<R>);
impl<L,R,F> Transform<F> for Mul<L,R>
where 
    L: Transform<F>,
    R: Transform<F>,
    F: Fold
{
    fn transform(&self, f: &mut F) -> <F as Fold>::Output
    {
        f.fold_mul(self)
    }
}

// implement operations for Xpr

impl<L,R> ops::Neg for Xpr<L,R> {
    type Output = Xpr<Xpr<L,R>,()>;
    fn neg(self) -> Self::Output {
        Xpr::Neg(Neg(Box::new(self)))
    }
}

impl<L,X,R,Y> ops::Add<Xpr<R,Y>> for Xpr<L,X> {
    type Output = Xpr<Xpr<L,X>,Xpr<R,Y>>;
    fn add(self, other: Xpr<R,Y>) -> Self::Output {
        Xpr::Add(Add(Box::new(self),Box::new(other)))
    }
}

impl<L,X,R,Y> ops::Mul<Xpr<R,Y>> for Xpr<L,X> {
    type Output = Xpr<Xpr<L,X>,Xpr<R,Y>>;
    fn mul(self, other: Xpr<R,Y>) -> Self::Output {
        Xpr::Mul(Mul(Box::new(self),Box::new(other)))
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