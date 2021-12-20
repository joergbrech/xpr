//! This crate allows you to use expression templates with your custom type.
//! It is inspired by the C++ expression template library [boost::yap](https://www.boost.org/doc/libs/1_74_0/doc/html/yap.html).
//!
//! To use `xpr`, all you need to do is wrap all input variables to an expression in calls to `Xpr::new` and apply any supported
//! operation to them. The result will be an object representing the operations you performed. Any possible chain of operations
//! will be represented by their own specific type.
//! 
//! ## Limitations
//! 
//! Current limitiations of the library are that an expression can only hold terminals
//! of the same type and that terminals are the only expressions that can be transformed using
//! [`Fold`] *(see below)*. These restrictions can be lifted, once Rust supports 
//! [specialization](https://github.com/rust-lang/rust/issues/31844), which is probably not any 
//! time soon.
//!
//! ## Usage
//! 
//! Evaluation of the expression is be performed lazily by calling `Xpr::eval`:
//!
//! ```
//! use xpr::Xpr;
//!
//! // create an exression. x will be a type representing the
//! // addition of 7 and 5, not the result of the calculation
//! let x = Xpr::new(7) + Xpr::new(5);
//!
//! // lazily evaluate the expression
//! assert_eq!(x.eval(), 12);
//! ```
//!
//! In the above example, the type of `x` is
//!
//! `Xpr<Add<(Xpr<Term<{integer}>>, Xpr<Term<{integer}>>)>>`,
//!
//! where all crate and nested module names have been omitted for better readability.
//! It is a type representing the addition of two integers. To actually evaulate the expression, we call `x.eval()`.
//!
//! In addition to lazy evaluation, you can manipulate expressions using the [`Fold`] trait. A struct implementing `Fold`
//! can replace terminals *(leaf expressions)* and perform operations along the way. It can be stateful or a
//! zero-sized type, depending on your needs.
//!
//! ```
//! use xpr::{Xpr, Fold, ops::Term};
//!
//! struct Fortytwoify;
//!
//! impl Fold<Term<i32>> for Fortytwoify {
//!
//!     // We will replace these terminals by terminal expressions wrapping i32 values
//!     type Output = Xpr<Term<i32>>;
//!
//!     // replaces terminals with terminals wrapping the value 42
//!     fn fold(&mut self, _: &Term<i32>) -> Self::Output {
//!         Xpr::new(42)
//!     }
//! }
//!
//! // create an expression
//! let x = Xpr::new(7) + Xpr::new(5);
//!
//! // fortitwoify the expression
//! let y = Fortytwoify.fold(&x);
//!
//! // lazily evaluate the expression
//! assert_eq!(y.eval(), 84);
//! ```
//!
//! Refer to the documentation of [`Fold`] for more useful examples.

use std::marker::PhantomData;

pub mod fold;
pub mod ops;

pub use crate::fold::{Fold, Foldable};

use crate::fold::OutputFoldable;
use crate::ops::Term;

/// An expression. `Xpr` together with the [`Fold`] trait are at the heart of this crate.
///
/// The nested type is a specific struct representing the operation, e.g. [`ops::Add`].
///
/// `Xpr` instances should be instantiated via the [`Xpr::new`] method, which creates
/// `Xpr<ops::Term>` leaf expressions. The other variants are constructed from them by
/// applying operations to the leaf expressions.
///
/// ```
/// use xpr::*;
/// let x = Xpr::new(5);
/// let y = Xpr::new(1);
/// let z = x + y;
/// //type of z is xpr::Xpr<xpr::ops::Add<xpr::Xpr<xpr::ops::Term<{integer}>>, xpr::Xpr<xpr::ops::Term<{integer}>>>>
/// ```
#[derive(Debug)]
pub struct Xpr<T>(T);

//In a perfect world, this would be  an enum. But this only makes sense as soon as
// enum variants are promoted as first class types, so that we have per variant impls and irefutabl
// patterns in fn args, see also https://github.com/rust-lang/rfcs/pull/2593

impl<T> Xpr<Term<T>> {
    /// creates a new  leaf expression.
    #[inline]
    pub const fn new(t: T) -> Self {
        Self(Term(t))
    }
}

impl<T, F> Foldable<F> for Xpr<T>
where
    T: Foldable<F>,
    F: Fold<Self> + Fold<T>,
{
    type Output = <F as Fold<Self>>::Output;

    // ping-pongs to Fold::fold
    #[inline]
    fn fold(&self, f: &mut F) -> Self::Output {
        f.fold(self)
    }
}

impl<T, U> Fold<Xpr<T>> for U
where
    U: Fold<T>,
    T: Foldable<Self>,
{
    type Output = OutputFoldable<Self, T>;

    #[inline]
    fn fold(&mut self, Xpr(t): &Xpr<T>) -> <T as Foldable<Self>>::Output {
        // ping-pong to the Foldable::fold impl for Term<T> and Add<L,R>
        t.fold(self)
    }
}

/// internal type for evaluating expression. It folds each terminal in an expression tree to its wrapped
/// type and performs the operations on its upwards traversal through the tree, thus evaluating the expression.
pub struct Evaluator<T>(pub PhantomData<T>);

impl<T> Fold<Term<T>> for Evaluator<T>
where
    T: Copy,
{
    type Output = T;
    /// replaces Terminal values with their wrapped type
    #[inline]
    fn fold(&mut self, Term(x): &Term<T>) -> T {
        *x
    }
}

impl<U> Xpr<U> {
    /// evaluates the expression by unwrapping all terminals and applying the operations
    /// in the expression. It is synactic sugar for folding the expression with [`Evaluator`].
    #[inline]
    pub fn eval<T>(&self) -> fold::OutputFoldable<Evaluator<T>, Self>
    where
        T: Copy,
        U: Foldable<Evaluator<T>>,
        Evaluator<T>: fold::Fold<U> + fold::Fold<Self>,
    {
        Evaluator(PhantomData::<T>).fold(self)
    }
}

/// A trait for converting an instance into xpr terminals
pub trait Expression {
    /// Wrap a reference of self in an Xpr terminal
    #[inline]
    fn as_xpr(&self) -> Xpr<Term<&Self>>
    where
        Self: Sized,
    {
        Xpr::new(self)
    }

    /// Consume self and return as Xpr terminal
    #[inline]
    fn into_xpr(self) -> Xpr<Term<Self>>
    where
        Self: Sized,
    {
        Xpr::new(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval() {
        let x = Xpr::new(1) + Xpr::new(5);
        assert_eq!(x.eval(), 6);
        assert_eq!(x.eval(), x.fold(&mut Evaluator(PhantomData::<i32>)));
        assert_eq!(x.eval(), Evaluator(PhantomData::<i32>).fold(&x));
    }

    struct Num(i32);
    impl std::ops::Add<i32> for Num {
        type Output = i32;
        fn add(self, other: i32) -> Self::Output {
            self.0 + other
        }
    }

    // #[test]
    // fn test_eval_different_terminal_types()
    // {
    //     // let x = Xpr::new(Num(2)) + Xpr::new(1);
    //     // assert_eq!(x.eval(), 3);
    //     assert!(false);
    // }
}
