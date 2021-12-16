//! This crate allows you to use expression templates with your custom type.
//! It is inspired by the C++ expression template library [boost::yap](https://www.boost.org/doc/libs/1_74_0/doc/html/yap.html).
//!
//! To use `xpr`, all you need to do is wrap all input variables to an expression in calls to `Xpr::new` and apply any supported
//! operation to them. The result will be an object representing the operations you performed. Any possible chain of operations
//! will be represented by their own specific type.
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
//! impl Fold for Fortytwoify {
//!
//!     // We will only manipulate terminal expressions wrapping i32 values
//!     type TerminalType = i32;
//!
//!     // We will replace these terminals by terminal expressions wrapping i32 values
//!     type Output = Xpr<Term<i32>>;
//!
//!     // replaces terminals with terminals wrapping the value 42
//!     fn fold_term(&mut self, _: &Term<i32>) -> Self::Output {
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
use std::fmt;
use std::marker::PhantomData;


pub mod fold;
pub mod ops;

pub use crate::fold::{Fold, Foldable};

/// All supported operations. Any expression will be of this type. `Xpr` together with the
/// [`Fold`] trait are at the heart of this crate.
///
/// The nested type is a specific struct representing the operation of the variant, e.g.
/// the [`Xpr::Add`] variant will always wrap an [`ops::Add`] instance.
///
/// `Xpr` instances should be instantiated via the [`Xpr::new`] method, which creates
/// `Xpr::Term(ops::Term)` leaf expressions. The other variants are constructed from them by
/// applying operations to the leaf expressions.
///
/// ```
/// use xpr::*;
/// let x = Xpr::new(5);
/// let y = Xpr::new(1);
/// let z = x + y;
/// //type of z is xpr::Xpr<xpr::ops::Add<(xpr::Xpr<xpr::ops::Term<{integer}>>, xpr::Xpr<xpr::ops::Term<{integer}>>)>>
/// ```
pub enum Xpr<T> {
    /// `Xpr::Term(ops::Term)` represents a leaf expression in an expression tree
    Term(T),
    /// `Xpr::Add(ops::Add)` represents an addition of two expressions
    Add(T),
}

impl<T> Xpr<ops::Term<T>> {
    /// creates a new  leaf expression.
    #[inline]
    pub const fn new(t: T) -> Self {
        Self::Term(ops::Term(t))
    }
}

impl<T, F> Foldable<F> for Xpr<T>
where
    T: Foldable<F>,
    F: Fold,
{
    type Output = fold::OutputFoldable<F, T>;

    // ping-pongs to Fold::fold
    #[inline]
    fn fold(&self, f: &mut F) -> Self::Output {
        f.fold(self)
    }
}

impl<U> Xpr<U> {
    /// evaluates the expression by unwrapping all terminals and applying the operations
    /// in the expression. It is synactic sugar for folding the expression with the [`fold::Evaluator`].
    pub fn eval<T>(&self) -> fold::OutputFoldable<fold::Evaluator<T>, Self>
    where
        T: Copy,
        U: Foldable<fold::Evaluator<T>>,
    {
        fold::Evaluator(PhantomData::<T>).fold(self)
    }
}

impl<T> fmt::Debug for Xpr<T>
where
    T: fmt::Debug,
{
    // delegates formatter to wrapped type
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Xpr::Term(x) | Xpr::Add(x) => x.fmt(f),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval() {
        let x = Xpr::new(1) + Xpr::new(5);
        assert_eq!(x.eval(), 6);
        assert_eq!(x.eval(), x.fold(&mut fold::Evaluator(PhantomData::<i32>)));
        assert_eq!(x.eval(), fold::Evaluator(PhantomData::<i32>).fold(&x));
    }

    struct Num(i32);
    impl std::ops::Add<i32> for Num
    {
        type Output = i32;
        fn add(self, other: i32) -> Self::Output 
        {
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
