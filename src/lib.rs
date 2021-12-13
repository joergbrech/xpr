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
//! In addition to lazy evaluation, you can manipulate expressions using the [`Fold`] trait. A struct implementing `Fold`
//! can replace terminals *(leaf expressions)* in an expression and perform operations on the way. It can be stateful or a
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
use crate::ops::*;
use crate::fold::*;

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

impl<U> Xpr<U> {
    /// evaluates the expression by unwrapping all terminals and applying the operations
    /// in the expression. It is synactic sugar for folding the expression with the [`Evaluator`].
    pub fn eval<T>(&self) -> OutputFoldable<Evaluator<T>, Self>
    where
        T: Copy,
        U: Foldable<Evaluator<T>>,
    {
        Evaluator(PhantomData::<T>).fold(self)
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

/// contains structs representing the supported operations by xpr. These only need to be used if you intend
/// to implement the [`Fold`] trait.
pub mod ops {
    use super::*;

    /// An `Xpr::Term(Term)` instance is a leaf in an
    /// expression tree, e.g. a single value with no
    /// operations applied to it.
    #[derive(Debug)]
    pub struct Term<T>(pub T);

    impl<T> Xpr<Term<T>> {
        /// creates a new  leaf expression.
        #[inline]
        pub const fn new(t: T) -> Self {
            Self::Term(Term(t))
        }
    }

    /// An `Xpr::Add(Add)` instance is an expression
    /// representing an addition. The generic type
    /// is a two-element tuple containg the left and
    /// right operands.
    #[derive(Debug)]
    pub struct Add<T>(pub T);

    // implement addition for Xpr<T> expressions
    impl<L, R> std::ops::Add<Xpr<R>> for Xpr<L> {
        type Output = Xpr<Add<(Self, Xpr<R>)>>;
        #[inline]
        fn add(self, other: Xpr<R>) -> Self::Output {
            Xpr::Add(Add((self, other)))
        }
    }

    /// The output of the addition of two `OutputFoldable<F,_>` types, where `F` implements [`Fold`]
    pub type OutputFoldableAdd<F, L, R> =
        <OutputFoldable<F, L> as std::ops::Add<OutputFoldable<F, R>>>::Output;
}


/// contains the [`Fold`] and [`Foldable`] traits, as well as implementors of [`Fold`] for 
/// convenience
pub mod fold
{
    use super::*;

    /// internal type for evaluating expression. It folds each terminal in an expression tree to its wrapped 
    /// type and performs the operations on its upwards traversal through the tree, thus evaluating the expression.
    pub struct Evaluator<T>(pub PhantomData<T>);

    impl<T> Fold for Evaluator<T>
    where
        T: Copy,
    {
        type TerminalType = T;
        type Output = Self::TerminalType;
        /// replaces Terminal values with their wrapped type
        #[inline]
        fn fold_term(&mut self, Term(x): &Term<T>) -> T {
            *x
        }
    }

    /// The output of `T` as `Foldable` by `F`, where `F` implements [`Fold`]
    pub type OutputFoldable<F, T> = <T as Foldable<F>>::Output;

    /// A trait for expression manipulation. `Fold` together with [`Xpr`] are at the heart of this crate.
    pub trait Fold {
        // implement the [fold pattern](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html)

        /// `Self` will only manipulate terminals wrapping `TerminalType`
        type TerminalType;

        /// The output of [`Fold::fold_term`], `Self` will replace all `Term<Self::TerminalType>` by values of this type.
        type Output;

        fn fold_term(&mut self, _: &Term<Self::TerminalType>) -> Self::Output;

        #[inline]
        fn fold_add<L, R>(&mut self, x: &Add<(L, R)>) -> OutputFoldableAdd<Self, L, R>
        where
            L: Foldable<Self>,
            R: Foldable<Self>,
            OutputFoldable<Self, L>: std::ops::Add<OutputFoldable<Self, R>>,
        {
            // ping-pongs to to the Foldable::fold impl for Xpr<T> for both arguments
            // and applies the operation +
            (x.0 .0).fold(self) + (x.0 .1).fold(self)
        }

        #[inline]
        fn fold<T>(&mut self, x: &Xpr<T>) -> OutputFoldable<Self, T>
        where
            T: Foldable<Self>,
        {
            // ping-pong to the Foldable::fold impl for Term<T> and Add<L,R>
            match x {
                Xpr::Term(y) | Xpr::Add(y) => y.fold(self),
            }
        }
    }

    /// Just contains the Sealed pattern to make Foldable not implementable from the outside
    mod private {
        use super::{Fold, Foldable};

        /// Sealed pattern to make Foldable not implementable from the outside
        pub trait Sealed<F: ?Sized> {}

        impl<F, T> Sealed<F> for T
        where
            T: Foldable<F>,
            F: Fold + ?Sized,
        {
        }
    }

    /// An internal ping-pong trait for `Fold`. This trait is part of the signature of the methods in [`Fold`],
    /// but not implementable from the outside.
    ///
    /// The trait is needed internally to implement the [Fold pattern](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html)
    /// for generic expressions:
    ///
    /// The method [`Fold::fold`] will unwrap an explicit `Xpr<T>` wrapping a generic `Foldable` type `T`. It will
    /// forward the nested generic element to an explicit implementation of  `Foldable`.
    ///
    /// The `Foldable` implementations will then ping-pong the call back to the methods in `Fold` that handle
    /// concrete expression types, e.g. [`Fold::fold_add`]. These methods can in turn recurse to wrapped internal
    /// generic `Foldable` types by ping-ponging back to the explicit implementations of `Foldable`.
    ///
    /// The ping-pong recursion ends when we hit a leaf expression in `Fold::fold_term`, which will not
    /// trigger a recursion to a nested foldable type.
    pub trait Foldable<F>: private::Sealed<F>
    where
        F: Fold + ?Sized,
    {

        /// The output of the fold operation
        type Output;

        fn fold(&self, _: &mut F) -> Self::Output;
    }

    impl<T, F> Foldable<F> for Xpr<T>
    where
        T: Foldable<F>,
        F: Fold,
    {
        type Output = OutputFoldable<F, T>;

        // ping-pongs to Fold::fold
        #[inline]
        fn fold(&self, f: &mut F) -> Self::Output {
            f.fold(self)
        }
    }

    impl<T, F> Foldable<F> for Term<T>
    where
        F: Fold<TerminalType = T>,
    {
        type Output = <F as Fold>::Output;

        // ping-pongs to Fold::fold
        #[inline]
        fn fold(&self, f: &mut F) -> Self::Output {
            f.fold_term(self)
        }
    }

    impl<L, R, F> Foldable<F> for Add<(L, R)>
    where
        L: Foldable<F>,
        R: Foldable<F>,
        F: Fold,
        OutputFoldable<F, L>: std::ops::Add<OutputFoldable<F, R>>,
    {
        type Output = OutputFoldableAdd<F, L, R>;

        // ping-pongs to Fold::fold
        #[inline]
        fn fold(&self, f: &mut F) -> Self::Output {
            f.fold_add(self)
        }
    }
}

pub use fold::{Fold, Foldable};

#[cfg(test)]
mod tests {
    use super::*;

    struct FortytwoifyI32;
    impl Fold for FortytwoifyI32 {
        type TerminalType = i32;
        type Output = Xpr<Term<i32>>;
        fn fold_term(&mut self, _: &Term<i32>) -> Self::Output {
            Xpr::new(42)
        }
    }

    // #[test]
    // fn test_different_terminal_types()
    // {
    //     let x = Xpr::new(5) + Xpr::new(true);
    //     let y = FortytwoifyI32.fold(&x);
    // }

    #[test]
    fn test_eval() {
        let x = Xpr::new(1) + Xpr::new(5);
        assert_eq!(x.eval(), 6);
        assert_eq!(x.eval(), x.fold(&mut Evaluator(PhantomData::<i32>)));
        assert_eq!(x.eval(), Evaluator(PhantomData::<i32>).fold(&x));
    }
}
