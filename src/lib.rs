//! This crate allows you to use expression templates with your custom type.
//! It is inspired by the C++ expression template library [boost::yap](https://www.boost.org/doc/libs/1_74_0/doc/html/yap.html).
//! It currently relies on the unstable `generic_associated_types` feature.
//!
//! To use `xpr`, all you need to do is wrap all input variables to an expression in calls to `Xpr::new` and apply any supported
//! operation to them. The result will be an object representing the operations you performed. Any possible chain of operations
//! will be represented by their own specific type.
//!
//! Evaluation of the expression is be performed lazily by calling `Xpr::eval`:
//!
//! ```
//! #![feature(generic_associated_types)]
//! use xpr::*;
//!
//! let x = Xpr::new(7) + Xpr::new(5);
//! assert_eq!(x.eval(), 12);
//! ```
//!
//! In addition to lazy evaluation, you can manipulate expressions using the [`Fold`] trait. You define a `struct` as a
//! manipulator and implement the `Fold` trait for it. The [`Fold::fold_term`] function will manipulate all terminals in
//! an expression and it is likely the only function that needs to be overwritten.
//!
//! ```
//! #![feature(generic_associated_types)]
//! use xpr::{*, ops::*};
//!
//! struct Fortytwoify;
//! impl Fold for Fortytwoify {
//!     type TerminalType<T> = T;
//!     type Output<T> = Xpr<Term<i32>>;
//!     // replaces all terminals with terminals wrapping the value 42
//!     fn fold_term<T>(&mut self, _: &Term<T>) -> Self::Output<T> {
//!         Xpr::new(42)
//!     }
//! }
//!
//! let x = Xpr::new(7) + Xpr::new(5);
//! let y = Fortytwoify.fold(&x);
//! assert_eq!(y.eval(), 84);
//! ```
//!
//! All of this is implemented only with generics static dispatch for maximum performance.
//!
#![feature(generic_associated_types)]

use std::fmt;

/// All supported operations.
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

impl<T> Xpr<T>
where
    T: Foldable<Evaluator>,
{
    /// evaluates the expression by unwrapping all terminals and applying the operations
    /// in the expression. It is synactic sugar for folding the expression with the [`Evaluator`].
    pub fn eval(&self) -> OutputFoldable<Evaluator, Self> {
        Evaluator.fold(self)
    }
}

/// A zero-sized type that implements the [`Fold`] trait. It folds each terminal in an expression
/// tree to its wrapped type and performs the operations on its upwards traversal through the
/// tree, thus evaluating the expression.
pub struct Evaluator;
impl Fold for Evaluator {
    type TerminalType<T> = T;
    type Output<T> = T;
    /// replaces Terminal values with their wrapped type
    fn fold_term<T>(&mut self, x: &Term<T>) -> T
    where
        T: Copy,
    {
        x.0
    }
}

impl<T> fmt::Debug for Xpr<T>
where
    T: fmt::Debug,
{
    // delegates formatter to wrapped type
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Xpr::Term(x) => x.fmt(f),
            Xpr::Add(x) => x.fmt(f),
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
        pub fn new(t: T) -> Self {
            Xpr::Term(Term(t))
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
        type Output = Xpr<Add<(Xpr<L>, Xpr<R>)>>;
        fn add(self, other: Xpr<R>) -> Self::Output {
            Xpr::Add(Add((self, other)))
        }
    }
}
use ops::{Add, Term};

// The types get a bit yucky...let's add some shorthand notation

/// The output of `T` as `Foldable` by `F`, where `F` implements [`Fold`]
pub type OutputFoldable<F, T> = <T as Foldable<F>>::Output;
/// The output of the addition of two `OutputFoldable<F,_>` types, where `F` implements [`Fold`]
pub type OutputFoldableAdd<F, L, R> =
    <OutputFoldable<F, L> as std::ops::Add<OutputFoldable<F, R>>>::Output;

pub trait Fold {
    // implement the [fold pattern](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html)

    ///
    type TerminalType<T>;

    /// The output of [`Fold::fold_term`]
    type Output<T>;

    fn fold_term<T>(&mut self, _: &Term<Self::TerminalType<T>>) -> Self::Output<T>
    where
        T: Copy;

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

    fn fold<T>(&mut self, x: &Xpr<T>) -> OutputFoldable<Self, T>
    where
        T: Foldable<Self>,
    {
        // ping-pong to the Foldable::fold impl for Term<T> and Add<L,R>
        match x {
            Xpr::Term(x) => x.fold(self),
            Xpr::Add(x) => x.fold(self),
        }
    }
}

mod private {
    use super::*;

    pub trait Sealed<F: ?Sized> {}

    impl<F, T> Sealed<F> for T
    where
        T: Foldable<F>,
        F: Fold + ?Sized,
    {
    }
}

/// A ping-pong trait needed to implement the fold pattern to recurse the generic expressionin Xpr:
///
/// The methods in Fold will unwrap an explicit `Xpr<T>` containing a generic Foldable type. It will
/// forward the nested generic element to the methods in Foldable, which will expand the generic
/// type to the explicit type of the expression.
///
/// The Foldable implementations will then ping-pong the call back to the methods in Fold that handle
/// concrete types. These methods can in turn recurse to the wrapped internal generic Foldable type
/// by ping-ponging back to the Foldable trait, which will expand the ....
///
/// The ping-pong recursion ends when we hit a leaf expression in Fold::fold_term, which will not
/// trigger a recursion to a nested foldable type.
#[doc(hidden)]
pub trait Foldable<F>: private::Sealed<F>
where
    F: Fold + ?Sized,
{
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
    fn fold(&self, f: &mut F) -> Self::Output {
        f.fold(self)
    }
}

impl<T, F> Foldable<F> for Term<T>
where
    F: Fold<TerminalType<T> = T>,
    T: Copy,
{
    type Output = <F as Fold>::Output<T>;

    // ping-pongs to Fold::fold
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
    fn fold(&self, f: &mut F) -> Self::Output {
        f.fold_add(self)
    }
}
