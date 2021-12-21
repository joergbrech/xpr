//! contains structs representing the supported operations by xpr. These only need to be used if you intend
//! to implement the [`Fold`] trait.

use super::{
    foldable::{Foldable, OutputFoldable},
    Expression, Fold, Xpr,
};

/// An `Xpr<Term<T>>` instance is a leaf in an expression tree, e.g. a single value of type `T` with no
/// operations applied to it.
#[derive(Debug)]
pub struct Term<T>(pub T);

impl<T, F> Foldable<F> for Term<T>
where
    F: Fold<Self>,
{
    type Output = <F as Fold<Self>>::Output;

    // ping-pongs to Fold::fold
    #[inline]
    fn fold(&self, f: &mut F) -> Self::Output {
        f.fold(self)
    }
}

/// An `Xpr<Add<L,R>>` instance is an expression representing an addition.
#[derive(Debug)]
pub struct Add<L, R>(pub L, pub R);

// implement addition for Xpr<T> expressions
impl<L, R> std::ops::Add<Xpr<R>> for Xpr<L> {
    type Output = Xpr<Add<Self, Xpr<R>>>;
    #[inline]
    fn add(self, other: Xpr<R>) -> Self::Output {
        Xpr(Add(self, other))
    }
}

// implement addition for Xpr<T> + Expression
impl<'a, L, R> std::ops::Add<&'a R> for Xpr<L>
where
    R: Expression,
{
    type Output = Xpr<Add<Self, Xpr<Term<&'a R>>>>;
    #[inline]
    fn add(self, other: &'a R) -> Self::Output {
        Xpr(Add(self, other.as_xpr()))
    }
}

impl<L, R, F> Foldable<F> for Add<L, R>
where
    L: Foldable<F>,
    R: Foldable<F>,
    F: Fold<Self> + Fold<L> + Fold<R>,
    OutputFoldable<F, L>: std::ops::Add<OutputFoldable<F, R>>,
{
    type Output = <F as Fold<Self>>::Output;

    // ping-pongs to Fold::fold
    #[inline]
    fn fold(&self, f: &mut F) -> Self::Output {
        f.fold(self)
    }
}

impl<L, R, U> Fold<Add<L, R>> for U
where
    U: Fold<L> + Fold<R>,
    L: Foldable<Self>,
    R: Foldable<Self>,
    OutputFoldable<Self, L>: std::ops::Add<OutputFoldable<Self, R>>,
{
    type Output = <OutputFoldable<Self, L> as std::ops::Add<OutputFoldable<Self, R>>>::Output;

    #[inline]
    fn fold(&mut self, Add(l, r): &Add<L, R>) -> Self::Output {
        // ping-pongs to to the Foldable::fold impl for Xpr<T> for both arguments
        // and applies the operation +
        l.fold(self) + r.fold(self)
    }
}

/// The output of adding two foldable L, R where L and R are assumed not to be `Xpr`,
/// e.g. `OutputOfAdd<Term<u32>, Term<bool>>`.
///
/// This is a convenience type to not have to write out the `Xpr` enum explicitly.
pub type OutputOfAdd<L, R> = Xpr<Add<Xpr<L>, Xpr<R>>>;
