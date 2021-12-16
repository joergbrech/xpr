//! contains structs representing the supported operations by xpr. These only need to be used if you intend
//! to implement the [`Fold`] trait.

use super::{*, fold::*};

/// An `Xpr::Term(Term)` instance is a leaf in an
/// expression tree, e.g. a single value with no
/// operations applied to it.
#[derive(Debug)]
pub struct Term<T>(pub T);

impl<T, F> Foldable<F> for Term<T>
where
    F: Fold<TerminalType = T>
{
    type Output = <F as Fold>::Output;

    // ping-pongs to Fold::fold
    #[inline]
    fn fold(&self, f: &mut F) -> Self::Output {
        f.fold_term(self)
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

/// The output of the addition of two `OutputFoldable<F,_>` types, where `F` implements [`Fold`]
pub type OutputFoldableAdd<F, L, R> =
    <OutputFoldable<F, L> as std::ops::Add<OutputFoldable<F, R>>>::Output;