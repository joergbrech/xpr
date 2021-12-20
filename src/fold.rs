//! contains the [`Fold`] and [`Foldable`] traits, as well as implementors of [`Fold`] for
//! convenience. Currently, [`Evaluator`] is the only implementor.

use std::marker::PhantomData;

use super::{
    ops::{Term}
};

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

/// A trait for expression manipulation. `Fold` together with [`Xpr`] are at the heart of this crate.
pub trait Fold<T: ?Sized> {
    // implement the [fold pattern](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html)

    /// The output of [`Fold::fold_term`], `Self` will replace all `Term<Self::TerminalType>` by values of this type.
    type Output;

    fn fold(&mut self, _: &T) -> Self::Output;
}

/// Just contains the Sealed pattern to make Foldable not implementable from the outside
mod private {
    use super::{Fold, Foldable};

    /// Sealed pattern to make Foldable not implementable from the outside
    pub trait Sealed<F: ?Sized> {}

    impl<F, T> Sealed<F> for T
    where
        T: Foldable<F>,
        F: Fold<Self> + ?Sized,
    {
    }
}

/// An internal ping-pong trait for `Fold`. This trait is part of the signature of the methods in [`Fold`],
/// but not implementable from the outside. As a user of `xpr`, you don't have to worry about the implementation
/// details of this trait.
///
/// If you are curious anyway: The trait is needed internally to implement the
/// [Fold pattern](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html)
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
    F: Fold<Self> + ?Sized,
{
    /// The output of the fold operation
    type Output;

    /// ping-pong back to the appropriate method in `Fold` corresponding to `Self`. E.g.
    /// the implementation of `Foldable` for [`crate::ops::Add`] will call [`Fold::fold_add`] in its implementation
    /// of `Foldable::fold`.
    fn fold(&self, _: &mut F) -> Self::Output;
}

/// The output of `T` as `Foldable` by `F`, where `F` implements [`Fold`]
pub type OutputFoldable<F, T> = <T as Foldable<F>>::Output;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ops, Xpr};

    struct FortytwoifyI32;
    impl Fold<ops::Term<i32>> for FortytwoifyI32 {
        type Output = Xpr<ops::Term<i32>>;
        fn fold(&mut self, _: &ops::Term<i32>) -> Self::Output {
            Xpr::new(42)
        }
    }

    // #[test]
    // fn test_fold_different_terminal_types()
    // {
    //     // let x = Xpr::new(5) + Xpr::new(true);
    //     // let y = FortytwoifyI32.fold(&x);
    //     assert!(false);
    // }
}
