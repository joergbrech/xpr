//! contains the [`Foldable`] trait
use crate::Fold;

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
/// concrete expression types, e.g. [`crate::ops::Add`]. These methods can in turn recurse to wrapped internal
/// generic `Foldable` types by ping-ponging back to the explicit implementations of `Foldable`.
///
/// The ping-pong recursion ends when we hit a leaf expression in `Fold::fold_term`, which will not
/// trigger a recursion to a nested foldable type.
pub trait Foldable<F>
where
    F: Fold<Self> + ?Sized,
{
    /// The output of the fold operation
    type Output;

    /// ping-pong back to the appropriate method in `Fold` corresponding to `Self`.
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
