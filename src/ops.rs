//! contains structs representing the supported operations by xpr. These only need to be used if you intend
//! to implement the [`Fold`] trait.

use super::{
    foldable::{Foldable, OutputFoldable},
    ExpressionCast, Fold, Xpr,
};

/// An `Xpr<Term<T>>` instance is a leaf in an expression tree, e.g. a single value of type `T` with no
/// operations applied to it.
#[derive(Debug, Copy, Clone)]
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

// expand implementations for unary operations
unary_op!(Neg, neg, -, OutputOfNeg, "An `Xpr<Neg<T>>` represents negation: `- t`");
unary_op!(
    Not,
    not,
    !,
    OutputOfNot,
    "An `Xpr<Not<T>>` represents logical negation: `! t`"
);

// expand implementations for binary operations
binary_op!(Add, add, +, OutputOfAdd, "An `Xpr<Add<L,R>>` represents addition: `l + r`");
binary_op!(Sub, sub, -, OutputOfSub, "An `Xpr<Sub<L,R>>` represents subtractio: `l - r`");
binary_op!(Mul, mul, *, OutputOfMul, "An `Xpr<Mul<L,R>>` represents multiplication: `l * r`");
binary_op!(Div, div, /, OutputOfDiv, "An `Xpr<Div<L,R>>` represents division: `l / r`");
binary_op!(Rem, rem, %, OutputOfRem, "An `Xpr<Rem<L,R>>` represents remainder: `l % r`");
binary_op!(Shl, shl, <<, OutputOfShl, "An `Xpr<Shl<L,R>>` represents the left shift operator: `l << r`");
binary_op!(Shr, shr, >>, OutputOfShr, "An `Xpr<Shr<L,R>>` represents the right shift operator: `l >> r`");
binary_op!(BitAnd, bitand, &, OutputOfBitAnd, "An `Xpr<BitAnd<L,R>>` represents the bitwise AND: `l & r`");
binary_op!(BitOr, bitor, |, OutputOfBitOr, "An `Xpr<BitOr<L,R>>` represents the bitwise OR: L` and `r: `l | r`");
binary_op!(BitXor, bitxor, ^, OutputOfBitXor, "An `Xpr<BitXor<L,R>>` represents the bitwise XOR: `l ^ r`");

// Missing operators:
//
//  - unary operators Deref, DerefMut, Index, IndexMut, Drop
//     - Issue: Some take mutable self, see issue with XXXAssign
//     - Issue: Some return a reference, e.g. Index
//
//  - XXXAssign operators
//     - Issue: trait functions don't return anything but mutate self. By design of xpr, this
//              would require a change of the type of self. E.g. for AddAssign, Xpr<U> would be
//              replaced by Xpr<Add<Xpr<U>, Xpr<V>>>. I can't change the type of self
//
//  - special operators like Fn, FnOnce, FnMut, RangeBounds

// To Do:
//  - TESTING!!!!
