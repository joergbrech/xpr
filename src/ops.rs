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

// expand implementations for binary operations
binary_op!(Add, add, +, OutputOfAdd, "An `Xpr<Add<L,R>>` represents addition: `l + r`");
binary_op!(Sub, sub, -, OutputOfSub, "An `Xpr<Sub<L,R>>` represents subtractio: `l - r`");
binary_op!(Mul, mul, *, OutputOfMul, "An `Xpr<Mul<L,R>>` represents multiplication: l * r");
binary_op!(Div, div, /, OutputOfDiv, "An `Xpr<Div<L,R>>` represents division: l / r");
binary_op!(Rem, rem, %, OutputOfRem, "An `Xpr<Rem<L,R>>` represents remainder: R`: l % r");
binary_op!(Shl, shl, <<, OutputOfShl, "An `Xpr<Shl<L,R>>` represents the left shift operator: R`: l << r");
binary_op!(Shr, shr, >>, OutputOfShr, "An `Xpr<Shr<L,R>>` represents the right shift operator: R`: l >> r");
binary_op!(BitAnd, bitand, &, OutputOfBitAnd, "An `Xpr<BitAnd<L,R>>` represents the bitwise AND: R`: `l & r`");
binary_op!(BitOr, bitor, |, OutputOfBitOr, "An `Xpr<BitOr<L,R>>` represents the bitwise OR: L` and `r: R`: `l | r`");
binary_op!(BitXor, bitxor, ^, OutputOfBitXor, "An `Xpr<BitXor<L,R>>` represents the bitwise XOR: R`: `l ^ r`");


// To Do
//  - unary operators
//  - XXAssign operators (fn takes &mut self)
//  - special operators like Fn
//  - TESTING!!!!