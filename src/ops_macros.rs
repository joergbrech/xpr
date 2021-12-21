//! defines the internal macros for the ops module

macro_rules! unary_op {
    ($op:ident, $fun:ident, $op_token:tt, $output_of:ident, $doc:expr) => {
        #[doc = $doc]
        #[derive(Debug)]
        pub struct $op<T>(pub T);

        // overload the operator for Xpr
        impl<T> std::ops::$op for Xpr<T>
        {
            type Output = Xpr<$op<Xpr<T>>>;
            #[inline]
            fn $fun(self) -> Self::Output {
                Xpr($op(self))
            }
        }

        impl<T, F> Foldable<F> for $op<T>
        where
            T: Foldable<F>,
            F: Fold<Self> + Fold<T>,
            OutputFoldable<F, T>: std::ops::$op,
        {
            type Output = <F as Fold<Self>>::Output;

            // ping-pongs to Fold::fold
            #[inline]
            fn fold(&self, f: &mut F) -> Self::Output {
                f.fold(self)
            }
        }

        impl<T, U> Fold<$op<T>> for U
        where
            U: Fold<T>,
            T: Foldable<Self>,
            OutputFoldable<Self, T>: std::ops::$op,
        {
            type Output = <OutputFoldable<Self, T> as std::ops::$op>::Output;

            #[inline]
            fn fold(&mut self, $op(t): &$op<T>) -> Self::Output {
                // ping-pongs to to the Foldable::fold impl for Xpr<T> for both arguments
                // and applies the operation
                $op_token t.fold(self)
            }
        }

        /// The output of the given operation for two operands L, R where L and R are operation structs
        /// from this module, e.g. 
        /// e.g. `OutputOfXXX<Term<u32>, OutputOfMul<Term<bool>, Term<f64>>>`.
        ///
        /// This is a convenience type to not have to write out the `Xpr` enum explicitly.
        pub type $output_of<T> = Xpr<$op<Xpr<T>>>;

    }
}

macro_rules! binary_op {
    ($op:ident, $fun:ident, $op_token:tt, $output_of:ident, $doc:expr) => {
        #[doc = $doc]
        #[derive(Debug)]
        pub struct $op<L,R>(pub L, pub R);

        // overload the operator for (Xpr,Xpr)
        impl<L,R> std::ops::$op<Xpr<R>> for Xpr<L>
        {
            type Output = Xpr<$op<Xpr<L>, Xpr<R>>>;
            #[inline]
            fn $fun(self, other: Xpr<R>) -> Self::Output {
                Xpr($op(self, other))
            }
        }

        // overload the operator for (Xpr,Expression)
        impl<'a, L, R> std::ops::$op<&'a R> for Xpr<L>
        where
            R: Expression,
        {
            type Output = Xpr<$op<Self, Xpr<Term<&'a R>>>>;
            #[inline]
            fn $fun(self, other: &'a R) -> Self::Output {
                Xpr($op(self, other.as_xpr()))
            }
        }

        impl<L, R, F> Foldable<F> for $op<L, R>
        where
            L: Foldable<F>,
            R: Foldable<F>,
            F: Fold<Self> + Fold<L> + Fold<R>,
            OutputFoldable<F, L>: std::ops::$op<OutputFoldable<F, R>>,
        {
            type Output = <F as Fold<Self>>::Output;

            // ping-pongs to Fold::fold
            #[inline]
            fn fold(&self, f: &mut F) -> Self::Output {
                f.fold(self)
            }
        }

        impl<L, R, U> Fold<$op<L, R>> for U
        where
            U: Fold<L> + Fold<R>,
            L: Foldable<Self>,
            R: Foldable<Self>,
            OutputFoldable<Self, L>: std::ops::$op<OutputFoldable<Self, R>>,
        {
            type Output = <OutputFoldable<Self, L> as std::ops::$op<OutputFoldable<Self, R>>>::Output;

            #[inline]
            fn fold(&mut self, $op(l, r): &$op<L, R>) -> Self::Output {
                // ping-pongs to to the Foldable::fold impl for Xpr<T> for both arguments
                // and applies the operation
                l.fold(self) $op_token r.fold(self)
            }
        }

        /// The output of the given operation for two operands L, R where L and R are operation structs
        /// from this module, e.g. 
        /// e.g. `OutputOfXXX<Term<u32>, OutputOfMul<Term<bool>, Term<f64>>>`.
        ///
        /// This is a convenience type to not have to write out the `Xpr` enum explicitly.
        pub type $output_of<L, R> = Xpr<$op<Xpr<L>, Xpr<R>>>;

    }
}
