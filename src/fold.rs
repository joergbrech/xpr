//! contains the [`Fold`] and [`Foldable`] traits

/// A trait for expression manipulation. `Fold` together with [`crate::Xpr`] are at the heart of this crate.
/// [`Fold<T>::fold`] will replace any instance of type `T` in an expression tree [`crate::Xpr<U>`] with 
/// a value of type [`Fold::Output`].
/// 
/// The usage of this trait is best explained with examples.
///
/// # Examples
///
/// The following examples can also be found in the `examples` directory.
///
/// ## Evaluating expressions
///
/// We can replace all `Term<i32>` instances in an expression by their wrapped type.
/// This will effectively evaluate the expression.
///
/// ```
/// use xpr::{ops::Term, Fold, Foldable, Xpr};
///
/// struct Evaluator;
///
/// /// evaluates an expression of i32 terminals
/// impl Fold<Term<i32>> for Evaluator {
///
///     type Output = i32;
///
///     /// replaces Terminal values with their wrapped type
///     fn fold(&mut self, Term(x): &Term<i32>) -> i32 {
///         *x
///     }
/// }
///
/// // create a new expression representing a chained addition
/// let x = Xpr::new(10) + Xpr::new(15) + Xpr::new(17);
///
/// // use the Evaluator to evaluate the expression
/// let res = Evaluator.fold(&x);
/// assert_eq!(res, 42);
///
/// // alternatively, we can pass a mutable reference to an
/// // Evalutor instance to the expression.
/// assert_eq!(res, x.fold(&mut Evaluator {}));
///
/// // note that `Xpr::eval` is syntactic sugar around a similar generic
/// // Evaluator struct
/// assert_eq!(res, x.eval());
/// ```
///
/// ## Substituting expressions
///
/// We can replace terminals by other expressions
///
/// ```
/// use xpr::{
///     ops::{OutputOfAdd, Term},
///     Fold, Xpr,
/// };
///
/// struct Substitution;
///
/// impl Fold<Term<i32>> for Substitution {
///    
///     type Output = OutputOfAdd<Term<i32>, Term<i32>>;
///
///     // replaces i32 terminals with an Add expression
///     // that returns the same value
///     fn fold(&mut self, &Term(x): &Term<i32>) -> Self::Output {
///         Xpr::new(x - 42) + Xpr::new(42)
///     }
/// }
///
/// let x = Xpr::new(10) + Xpr::new(15) + Xpr::new(17);
///
/// assert_eq!(x.eval(), 42);
///
/// // let's use the Substitution folder
/// let x = Substitution.fold(&x);
///
/// assert_eq!(x.eval(), 42);
///
/// ```
///  
/// ## Improving performance: Avoiding temporaries
///
/// This is the test-piece for expression templates and the classical use case scenario. Image we are using
/// a linear algebra library that supplies a `Vec` type and implements [`std::ops::Add`] for it. Then, we could
/// write a chained addition of potentially large vectors `x1`, `x2` and `x3`.
///
/// `
/// let x = x1 + x2 + x3;
/// `
///
/// Internally, the additon of `x2` and `x3` would yield a temporary vector containing the result of the addition.
/// This temporary vector is than added to `x1` yielding yet another temporary vector, which is moved into `x`. The
/// allocation of these two temporary vectors can be avoided using expression templates.
///
/// Let's pretend we want to write our own linear algebra library and let's start just with a statically sized
/// vector type.
///
/// ```
/// use xpr::{ops::Term, Expression, Fold, Xpr};
/// use std::ops::{Index, Range};
///
/// // This is our statically sized vector type
/// #[derive(Debug)]
/// struct Vec<const N: usize>(Box<[f64; N]>);
///
/// impl<const N: usize> Vec<{ N }> {
///     #[inline]
///     fn new(array: [f64; N]) -> Self {
///         Self(Box::new(array))
///     }
/// }
///
/// // a convenience trait for cnverting Vec instances to xpr terminals.
/// // this lets us call as_xpr and into_xpr on Vec instances.
/// impl<const N: usize> Expression for Vec<N> {}
///
/// // now lets actually implement conversion from an Xpr<T> expression to Vec
///
/// // We need the folder IthElement to depend on the lifetime 'a, so we
/// // wrap an std::marker::PhantomData<&'a ()>.
/// struct IthElement<'a, const N: usize>(usize, std::marker::PhantomData<&'a ()>);
///
/// // IthElement will match all Terminals wrapping a reference to Vec and
/// // return it's i-th element.
/// impl<'a, const N: usize> Fold<Term<&'a Vec<{ N }>>> for IthElement<'a, { N }> {
///
///     type Output = f64;
///
///     // extracts the i-th element of a vector terminal
///     #[inline]
///     fn fold(&mut self, Term(v): &Term<&'a Vec<{ N }>>) -> f64 {
///         v.0[self.0]
///     }
/// }
///
/// // convert any Xpr<T> to Vec
/// impl<'a, T, const N: usize> From<Xpr<T>> for Vec<{ N }>
/// where
///     IthElement<'a, N>: Fold<Xpr<T>, Output = f64>,
/// {
///     // conversion from a vector expression to a Vec instance
///     #[inline]
///     fn from(expr: Xpr<T>) -> Self {
///         // scary unsafe uninitialized array
///         let mut ret = Vec::new(unsafe { std::mem::MaybeUninit::uninit().assume_init() });
///
///         // apply the operations in the vector expression element-wise
///         for (i, e) in ret.0.iter_mut().enumerate() {
///             *e = IthElement(i, std::marker::PhantomData).fold(&expr);
///         }
///         ret
///     }
/// }
///
/// // Now let's take it for a spin!
///
/// // Create a couple of vectors
/// let x1 = Vec::new([0.6; 5000]);
/// let x2 = Vec::new([1.0; 5000]);
/// let x3 = Vec::new([40.0; 5000]);
/// let x4 = Vec::new([100.0; 5000]);
/// let x5 = Vec::new([3000.0; 5000]);
///
/// // A chained addition without any Vec temporaries!
/// let v = Vec::from(x1.as_xpr() + &x2 + &x3 + &x4 + &x5);
/// assert_eq!(v.0[0], 3141.6);
/// ```
///
pub trait Fold<T: ?Sized> {
    // implement the [fold pattern](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html)

    /// The output of [`Fold::fold`], `Self` will replace all instances of `T` in an expression by values of this type.
    type Output;

    /// perform the replacement
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
/// concrete expression types, e.g. [`crate::ops::Add`]. These methods can in turn recurse to wrapped internal
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
