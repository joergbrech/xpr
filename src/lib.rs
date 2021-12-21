//! This crate allows you to use expression templates with your custom type.
//! It is inspired by the C++ expression template library [boost::yap](https://www.boost.org/doc/libs/1_74_0/doc/html/yap.html).
//!
//! To use `xpr`, all you need to do is wrap all input variables to an expression in calls to `Xpr::new` and apply any supported
//! operation to them. The result will be an object representing the operations you performed. Any possible chain of operations
//! will be represented by their own specific type.
//!
//! ## Usage
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
//! In the above example, the type of `x` is `Xpr<Add<(Xpr<Term<{integer}>>, Xpr<Term<{integer}>>)>>`[^note].
//! It is a type representing the addition of two integers. To actually evaulate the expression, we call `x.eval()`.
//! 
//! [^note]: All crate and nested module names have been omitted in the type for better readability.
//!
//! In addition to lazy evaluation, you can manipulate expressions using the [`Fold`] trait. A struct implementing `Fold`
//! can replace terminals *(leaf expressions)* and perform operations along the way. It can be stateful or a
//! zero-sized type, depending on your needs.
//!
//! ```
//! use xpr::{Xpr, Fold, ops::Term};
//!
//! struct Fortytwoify;
//!
//! impl Fold<Term<f64>> for Fortytwoify {
//!
//!     // We will replace these terminals by terminal expressions wrapping f64 values
//!     type Output = Xpr<Term<f64>>;
//!
//!     // replaces terminals with terminals wrapping the value 42.
//!     fn fold(&mut self, _: &Term<f64>) -> Self::Output {
//!         Xpr::new(42.)
//!     }
//! }
//!
//! // create an expression
//! let x = -Xpr::new(2.)/Xpr::new(5.);
//!
//! // fortitwoify the expression
//! let y = Fortytwoify.fold(&x);
//!
//! // lazily evaluate the expression
//! assert_eq!(y.eval(), -1.);
//! ```
//!
//! Refer to the documentation of [`Fold`] for more useful examples.
//! 
//! ## Limitations
//!
//! Current limitiations of the library are 
//!  - that only expression holding terminals of the same type can be transformed using [`Fold`],
//!  - that terminals are the only expressions that can be transformed using [`Fold`]. 
//! 
//! As a consequence, **we can not mix e.g. scalars, vectors and matrices in expressions**, which is 
//! a major limitation.
//! 
//! These restrictions could easily be lifted, once Rust supports
//! [specialization](https://github.com/rust-lang/rust/issues/31844), which is probably not any
//! time soon.
//! 
//! ## Performance
//! 
//! xpr allows us to write arithmetic operations like we would expect and hide away ugly implementation
//! details like optimizations, which the user should neither have to worry about, nor interfere with.
//! 
//! What is the overhead of this? Spoiler alert: **xpr provides zero-cost abstraction!**
//! 
//! Similarly to the 
//! [analysis in the boost::yap documentation](https://www.boost.org/doc/libs/1_71_0/doc/html/boost_yap/manual.html#boost_yap.manual.object_code),
//! we can compare the assembly code of an xpr implementation to a native implementation.
//! The following code introduces two functions, `eval_native` and `eval_as_xpr`, were the former performs some arithmetic calculation directly and the latter creates an xpr 
//! expression and evaluates it.
//! 
//! ```
//! use xpr::Xpr;
//!
//! #[derive(Debug, Copy, Clone)]
//! struct Num(f64);
//!
//! impl std::ops::Add for Num {
//!     type Output = Num;
//!     #[inline]
//!     fn add(self, other: Num) -> Num {
//!         Num(self.0 + other.0)
//!     }
//! }
//!
//! impl std::ops::Mul for Num {
//!     type Output = Num;
//!     #[inline]
//!     fn mul(self, other: Num) -> Num {
//!         Num(self.0 * other.0)
//!     }
//! }
//!
//! fn eval_native(a: Num, x: Num, y: Num) -> Num {
//!     (a * x + y) * (a * x + y) + (a * x + y) +
//!     (a * x + y) * (a * x + y) + (a * x + y) +
//!     (a * x + y) * (a * x + y) + (a * x + y) +
//!     (a * x + y) * (a * x + y) + (a * x + y)
//! }
//!
//! fn eval_as_xpr(a: Num, x: Num, y: Num) -> Num {
//!     let a = Xpr::new(a);
//!     let x = Xpr::new(x);
//!     let y = Xpr::new(y); 
//!     (
//!         (a * x + y) * (a * x + y) + (a * x + y) +
//!         (a * x + y) * (a * x + y) + (a * x + y) +
//!         (a * x + y) * (a * x + y) + (a * x + y) +
//!         (a * x + y) * (a * x + y) + (a * x + y)
//!     ).eval()
//! }
//!
//! fn main() {
//!     let a = Num(1.);
//!     let x = Num(2.);
//!     let y = Num(3.);
//!     // println!("{:?}", eval_as_xpr(a, x, y));
//!     println!("{:?}", eval_native(a, x, y));
//! }
//! ```
//! We can look at the assembly code after optimization by building in release mode and using the `--emit=asm` option for `rustc`.
//! If we comment the last line in the main function and uncomment the second to last line, we will get the exact same assembly. This 
//! does not change if you add more operations to the expression *(You might have to add `#![recursion_limit = "256"]` to the top 
//! of the file)*.
//! 
//! That being said, this analysis only checks a special case using simple data types and only evaluation. The performance still needs to
//! be tested for more complex data types, expressions and fold operations.
//! 

#[macro_use]
mod ops_macros;
mod foldable;

pub mod ops;

use std::marker::PhantomData;

use crate::foldable::{Foldable, OutputFoldable};
use crate::ops::Term;

/// An expression. `Xpr` together with the [`Fold`] trait are at the heart of this crate.
///
/// The nested type is a specific struct representing the operation, e.g. [`ops::Add`].
///
/// `Xpr` instances should be instantiated via the [`Xpr::new`] method, which creates
/// `Xpr<ops::Term>` leaf expressions. The other variants are constructed from them by
/// applying operations to the leaf expressions.
///
/// ```
/// use xpr::*;
/// let x = Xpr::new(5);
/// let y = Xpr::new(1);
/// let z = x * y;
/// //type of z is xpr::Xpr<xpr::ops::Mul<xpr::Xpr<xpr::ops::Term<{integer}>>, xpr::Xpr<xpr::ops::Term<{integer}>>>>
/// ```
#[derive(Debug, Copy, Clone)]
pub struct Xpr<T>(T);

//In a perfect world, this would be  an enum. But this only makes sense as soon as
// enum variants are promoted as first class types, so that we have per variant impls and irefutabl
// patterns in fn args, see also https://github.com/rust-lang/rfcs/pull/2593

impl<T> Xpr<Term<T>> {
    /// creates a new  leaf expression.
    #[inline]
    pub const fn new(t: T) -> Self {
        Self(Term(t))
    }
}

impl<T, F> Foldable<F> for Xpr<T>
where
    T: Foldable<F>,
    F: Fold<Self> + Fold<T>,
{
    type Output = <F as Fold<Self>>::Output;

    // ping-pongs to Fold::fold
    #[inline]
    fn fold(&self, f: &mut F) -> Self::Output {
        f.fold(self)
    }
}

impl<T, U> Fold<Xpr<T>> for U
where
    U: Fold<T>,
    T: Foldable<Self>,
{
    type Output = OutputFoldable<Self, T>;

    #[inline]
    fn fold(&mut self, Xpr(t): &Xpr<T>) -> <T as Foldable<Self>>::Output {
        // ping-pong to the Foldable::fold impl for Term<T> and Add<L,R>
        t.fold(self)
    }
}

/// internal type for evaluating expression. It folds each terminal in an expression tree to its wrapped
/// type and performs the operations on its upwards traversal through the tree, thus evaluating the expression.
#[doc(hidden)]
pub struct Evaluator<T>(pub PhantomData<T>);

impl<T> Fold<Term<T>> for Evaluator<T>
where
    T: Clone,
{
    type Output = T;
    /// replaces Terminal values with their wrapped type
    #[inline]
    fn fold(&mut self, Term(x): &Term<T>) -> T {
        x.clone()
    }
}

impl<U> Xpr<U> {
    /// evaluates the expression by unwrapping all terminals and applying the operations
    /// in the expression. It is synactic sugar for folding the expression with [`Evaluator`].
    #[inline]
    pub fn eval<T>(&self) -> foldable::OutputFoldable<Evaluator<T>, Self>
    where
        T: Clone,
        U: Foldable<Evaluator<T>>,
        Evaluator<T>: Fold<U> + Fold<Self>,
    {
        Evaluator(PhantomData::<T>).fold(self)
    }
}

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
/// use xpr::{ops::Term, Fold, Xpr};
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
/// let x = Xpr::new(20) / Xpr::new(2) + Xpr::new(3) * Xpr::new(5) + Xpr::new(23) - Xpr::new(6);
///
/// // use the Evaluator to evaluate the expression
/// let res = Evaluator.fold(&x);
/// assert_eq!(res, 42);
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
///     ops::{OutputOfSub, Term},
///     Fold, Xpr,
/// };
///
/// struct Substitution;
///
/// impl Fold<Term<i32>> for Substitution {
///    
///     type Output = OutputOfSub<Term<i32>, Term<i32>>;
///
///     // replaces i32 terminals with a Sub expression
///     // that returns the same value
///     fn fold(&mut self, &Term(x): &Term<i32>) -> Self::Output {
///         Xpr::new(x + 42) - Xpr::new(42)
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
/// Let's pretend we want to write our own linear algebra library.
///
/// ```
/// use xpr::{ops::Term, Expression, Fold, Xpr};
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
/// struct IthElement<const N: usize>(usize);
///
/// // IthElement will match all Terminals wrapping a Vec and return it's i-th element.
/// impl<const N: usize> Fold<Term<Vec<{ N }>>> for IthElement<{ N }> {
///
///     type Output = f64;
///
///     // extracts the i-th element of a vector terminal
///     #[inline]
///     fn fold(&mut self, Term(v): &Term<Vec<{ N }>>) -> f64 {
///         v.0[self.0]
///     }
/// }
///
/// // convert any Xpr<T> to Vec
/// impl<T, const N: usize> From<Xpr<T>> for Vec<{ N }>
/// where
///     IthElement<N>: Fold<Xpr<T>, Output = f64>,
/// {
///     // conversion from a vector expression to a Vec instance
///     #[inline]
///     fn from(expr: Xpr<T>) -> Self {
///         // scary unsafe uninitialized array
///         let mut ret = Vec::new(unsafe { std::mem::MaybeUninit::uninit().assume_init() });
///
///         // apply the operations in the vector expression element-wise
///         for (i, e) in ret.0.iter_mut().enumerate() {
///             *e = IthElement(i).fold(&expr);
///         }
///         ret
///     }
/// }
///
/// // Now let's take it for a spin!
///
/// // Create a couple of vector expressions
/// let x1 = Vec::new([0.6; 5000]).into_xpr();
/// let x2 = Vec::new([1.0; 5000]).into_xpr();
/// let x3 = Vec::new([40.0; 5000]).into_xpr();
/// let x4 = Vec::new([100.0; 5000]).into_xpr();
/// let x5 = Vec::new([3000.0; 5000]).into_xpr();
///
/// // A chained addition without any Vec temporaries!
/// let v = Vec::from(x1 + x2 + x3 + x4 + x5);
/// assert_eq!(v.0[0], 3141.6);
/// ```
///
/// Granted, the example is a bit of an oversimplification because the addition consumes
/// its summands and it would have been possible to write a fairly performant vector addition
/// with the same functionality based on move semantics. But a sum that consumes its summands
/// is something you usually don't want. It takes only a little bit of tweaking to get the
/// example to work on terminals that wrap references to vectors that will not be consumed.
/// The tweaked example can be found in the examples subdirectory of the repository.
///
pub trait Fold<T: ?Sized> {
    // implement the [fold pattern](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html)

    /// The output of [`Fold::fold`], `Self` will replace all instances of `T` in an expression by values of this type.
    type Output;

    /// perform the replacement
    fn fold(&mut self, _: &T) -> Self::Output;
}

/// A trait for converting an instance into xpr terminals
pub trait Expression {
    /// Wrap a reference of self in an Xpr terminal
    #[inline]
    fn as_xpr(&self) -> Xpr<Term<&Self>>
    where
        Self: Sized,
    {
        Xpr::new(self)
    }

    /// Consume self and return as Xpr terminal
    #[inline]
    fn into_xpr(self) -> Xpr<Term<Self>>
    where
        Self: Sized,
    {
        Xpr::new(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval() {
        let x = Xpr::new(1) + Xpr::new(5);
        assert_eq!(x.eval(), 6);
        assert_eq!(x.eval(), x.fold(&mut Evaluator(PhantomData::<i32>)));
        assert_eq!(x.eval(), Evaluator(PhantomData::<i32>).fold(&x));
    }

    struct Num(i32);
    impl std::ops::Add<i32> for Num {
        type Output = i32;
        fn add(self, other: i32) -> Self::Output {
            self.0 + other
        }
    }

    // #[test]
    // fn test_eval_different_terminal_types()
    // {
    //     // let x = Xpr::new(Num(2)) + Xpr::new(1);
    //     // assert_eq!(x.eval(), 3);
    //     assert!(false);
    // }
}
