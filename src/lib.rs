//! This crate allows you to use expression templates with your custom type.
//! It is inspired by [boost::yap](https://www.boost.org/doc/libs/1_74_0/doc/html/yap.html).
//! Expressions can be evaluated or manipulated using transforms, which match subexpressions,
//! and transform matched expressions, do any kind of calculations in between and return anything
//! that implements the operations of the used expression.
//!
//! # Example usage
//! ```rust
//! use xpr::*;
//!
//! // An expression
//! let y = -Xpr::Terminal(2)*Xpr::Terminal(-21);
//! assert_eq!(y.eval(), Ok(42));
//! ```

use std::any::Any;
use std::marker::PhantomData;
use std::ops;

/// A trait for evaluating expressions
pub trait Eval {
    /// evaluate an expression. This is syntactic sugar around a transform, that unwraps any terminal
    /// to its contained type, see [`Transform::transform`].
    ///
    /// # Example
    ///
    /// ```rust
    /// use xpr::*;
    ///
    /// // An expression
    /// let y = -Xpr::Terminal(3)*Xpr::Terminal(2)*Xpr::Terminal(-7);
    ///
    /// assert_eq!(y.eval(), Ok(42));
    /// ```
    fn eval<R>(&self) -> Result<R, &str>
    where
        Self: Sized,
        R: 'static + Copy;
}

/// A trait for transforming expressions
pub trait Transform: TransformInternal {
    /// The [`Transform::transform`] function is a central feature of this crate. It can be used to transform and manipulate
    /// expression by traversing the expression tree made up of an expression's subexpressions.
    ///
    ///
    /// It takes a mutable visiting closure that returns an [`Option<T>`]. The expression tree is traversed
    /// recursively until the closure returns `Some(_)`, in which case the return value is passed upwards through the
    /// expression tree, applying each operation to the value contained in the `Some` on the way.
    ///
    /// Note that the argument to the visitor closure `f` is a to [`&dyn Any`]. This way, it can be applied to any kind
    /// of expression. To properly match expressions in the body of `f`, we need to downcast the argument to an `&Xpr<T>`
    /// with an appropriate return type `T` using `downcast_ref`, see the examples below.
    ///
    /// # Examples
    ///
    /// ## A transform that returns a value
    ///
    /// This example demonstrates how an expression can be evaluated using a transform. This is taken directly from the
    /// implementation of [`Eval::eval`]. We will match any terminal, unwrap its wrapped value and pass that up the the
    /// expression tree, evaluating each subexpression along the way.
    ///
    /// ```rust
    /// use xpr::*;
    /// use std::any::Any;
    ///
    /// /// triple negation
    /// let x = ---Xpr::Terminal(42);
    ///
    /// /// the following transform evaluates an expression
    /// let mut evaluator = |e: &dyn Any| -> Option<i32> {
    ///     match e.downcast_ref::<Xpr<i32>>() {
    ///         Some(&Xpr::Terminal(x)) => Some(x),
    ///         _ => None
    ///     }
    /// };
    ///
    /// let result = x.transform::<i32>(&mut evaluator);
    ///
    /// assert_eq!(result, Some(-42));
    /// ```
    ///
    /// Note that the type annotations can be dropped if the closure is passed anonymously to the function, as can be seen in the next
    /// example
    ///
    /// ## A transform that always returns `None`
    ///
    /// In this example we are not interested in transforming the expression, we just want to do some analysis on it.
    /// Let's count the number of negations from the expression of the previous example
    ///
    /// ```rust
    /// use xpr::*;
    ///
    /// // create an expression
    /// let y = ---Xpr::Terminal(42);
    ///
    /// let mut count = 0;
    ///
    /// // traverse the expression tree and capture count mutably
    /// y.transform(&mut |x| -> Option<()> {
    ///     match x.downcast_ref::<Xpr<i32>>() {
    ///         Some(&Xpr::Neg(_)) => {
    ///             count+=1;
    ///             None
    ///         }
    ///         _ => None
    ///     }
    /// });
    ///
    /// assert_eq!(count, 3);
    /// ```
    ///
    /// We still need to directly declare the output type, because the closure never returns `Some`.
    /// In the next example, the compiler will be able to deduce the output type for us.
    ///
    /// ## A transform that returns an expression
    ///
    /// This example demonstrates how to replace a terminal expression with another expression.
    ///
    /// **To Do**
    ///
    /// ```rust
    /// use xpr::*;
    ///
    /// let x = -Xpr::Terminal(5)*Xpr::Terminal(6);
    /// let y = Xpr::Terminal(-2)*Xpr::Terminal(7);
    ///
    /// //let z = x.transform(&mut |e|{
    /// //    match e.as_xpr::<i32>() {
    /// //        // replaces 5 with 3
    /// //        Some(&Xpr::Terminal(v)) if v==5 => Some(Xpr::Terminal(3)),
    /// //        // inserts y in place of 6
    /// //        Some(&Xpr::Terminal(v)) => Some(y),
    /// //        _ => None
    /// //    }
    /// //});
    /// //
    /// //assert_eq!(z.unwrap().eval(), Ok(42));
    /// ```
    ///
    fn transform<R>(&self, f: &mut dyn FnMut(&dyn Any) -> Option<R>) -> Option<R>
    where
        R: 'static + Copy,
    {
        let mut t = |e: &dyn Any| -> Option<Box<dyn Any>> {
            match f(e) {
                Some(e) => Some(Box::new(e)),
                _ => None,
            }
        };
        cast_optional_any(self.transform_internal(&mut t))
    }
}

mod private {
    use std::any::Any;

    pub trait TransformInternal {
        /// The [`Transform::transform_internal`] function can be used to transform and manipulate
        /// expression by traversing the expression tree made up of an expression's subexpressions.
        ///
        /// The return type of [`Transform::transform`] is an [`std::any::Any`]. For now the type erasure of
        /// the return type is needed to properly traverse the tree. [`Xpr::transform`] is a more convenient version
        /// of this transform.
        ///
        /// The type erased return type is a bit misfortunate, because
        /// 1. I cannot replace an operation taking in input of type X with an output of type Y, because the operation
        ///    must downcast to an `Xpr<X>` when passing the returned value up through the expression tree.
        /// 2. It makes the code a bit awkward.
        ///
        /// It is currently needed, for the following reason:
        /// 1. I need the transform(_internal) function to be a trait method, because I want to store trait objects as operands
        ///    to expressions. This way I have shared behavior (all operands can transform recursively)
        /// 2. I need some form of type erasure of the operands of the operands.
        ///
        /// Ideally, the transform(_internal) would return `Option<T>` for a generic `T`, and we wouldn't need the extra level
        /// of indirection between [`Xpr::transform`] and [`Transform::transform_internal`], but I can't call generic trait
        /// methods on trait objects.
        ///
        fn transform_internal(
            &self,
            f: &mut dyn FnMut(&dyn Any) -> Option<Box<dyn Any>>,
        ) -> Option<Box<dyn Any>>;
    }

    /// A helper function to downcast a Option<Box<dyn Any>> to an Option<T>
    pub fn cast_optional_any<T>(x: Option<Box<dyn Any>>) -> Option<T>
    where
        T: 'static + Copy,
    {
        match x {
            Some(x) => x.downcast_ref::<T>().copied(),
            _ => None,
        }
    }
}
use private::*;

impl<T> Transform for T where T: TransformInternal {}

impl<T> Eval for T
where
    T: Transform,
{
    fn eval<R: 'static + Copy>(&self) -> Result<R, &str> {
        let mut evaluator = |e: &dyn Any| -> Option<R> {
            match e.downcast_ref::<Xpr<R>>() {
                Some(&Xpr::Terminal(x)) => Some(x),
                _ => None,
            }
        };

        self.transform(&mut evaluator)
            .ok_or("Error evaluation expression")
    }
}

//***********************************************************************//

/// Supported expression types.
///
/// Note that [`Xpr::Terminal`] is the only variant that can be instantiated
/// explicitly. It represents the leaf expressions of any expression. All
/// other expressions are created by applying the supported operations to
/// `Xpr::Terminal` instances.
///
pub enum Xpr<T>
where
    T: 'static + Copy,
{
    /// A Terminal represents a leaf expression, e.g. a single value
    Terminal(T),
    /// Negation of an expression `l -> -l`
    Neg(Box<dyn TransformInternal>),
    /// Multiplication of two expressions `(l,r) -> l*r`
    Mul(Box<dyn TransformInternal>),
}

impl<T> TransformInternal for Xpr<T>
where
    T: 'static + Copy,
{
    fn transform_internal(
        &self,
        f: &mut dyn FnMut(&dyn Any) -> Option<Box<dyn Any>>,
    ) -> Option<Box<dyn Any>> {
        if let Some(v) = f(self) {
            return Some(v);
        }

        match self {
            Xpr::Terminal(_) => None,
            Xpr::Mul(x) => x.transform_internal(f),
            Xpr::Neg(x) => x.transform_internal(f),
        }
    }
}

//***********************************************************************//

struct Neg<T> {
    input: Box<dyn TransformInternal>,
    _input: PhantomData<T>,
}
impl<T> Neg<T> {
    fn new(input: Box<dyn TransformInternal>) -> Self {
        Neg {
            input,
            _input: PhantomData::<T>,
        }
    }
}
impl<T> TransformInternal for Neg<T>
where
    T: 'static + Copy + ops::Neg,
{
    fn transform_internal(
        &self,
        f: &mut dyn FnMut(&dyn Any) -> Option<Box<dyn Any>>,
    ) -> Option<Box<dyn Any>> {
        if let Some(l) = cast_optional_any::<T>(self.input.transform_internal(f)) {
            return Some(Box::new(-l));
        }
        None
    }
}

impl<T> ops::Neg for Xpr<T>
where
    T: 'static + Copy + ops::Neg,
    <T as ops::Neg>::Output: Copy,
{
    type Output = Xpr<<T as ops::Neg>::Output>;
    fn neg(self) -> Self::Output {
        Xpr::Neg(Box::new(Neg::<T>::new(Box::new(self))))
    }
}

//***********************************************************************//

struct Mul<L, R> {
    left: Box<dyn TransformInternal>,
    right: Box<dyn TransformInternal>,
    _left: PhantomData<L>,
    _right: PhantomData<R>,
}
impl<L, R> Mul<L, R> {
    fn new(left: Box<dyn TransformInternal>, right: Box<dyn TransformInternal>) -> Self {
        Mul {
            left,
            right,
            _left: PhantomData::<L>,
            _right: PhantomData::<R>,
        }
    }
}
impl<L, R> TransformInternal for Mul<L, R>
where
    L: 'static + Copy + ops::Mul<R>,
    R: 'static + Copy,
    <L as ops::Mul<R>>::Output: 'static,
{
    fn transform_internal(
        &self,
        f: &mut dyn FnMut(&dyn Any) -> Option<Box<dyn Any>>,
    ) -> Option<Box<dyn Any>> {
        if let Some(l) = cast_optional_any::<L>(self.left.transform_internal(f)) {
            if let Some(r) = cast_optional_any::<R>(self.right.transform_internal(f)) {
                return Some(Box::new(l * r));
            }
        }
        None
    }
}

impl<L, R> ops::Mul<Xpr<R>> for Xpr<L>
where
    L: 'static + Copy + ops::Mul<R>,
    R: 'static + Copy,
    <L as ops::Mul<R>>::Output: Copy,
{
    type Output = Xpr<<L as ops::Mul<R>>::Output>;
    fn mul(self, other: Xpr<R>) -> Self::Output {
        Xpr::Mul(Box::new(Mul::<L, R>::new(Box::new(self), Box::new(other))))
    }
}

//***********************************************************************//

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mul_eval_vs_transform() {
        let mut evaluator = |e: &dyn Any| -> Option<i32> {
            match e.downcast_ref::<Xpr<i32>>() {
                Some(&Xpr::Terminal(x)) => Some(x),
                _ => None,
            }
        };

        let x = Xpr::Terminal(3) * Xpr::Terminal(2) * Xpr::Terminal(7);
        assert_eq!(x.eval(), Ok(42));
        assert_eq!(x.transform::<i32>(&mut evaluator), Some(42))
    }

    // a struct that doesn't implement all supported operations
    #[derive(Clone, Copy, Debug, PartialEq)]
    struct OnlyNeg(i32);

    impl ops::Neg for OnlyNeg {
        type Output = Self;
        fn neg(self) -> Self::Output {
            OnlyNeg(-self.0)
        }
    }

    #[test]
    fn test_neg_only() {
        let x = -Xpr::Terminal(OnlyNeg(1));
        assert_eq!(x.eval(), Ok(OnlyNeg(-1)));
    }
}
