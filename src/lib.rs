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
//! let y = -Xpr::new(2)*Xpr::new(-21);
//! assert_eq!(y.eval(), Ok(42));
//! ```

use std::any::Any;

/// Evaluation expressions.
pub trait Eval {

    /// The output type of the expression.
    type Output: 'static;

    /// evaluate an expression. This is syntactic sugar around a transform, that unwraps any terminal
    /// to its contained type, see [`Xpr::transform`].
    /// 
    /// # Example
    /// 
    /// ```rust
    /// use xpr::*; 
    ///
    /// // An expression 
    /// let y = -Xpr::new(3)*Xpr::new(2)*Xpr::new(-7);
    /// 
    /// assert_eq!(y.eval(), Ok(42));
    /// ```
    fn eval(&self) -> Result<Self::Output,&str>;
}

mod private 
{
    use super::AnyXpr;
    use std::any::Any;

    pub trait Transform {

        type Output;

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
        fn transform_internal(&self, f: &mut dyn FnMut(&AnyXpr) -> Option<Box<dyn Any>>) -> Option<Box<dyn Any>>;

    }
}
use private::*;

impl<T> Eval for T 
where T: Transform,
      <T as Transform>::Output: 'static + Copy
{
    type Output = <T as Transform>::Output;

    fn eval(&self) -> Result<Self::Output,&str> {

        let mut evaluator = |e: &AnyXpr| -> Option<Box<dyn Any>> {
            match e.as_xpr::<Self::Output>() {
                Some(&Xpr::Terminal(x)) => Some(Box::new(x)),
                _ => None
            }
        };

        cast_optional_any(self.transform_internal(&mut evaluator)).ok_or("Error evaluation expression")
    }
}


/// A helper function to downcast a Option<Box<dyn Any>> to an Option<T>
fn cast_optional_any<T: 'static + Copy>(x: Option<Box<dyn Any>>) -> Option<T>
{
    match x {
        Some(x) => x.downcast_ref::<T>().copied(),
        _ => None
    }
}

//***********************************************************************//

/// Supported expression types.
/// 
/// `Xpr` should not be instantiated directly. Use the [`Xpr::new`] method
/// to create an `Xpr::Terminal` leaf and then apply operations to it.
pub enum Xpr<T : 'static + Copy> {
    /// A Terminal represents a leaf expression, e.g. a single value
    Terminal(T),
    /// Negation of an expression `l -> -l`
    Neg(Box<dyn Transform<Output = T>>),
    /// Multiplication of two expressions `(l,r) -> l*r`
    Mul(Box<dyn Transform<Output = T>>)
}
impl<T> Xpr<T> 
where T: 'static + Copy
{
    /// Create a new leaf expression of type [`Xpr::Terminal`]. This is the 
    /// only way expression should be instantiated.
    /// 
    /// # Examples
    /// 
    /// ```rust
    /// use xpr::*;
    /// let x = Xpr::new(3.145);
    /// let z = -x; 
    /// assert_eq!(z.eval(), Ok(-3.145));
    /// ```
    pub fn new(value: T) -> Self 
    {
        Xpr::<T>::Terminal(value)
    }

    /// type erase the output type. This is the argument accepted by the transform closures
    fn as_anyxpr(&self) -> AnyXpr {
        AnyXpr { expr: self }
    }

    /// The [`Xpr::transform`] function is a central feature of this crate. It can be used to transform and manipulate
    /// expression by traversing the expression tree made up of an expression's subexpressions. 
    /// 
    /// 
    /// It takes a mutable closure that returns an [`Option<T>`]. The expression tree is traversed
    /// recursively until the closure returns `Some(_)`, in which case the return value is passed upwards through the 
    /// expression tree, applying each operation to the value contained in the `Some` on the way.
    /// 
    /// Note that the argument to `f` is a reference to [`AnyXpr`], which is a type erased version of [`Xpr<T>`]. To properly
    /// match expressions in the body of `f`, we need to cast the argument to an `Xpr<T>` with an appropriate return type `T` 
    /// using [`AnyXpr::as_xpr`], see the examples below.
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
    /// let mut evaluator = |e: &AnyXpr| -> Option<i32> {
    ///     match e.as_xpr::<i32>() {
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
    ///     match x.as_xpr::<i32>() {
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
    /// //use xpr::*;
    /// //
    /// //let x = -Xpr::new(5)*Xpr::new(6);
    /// //let y = Xpr::new(-2)*Xpr::new(7);
    /// //
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
    pub fn transform<R: 'static + Copy>(&self, f: &mut dyn FnMut(&AnyXpr) -> Option<R>) -> Option<R>
    {
        let mut t = |e: &AnyXpr| -> Option<Box<dyn Any>> {
            match f(e) {
                Some(e) => Some(Box::new(e)),
                _ => None
            }
        };
        cast_optional_any(self.transform_internal(&mut t))
    }
}

impl<T: 'static + Copy> Transform for Xpr<T>
{
    type Output = T;

    fn transform_internal(&self, f: &mut dyn FnMut(&AnyXpr) -> Option<Box<dyn Any>>) -> Option<Box<dyn Any>>
    {
        if let Some(v) = f(&self.as_anyxpr()) { return Some(v); }
        
        match self {
            Xpr::Terminal(_) => None,
            Xpr::Mul(x) => x.transform_internal(f),
            Xpr::Neg(x) => x.transform_internal(f)
        }
    }
}

//***********************************************************************//

struct Neg<T>
{
    input: Box<dyn Transform<Output=T>>,
}
impl<T> Transform for Neg<T>
where T: 'static + Copy + std::ops::Neg
{
    type Output = <T as std::ops::Neg>::Output;

    fn transform_internal(&self, f: &mut dyn FnMut(&AnyXpr) -> Option<Box<dyn Any>>) -> Option<Box<dyn Any>>
    {
        if let Some(l) = cast_optional_any::<T>(self.input.transform_internal(f)) {
            return Some(Box::new(-l));
        }
        None
    }
}

impl<T> std::ops::Neg for Xpr<T>
where T: 'static + Copy + std::ops::Neg,
      <T as std::ops::Neg>::Output: Copy
{
    type Output = Xpr<<T as std::ops::Neg>::Output>;
    fn neg(self) -> Self::Output
    {
        Xpr::Neg( Box::new(Neg{ input: Box::new(self) } ))
    }
}

//***********************************************************************//

struct Mul<L,R>
{
    left: Box<dyn Transform<Output=L>>,
    right: Box<dyn Transform<Output=R>>
}
impl<L,R> Transform for Mul<L,R>
where L: 'static + Copy + std::ops::Mul<R>,
      R: 'static + Copy,
      <L as std::ops::Mul<R>>::Output: 'static
{
    type Output = <L as std::ops::Mul<R>>::Output;

    fn transform_internal(&self, f: &mut dyn FnMut(&AnyXpr) -> Option<Box<dyn Any>>) -> Option<Box<dyn Any>>
    {
        if let Some(l) = cast_optional_any::<L>(self.left.transform_internal(f)) {
            if let Some(r) = cast_optional_any::<R>(self.right.transform_internal(f)) {
                return Some(Box::new(l*r));
            }
        }
        None
    }
}

impl<L,R> std::ops::Mul<Xpr<R>> for Xpr<L>
where L: 'static + Copy + std::ops::Mul<R>,
      R: 'static + Copy,
      <L as std::ops::Mul<R>>::Output: Copy
{
    type Output = Xpr<<L as std::ops::Mul<R>>::Output>;
    fn mul(self, other : Xpr<R>) -> Self::Output
    {
        Xpr::Mul( Box::new(Mul{ left: Box::new(self), right: Box::new(other) } ))
    }
}

//***********************************************************************//

/// [`AnyXpr`] is a type erased version of an [`Xpr<T>`]. 
/// 
/// Its sole purpose is to serve an argument in the closures passed to [`Xpr::transform`].
/// 
/// The type erasure is needed, because the transform will need to work on any kind of 
/// expression, so it cannot have a generic parameter in it. We can't use a trait object
/// like `dyn Transform` or `dyn Eval` because then we wouldn't be able to properly match
/// an `Xpr` variant in the body of the closure of [`Xpr::transform`].
pub struct AnyXpr<'a> {
    expr: &'a dyn std::any::Any
}
impl<'a> AnyXpr<'a>
{
    /// To properly match an `AnyXpr` to an `Xpr<T>` 
    /// with its associated return type `T`, you can use the `as_xpr`
    /// to downcast to to its contained `Xpr<T>`
    /// 
    /// # Examples 
    /// 
    /// See the documentation of [`Xpr::transform`] for a usage 
    /// example within a transform.
    pub fn as_xpr<T>(&'a self) -> Option<&'a Xpr<T>>
    where T: 'static + Copy
    {
        self.expr.downcast_ref::<Xpr<T>>()
    }
}

//***********************************************************************//

#[cfg(test)] 
mod tests {
    use super::*;


    #[test]
    fn test_mul_eval_vs_transform() {
        let mut evaluator = |e: &AnyXpr| -> Option<i32> {
            match e.as_xpr::<i32>() {
                Some(&Xpr::Terminal(x)) => Some(x),
                _ => None
            }
        };
    
        let x = Xpr::new(3)*Xpr::new(2)*Xpr::new(7);
        assert_eq!(x.eval(), Ok(42));
        assert_eq!(x.transform::<i32>(&mut evaluator), Some(42))
    }

}
