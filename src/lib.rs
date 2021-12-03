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


/// Evaluation and Transformation of expressions.
pub trait Eval {

    /// The output type of the expression.
    type Output: 'static;

    /// The [`Eval::transform`] function is a central feature of this crate. It can be used to transform and manipulate
    /// expression by traversing the expression tree made up of an expression's subexpressions. 
    /// 
    /// The return type of [`Eval::transform`] is an [`std::any::Any`]. For now the type erasure of the return type is needed
    /// to properly traverse the tree. [`Xpr::transform`] is a more convenient version of this transform.
    /// 
    /// Examples
    /// 
    /// In the following example, we use the [`Eval::transform`] function to count the number of negations
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
    fn transform_internal(&self, f: &mut dyn FnMut(&AnyXpr) -> Option<Box<dyn Any>>) -> Option<Box<dyn Any>>;

    /// evaluate an expression. This is syntactic sugar around a transform, that unwraps any terminal
    /// to its contained type, see [`Eval::transform`] or [`Xpr::transform`].
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
fn cast_optional_any<T: 'static>(x: Option<Box<dyn Any>>) -> Option<T>
{
    match x {
        Some(x) => match x.downcast_ref::<T>() {
            Some(x) => return Some(*x),
            None => None
        },
        _ => None
    }
}

//***********************************************************************//

/// Supported expression types.
/// 
/// `Xpr` should not be instantiated directly. Use the [`Xpr::new`] method
/// to create an `Xpr::Terminal` leaf and then apply operations to it.
pub enum Xpr<T> {
    /// A Terminal represents a leaf expression, e.g. a single value
    Terminal(T),
    /// Negation of an expression `l -> -l`
    Neg(Box<dyn Eval<Output = T>>),
    /// Multiplication of two expressions `(l,r) -> l*r`
    Mul(Box<dyn Eval<Output = T>>)
}
impl<T> Xpr<T> 
where T: 'static
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

    /// This function lets you traverse an expression tree and manipulate it. It is syntactic sugar around the
    /// [`Eval::transform`] function.
    /// 
    /// It takes a mutable closure that returns an [`Option<Box<dyn Any>>`]. The expression tree is traversed
    /// recursively until the closure returns `Some(_)`, in which case the return value is passed upwards through the 
    /// expression tree, applying each operation to the returned value on the way. 
    /// 
    /// Note, that the return type of [`Eval::transform`] is an [`std::any::Any`]. This is currently a limitation of 
    /// `xpr`, that is needed to properly handle any possible type of subexpression. The input argument of the closure
    /// is an instance of [`AnyXpr`], which is a type erased [`Xpr<T>`]. To properly match expressions within the closure,
    /// we can use [`AnyXpr::as_xpr`] for downcasting.
    /// 
    /// # Examples
    /// 
    /// ## Evaluate an expression
    /// 
    /// This example demonstrates how an expression can be evaluated using a transform. This is taken directly from the
    /// implementation of [`Eval::eval`].
    /// 
    /// ```rust
    /// use xpr::*;
    /// use std::any::Any;
    /// 
    /// /// triple negation
    /// let x = ---Xpr::Terminal(42);
    /// 
    /// /// the following transform evaluates an expression
    /// let mut evaluator = |e: AnyXpr| -> Option<i32> {
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
    /// Note that the type annotations can be dropped if the closure is passed anonymously to the function.
    /// 
    /// ## Replace a terminal expression
    /// 
    /// This example demonstrates how to replace a terminal expression with another.
    /// 
    /// ```rust
    /// use xpr::*;
    /// 
    /// let x = Xpr::new(5)*Xpr::new(7);
    /// assert_eq!(x.eval(), Ok(35));
    /// 
    /// let y = x.transform(&mut |e|{
    ///     match e.as_xpr::<i32>() {
    ///         Some(&Xpr::Terminal(v)) if v==5 => Some(Xpr::Terminal(7)),
    ///         _ => None
    ///     }
    /// });
    /// ```
    /// 
    pub fn transform<R: 'static>(&self, f: &mut dyn FnMut(&AnyXpr) -> Option<R>) -> Option<R>
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

impl<T: 'static> Eval for Xpr<T>
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
    input: Box<dyn Eval<Output=T>>,
}
impl<T> Eval for Neg<T>
where T: 'static + std::ops::Neg
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
where T: 'static + std::ops::Neg
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
    left: Box<dyn Eval<Output=L>>,
    right: Box<dyn Eval<Output=R>>
}
impl<L,R> Eval for Mul<L,R>
where L: 'static + std::ops::Mul<R>,
      R: 'static,
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
where L: 'static + std::ops::Mul<R>,
      R: 'static,
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
/// It is used as an argument in the closures passed to [`Eval::transform`]
///  or [`Xpr::transform`].
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
    where T: 'static
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
