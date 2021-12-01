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
use std::marker::PhantomData;
use std::ops::{Neg, Mul};


/// The enum `Xpr` represents all supported expressions. It is parametrized by an 
/// output type O, as well as the outputs types of all operands
#[derive(Debug)]
pub enum Xpr<O,L,R> 
{
    /// represents a leaf expression of type O
    Terminal(O),
    /// represents negation: o = -l
    Neg(UnaryOp<O,L>),
    /// represents multiplication: o = l*r
    Mul(BinaryOp<O,L,R>)
}

impl<O,L,R> Xpr<O,L,R>
where O: 'static,
      L: 'static,
      R: 'static
{
    /// evaluates the expression
    pub fn eval(&self) -> Result<O,&str> 
    where O: core::fmt::Debug + Copy + Neg<Output = O> + Mul<Output = O>  // <-- To Do: Does this even make any sense?
    {
        self.transform(& mut |x : Box<AnyXpr> | {
            match x.as_xpr::<O,Void,Void>() {
                Some(&Xpr::Terminal(v)) => Some(v),
                _ => None
            }
        }).ok_or("Error evaluating expression")
    }

    pub fn transform<F,T>(&self, f: &mut F) -> Option<T>
    where F: FnMut(Box<AnyXpr>) -> Option<T>,
          T: Mul<Output = T> + Neg<Output = T>
    {
        // CASE 1/3: Match! return f(self)
        if let Some(v) = f(Box::new(self.as_anyxpr())) { return Some(v); };
        
        match self {
            // CASE 2/3: We have reached a leaf-expression, no match!
            Xpr::Terminal(_) => None, 
            // CASE 3/3: Recurse and apply operation to result
            Xpr::Neg(op) => { 
                //l.as_xpr::<L,R>().expect("Shit").transform(f).map(|l| -l) 
                op.transform(f).map(|l| -l)
            },
            Xpr::Mul(op) => { 
                op.transform(f).map(|(l,r)| l*r)
            }
        }
    }

    fn as_subxpr<'a>(&'a self) -> SubXprRef<'a, O>
    {
        SubXprRef { expr: Box::new(self), phantom: PhantomData }
    }

    fn to_subxpr(self) -> SubXpr<O>
    {
        SubXpr { expr: Box::new(self), phantom: PhantomData }
    }

    fn as_anyxpr(&self) -> AnyXpr {
        AnyXpr { expr: self }
    }
}

/// A zero sized type. This can be replaced by !, once stabilized
#[derive(Debug)]
pub enum Void {}

impl<O> Xpr<O,Void,Void>
{
    /// A factory function to create a terminal without specifying the two 
    /// arbitrary operand types of `Xpr`
    pub fn new(o: O) -> Self {
        Xpr::Terminal(o)
    }
}

impl<I,X,Y,O> Neg for Xpr<I,X,Y> 
where X: 'static,
      Y: 'static,
      I: 'static + Neg<Output = O>
{
    type Output = Xpr<O,I,Void>;

    fn neg(self) -> Self::Output {
        Xpr::Neg(UnaryOp::new(Box::new(self.to_subxpr())))
    }
}

impl<R,A,B,L,X,Y> Mul<Xpr<R,A,B>> for Xpr<L,X,Y>
where R: 'static,
      A: 'static,
      B: 'static,
      L: 'static + Mul<R>,
      X: 'static,
      Y: 'static
{
    type Output = Xpr<<L as Mul<R>>::Output,L,R>;

    fn mul(self, other: Xpr<R,A,B>) -> Self::Output  {
        Xpr::Mul(BinaryOp::new(Box::new(self.to_subxpr()), Box::new(other.to_subxpr())))
    }
}

/// The `UnaryOp` represents any unary operation that takes an input parameter
/// of type I and returns a O
#[derive(Debug)]
pub struct UnaryOp<O,I>{
    input: Box::<SubXpr<I>>,
    phantom: PhantomData<O>
}
impl<O,I> UnaryOp<O,I> 
where I: 'static
{
    fn new(input : Box<SubXpr<I>>) -> Self
    {
        UnaryOp{ input, phantom: PhantomData }
    }

    fn transform<F,R>(&self, mut f: F) -> Option<R>
    where F: FnMut(Box<AnyXpr>) -> Option<R>
    {
        f(Box::new(self.input.as_anyxpr()))
    }
}


/// The `BinaryOp` represents any binary operation that takes input parameters
/// of type Expr<L> and Expr<R> and returns a O
#[derive(Debug)]
pub struct BinaryOp<O,L,R> {
    left: Box::<SubXpr<L>>,
    right: Box::<SubXpr<R>>,
    phantom: PhantomData<O>
}
impl<O,L,R> BinaryOp<O,L,R> 
where L: 'static,
      R: 'static
{

    fn new(left : Box<SubXpr<L>>, right: Box<SubXpr<R>>) -> Self {
        BinaryOp { left, right, phantom: PhantomData }
    }

    fn transform<F,X>(&self, mut f: F) -> Option<(X,X)>
    where F: FnMut(Box<AnyXpr>) -> Option<X>
    {
        match (f(Box::new(self.left.as_anyxpr())), f(Box::new(self.right.as_anyxpr()))) 
        {
            (Some(l), Some(r)) => Some((l,r)),
            _ => None
        }
    }
}

/// This represents an expression characterized only by its
/// output type. It must be generated from an `Xpr` instance
/// and allows downcasting
#[derive(Debug)]
pub struct SubXpr<O> {
    expr: Box<dyn Any>,
    phantom: PhantomData<O>
}
impl<O> SubXpr<O>
where O: 'static
{

    /// given two operand types L and R, this function downcasts
    /// an `SubXpr<O>` instance to a reference of its contained `Xpr<O,L,R>`
    fn as_xpr<'a,L,R>(&'a self) -> Option<&'a Xpr<O,L,R>>
    where L: 'static,
          R: 'static
    {
        self.expr.downcast_ref::<Xpr<O,L,R>>()
    }

    fn as_anyxpr(&self) -> AnyXpr {
        AnyXpr { expr: &*self.expr }
    }
}

/// This represents an expression characterized only by its
/// output type. It must be generated from an `Xpr` instance
/// and allows downcasting
struct SubXprRef<'a,O> {
    expr: Box<&'a dyn Any>,
    phantom: PhantomData<O>
}
impl<'a, O> SubXprRef<'a, O>
where O: 'static
{

    /// given two operand types L and R, this function downcasts
    /// an instance `Expr<O>` to a reference of its contained `Xpr<O,L,R>`
    fn as_xpr<L,R>(&'a self) -> Option<&'a Xpr<O,L,R>>
    where L: 'static,
          R: 'static
    {
        self.expr.downcast_ref::<Xpr<O,L,R>>()
    }

    fn as_anyxpr(&self) -> AnyXpr {
        AnyXpr { expr: *self.expr }
    }
}

/// This represents any expression where both the output and 
/// operand types have been erased. It must be generated from 
/// an `Expr<O>` instance and allows downcasting
pub struct AnyXpr<'a> {
    expr: &'a dyn Any
}
impl<'a> AnyXpr<'a>
{
    /// given an output type O and two operand types
    /// L and R, this function downcasts an instance
    /// `Expression` to its contained `Xpr<O,L,R>`
    fn as_xpr<O,L,R>(&self) -> Option<&Xpr<O,L,R>>
    where O: 'static,
          L: 'static,
          R: 'static
    {
        self.expr.downcast_ref::<Xpr<O,L,R>>()
    }
}

#[cfg(test)] 
mod tests {
    use super::*;
    use std::any;

    fn type_name_of<T>(_: &T) -> &str {
        any::type_name::<T>()
    }

    #[test]
    fn terminal_cast_xpr_subxpr() {

        let x = Xpr::new(42);
        assert_eq!(type_name_of(&x), any::type_name::<Xpr<i32,Void,Void>>());

        // as_subxpr borrows from x and returns SubXprRef
        let y = x.as_subxpr();
        assert_eq!(type_name_of(&y), any::type_name::<SubXprRef<i32>>());
        
        // to_subxpr consumes x and returns SubXpr
        let x = x.to_subxpr();
        assert_eq!(type_name_of(&x), any::type_name::<SubXpr<i32>>());

        // as_xpr downcasts and returns Option<Xpr<...>>
        let y = x.as_xpr::<Void,Void>().unwrap();
        assert_eq!(type_name_of(&y), any::type_name::<&Xpr<i32,Void,Void>>());

    }

    #[test]
    fn terminal_cast_incremental_consumed() {
        let x = Xpr::new(42);
        let y = x.to_subxpr();
        let z = y.as_anyxpr();
        let w = z.as_xpr::<i32, Void, Void>().unwrap();
        assert_eq!(type_name_of(&w), any::type_name::<&Xpr<i32,Void,Void>>());
        assert!(matches!(w, Xpr::<i32,Void,Void>::Terminal(m) if *m==42 ));
    }

    #[test]
    fn terminal_cast_incremental_referenced() {
        let x = Xpr::new(42);
        let y = x.as_subxpr();
        let z = y.as_anyxpr();
        let w = z.as_xpr::<i32, Void, Void>().unwrap();
        assert_eq!(type_name_of(&w), any::type_name::<&Xpr<i32,Void,Void>>());
        assert!(matches!(w, Xpr::<i32,Void,Void>::Terminal(m) if *m==42 ));
    }

    #[test]
    fn terminal_cast_xpr_anyxpr() {

        let x = Xpr::new(42);

        // as_anyxpr returns AnyXpr
        let y = x.as_anyxpr();
        assert_eq!(type_name_of(&y), any::type_name::<AnyXpr>());


        let y = y.as_xpr::<i32,Void,Void>().unwrap();
        assert_eq!(type_name_of(&y), any::type_name::<&Xpr<i32,Void,Void>>());

    }

    #[test]
    fn terminal_cast_subxpr_anyxpr(){

        let x = Xpr::new(42).to_subxpr();

        // as_anyxpr returns AnyXpr
        let x = x.as_anyxpr();
        assert_eq!(type_name_of(&x), any::type_name::<AnyXpr>());
    }
    
    #[test]
    fn terminal_eval_vs_transform() {
        
        let mut evaluator = |e : Box<AnyXpr> | {
            match e.as_xpr::<i32,Void,Void>() {
                Some(&Xpr::Terminal(v)) => Some(v),
                _ => None
            }
        };

        let x = Xpr::new(2);
        assert_eq!(x.transform(&mut evaluator), Some(2));
        assert_eq!(x.eval(), Ok(2));
    }

    #[test]
    fn mul_eval_vs_transform() {

        let mut evaluator = |e : Box<AnyXpr> | {
            match e.as_xpr::<i32,Void,Void>() {
                Some(&Xpr::Terminal(v)) => Some(v),
                _ => None
            }
        };

        let x = Xpr::new(2)*Xpr::new(21);
        assert_eq!(x.transform(&mut evaluator), Some(42));
        assert_eq!(x.eval(), Ok(42));
    }

    #[test]
    fn neg_eval_vs_transform() {
        let mut evaluator = |e : Box<AnyXpr> | {
            match e.as_xpr::<i32,Void,Void>() {
                Some(&Xpr::Terminal(v)) => Some(v),
                _ => None
            }
        };

        let x = -Xpr::new(2);
        assert_eq!(x.transform(&mut evaluator), Some(-2));
        assert_eq!(x.eval(), Ok(-2));
    }
}
