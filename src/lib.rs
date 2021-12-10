//! This crate allows you to use expression templates with your custom type.
//! It is inspired by [boost::yap](https://www.boost.org/doc/libs/1_74_0/doc/html/yap.html).
//! Expressions can be evaluated or manipulated using transforms, which match subexpressions,
//! and transform matched expressions, do any kind of calculations in between and return anything
//! that implements the operations of the used expression.
//!

/// An enum representing expressions.
/// Term represents a terminal leaf expression.
/// The nested type will be a specific type
/// representing the operation
pub enum Xpr<T> {
    Term(T),
    Add(T),
}

/// A specific type representing a terminal leaf
/// T can be anything.
pub struct Term<T>(T);

/// A new expression is always a leaf expression
impl<T> Xpr<Term<T>> {
    pub fn new(t: T) -> Self {
        Xpr::Term(Term(t))
    }
}

/// A specific type representing an addition.
/// T will always be a tuple type (Xpr<L>,Xpr<R>)
pub struct Add<T>(T);

// implement addition for Xpr<T> expressions
impl<L, R> std::ops::Add<Xpr<R>> for Xpr<L> {
    type Output = Xpr<Add<(Xpr<L>, Xpr<R>)>>;
    fn add(self, other: Xpr<R>) -> Self::Output {
        Xpr::Add(Add((self, other)))
    }
}

// The types get a bit yucky...let's add some shorthand notation
pub type OutputFoldable<F, T> = <T as Foldable<F>>::Output;
pub type OutputFoldableAdd<F, L, R> =
    <OutputFoldable<F, L> as std::ops::Add<OutputFoldable<F, R>>>::Output;

// implement the [fold pattern](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html)
pub trait Fold {
    fn fold_term<T>(&mut self, x: Term<T>) -> Xpr<Term<T>> {
        println!("A terminal!");
        Xpr::Term(x)
    }

    fn fold_add<L, R>(&mut self, x: Add<(L, R)>) -> OutputFoldableAdd<Self, L, R>
    where
        L: Foldable<Self>,
        R: Foldable<Self>,
        OutputFoldable<Self, L>: std::ops::Add<OutputFoldable<Self, R>>,
    {
        println!("An addition");

        // ping-pongs to to the Foldable::fold impl for Xpr<T> for both arguments
        // and applies the operation +
        (x.0 .0).fold(self) + (x.0 .1).fold(self)
    }

    fn fold<T>(&mut self, x: Xpr<T>) -> <T as Foldable<Self>>::Output
    where
        T: Foldable<Self>,
    {
        // ping-pong to the Foldable::fold impl for Term<T> and Add<L,R>
        match x {
            Xpr::Term(x) => x.fold(self),
            Xpr::Add(x) => x.fold(self),
        }
    }
}

mod private {
    use super::*;

    pub trait Sealed<F: ?Sized> {}

    impl<F, T> Sealed<F> for T
    where
        T: Foldable<F>,
        F: Fold + ?Sized,
    {
    }
}

/// A ping-pong trait needed to implement the fold pattern to recurse the generic expressionin Xpr:
///
/// The methods in Fold will unwrap an explicit `Xpr<T>` containing a generic Foldable type. It will
/// forward the nested generic element to the methods in Foldable, which will expand the generic
/// type to the explicit type of the expression.
///
/// The Foldable implementations will then ping-pong the call back to the methods in Fold that handle
/// concrete types. These methods can in turn recurse to the wrapped internal generic Foldable type
/// by ping-ponging back to the Foldable trait, which will expand the ....
///
/// The ping-pong recursion ends when we hit a leaf expression in Fold::fold_term, which will not
/// trigger a recursion to a nested foldable type.
#[doc(hidden)]
pub trait Foldable<F>: private::Sealed<F>
where
    F: Fold + ?Sized,
{
    type Output;
    fn fold(self, _: &mut F) -> Self::Output;
}

impl<T, F> Foldable<F> for Xpr<T>
where
    T: Foldable<F>,
    F: Fold,
{
    type Output = OutputFoldable<F, T>;

    // ping-pongs to Fold::fold
    fn fold(self, f: &mut F) -> Self::Output {
        f.fold(self)
    }
}

impl<T, F> Foldable<F> for Term<T>
where
    F: Fold,
{
    type Output = Xpr<Term<T>>;

    // ping-pongs to Fold::fold
    fn fold(self, f: &mut F) -> Self::Output {
        f.fold_term(self)
    }
}

impl<L, R, F> Foldable<F> for Add<(L, R)>
where
    L: Foldable<F>,
    R: Foldable<F>,
    F: Fold,
    OutputFoldable<F, L>: std::ops::Add<OutputFoldable<F, R>>,
{
    type Output = OutputFoldableAdd<F, L, R>;

    // ping-pongs to Fold::fold
    fn fold(self, f: &mut F) -> Self::Output {
        f.fold_add(self)
    }
}

#[cfg(test)]
mod tests {
    // use super::*;

    // struct Evaluator;
    // impl Fold for Evaluator {
    //     type Output = i32;
    //     fn fold<i32>(&mut self, e: &Xpr<i32>) -> Option<Self::Output> {
    //         match e {
    //             Xpr::Term(x) => { Some(x) },
    //             _ => None
    //         }
    //     }
    // }

    // #[test]
    // fn test() {

    // }
}
