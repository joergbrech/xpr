#![feature(generic_associated_types)]

use xpr::{ops::*, *};

/// evaluates an expression
struct Evaluator;
impl Fold for Evaluator {
    type TerminalType<T> = T;
    type Output<T> = T;
    /// replaces Terminal i32 values by their wrapped type
    fn fold_term<T>(&mut self, x: &Term<T>) -> T
    where
        T: Copy,
    {
        x.0
    }
}

// make every i32 terminal wrap "42"
struct Fortytwoify;
impl Fold for Fortytwoify {
    type TerminalType<T> = T;
    type Output<T> = Xpr<Term<i32>>;
    // replaces i32 terminals with other terminals of the same type
    fn fold_term<T>(&mut self, _: &Term<T>) -> Self::Output<T> {
        Xpr::new(42)
    }
}

#[derive(Debug, Copy, Clone)]
struct MyVec<const N: usize>([f64; N]);
impl<const N: usize> MyVec<{N}> {
    fn to_xpr(self) -> Xpr<Term<Self>>
    {
        Xpr::new(self)
    }
}

struct NthElement<const N: usize>(usize);
impl<const N: usize> Fold for NthElement<{N}> {
    type TerminalType<T> = MyVec<{N}>;
    type Output<T> = f64;
    // extracts the n-th element of a terminal
    fn fold_term<T>(&mut self, Term(v): &Term<MyVec<{N}>>) -> f64 {
        v.0[self.0]
    }
}

impl<T,const N: usize> From<Xpr<T>> for MyVec<{N}>
where
    T: Foldable<NthElement<{N}>, Output=f64>
{
    fn from(expr: Xpr<T>) -> Self {
        let mut ret = MyVec([0.;N]);
        for i in 0..3 {
            ret.0[i] = NthElement(i).fold(&expr);
        }
        ret
    }
}

pub fn main() {
    // create a new expression representing a chained addition
    let x = Xpr::new(10) + Xpr::new(15) + Xpr::new(17);
    println!("x = {:?}", x);

    // use the Evaluator to evaluate the expression
    let res = Evaluator.fold(&x);
    println!("res = {}", res);

    // Xpr.eval is just syntactic sugar around folding with an Evaluator
    assert_eq!(res, x.eval());

    // let's use the Fortytwoify folder
    let y = Fortytwoify.fold(&x);
    println!("y = {:?}", y);

    let res = Evaluator.fold(&y);
    println!("res = {}", res);

    // Now let's have a chained addition of vectors without any temporaries
    let x1 = MyVec([1., 2., 3.]).to_xpr();
    let x2 = MyVec([10., 20., 30.]).to_xpr();
    let x3 = MyVec([100., 200., 300.]).to_xpr();
    let v = MyVec::from(x1 + x2 + x3);
    println!("v = {:?}", v);
}
