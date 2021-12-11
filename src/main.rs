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
struct Vec3([f64; 3]);

struct NthElement(usize);
impl Fold for NthElement {
    type TerminalType<T> = Vec3;
    type Output<T> = f64;
    // extracts the n-th element of a terminal
    fn fold_term<T>(&mut self, Term(v): &Term<Vec3>) -> f64 {
        v.0[self.0]
    }
}

impl<T> From<Xpr<T>> for Vec3
where
    T: Foldable<NthElement>,
    OutputFoldable<NthElement, T>: Into<f64>,
{
    fn from(expr: Xpr<T>) -> Self {
        let mut ret = Vec3([0., 0., 0.]);
        for i in 0..3 {
            ret.0[i] = NthElement(i).fold(&expr).into();
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
    let x1 = Xpr::new(Vec3([1., 2., 3.]));
    let x2 = Xpr::new(Vec3([10., 20., 30.]));
    let x3 = Xpr::new(Vec3([100., 200., 300.]));
    let v = Vec3::from(x1 + x2 + x3);
    println!("v = {:?}", v);
}
