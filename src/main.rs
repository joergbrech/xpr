//! Delete me
#![feature(generic_associated_types)]

use xpr::*;

/// evaluates an expression
struct Evaluator;
impl Fold for Evaluator {
    type Output<T> = T;
    /// replaces Terminal i32 values by their wrapped type
    fn fold_term<T>(&mut self, x: Term<T>) -> Self::Output<T> {
        x.0
    }
}

// make every i32 terminal wrap "42"
struct Fortytwoify;
impl Fold for Fortytwoify {
    type Output<T> = Xpr<Term<i32>>;
    // replaces i32 terminals with other terminals of the same type
    fn fold_term<T>(&mut self, _: Term<T>) -> Self::Output<T> {
        Xpr::new(42)
    }
}

pub fn main() {
    let x = Xpr::new(10) + Xpr::new(15) + Xpr::new(17);
    println!("x = {:?}", x);

    let res = Evaluator.fold(x);
    println!("res = {}", res);

    let x = Xpr::new(10) + Xpr::new(15) + Xpr::new(17);
    let y = Fortytwoify.fold(x);
    println!("y = {:?}", y);

    let res = Evaluator.fold(y);
    println!("res = {}", res);
}
