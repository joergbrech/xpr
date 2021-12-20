use xpr::{ops::Term, Fold, Xpr};

pub struct Evaluator;

/// evaluates an expression of i32 terminals
impl Fold<Term<i32>> for Evaluator {
    type Output = i32;

    /// replaces Terminal values with their wrapped type
    fn fold(&mut self, Term(x): &Term<i32>) -> i32 {
        *x
    }
}

fn main() {
    // create a new expression representing a chained addition
    let x = Xpr::new(10) + Xpr::new(15) + Xpr::new(17);
    println!("x = {:?}", x);

    // use the Evaluator to evaluate the expression
    let res = Evaluator.fold(&x);

    println!("x evaluates to {}.", res);
    assert_eq!(res, 42);

    // note that `Xpr::eval` is syntactic sugar around a similar generic
    // Evaluator struct
    assert_eq!(res, x.eval());
}
