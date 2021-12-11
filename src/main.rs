//! Delete me

use xpr::*;

/// evaluates an expression
struct Evaluator;
impl Fold for Evaluator {
    type TerminalType = i32;
    type TerminalFoldOutput = i32;
    /// replaces Terminal i32 values by their wrapped type
    fn fold_term(&mut self, x: &Term<Self::TerminalType>) -> Self::TerminalFoldOutput
    {
        x.0
    }

}

// make every i32 terminal wrap "42"
struct Fortytwoify;
impl Fold for Fortytwoify {
    type TerminalType = i32;
    type TerminalFoldOutput = Xpr<Term<i32>>;
    // replaces i32 terminals with other terminals of the same type
    fn fold_term(&mut self, _: &Term<Self::TerminalType>) -> Self::TerminalFoldOutput
    {
        Xpr::new(42)
    }

}

pub fn main() {
    let x = Xpr::new(10) + Xpr::new(15) + Xpr::new(17);
    println!("x = {:?}", x);

    let res = Evaluator.fold(&x);
    println!("res = {}", res);

    let y = Fortytwoify.fold(&x);
    println!("y = {:?}", y);

    let res = Evaluator.fold(&y);
    println!("res = {}", res);

}
