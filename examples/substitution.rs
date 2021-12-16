use xpr::{
    ops::{OutputOfAdd, Term},
    Fold, Xpr,
};

struct Substitution;

// make every i32 terminal wrap "42"
impl Fold for Substitution {
    // matches all terminals wrapping `Self::TerminalType`
    type TerminalType = i32;

    // replaces the terminals by instances of `Self::Output`
    type Output = OutputOfAdd<Term<i32>, Term<i32>>;

    // replaces i32 terminals with an Add expression
    fn fold_term(&mut self, &Term(x): &Term<i32>) -> Self::Output {
        Xpr::new(x - 42) + Xpr::new(42)
    }
}

fn main() {
    // create a new expression representing a chained addition
    let x = Xpr::new(10) + Xpr::new(15) + Xpr::new(17);
    println!("x = {:?}", x);

    println!("x evaluates to {}.", x.eval());
    assert_eq!(x.eval(), 42);

    // let's use the Substitution folder
    println!("Substitution fold...");
    let x = Substitution.fold(&x);
    println!("x = {:?}", x);

    println!("x evaluates to {}.", x.eval());
    assert_eq!(x.eval(), 42);
}
