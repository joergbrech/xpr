use xpr::{ops::Term, Fold, Xpr};

struct Fortytwoify;

// make every i32 terminal wrap "42"
impl Fold<Term<i32>> for Fortytwoify {
    // replaces the terminals by instances of `Self::Output`
    type Output = Xpr<Term<i32>>;

    // replaces i32 terminals with i32 terminals wrapping the value "42"
    fn fold(&mut self, _: &Term<i32>) -> Self::Output {
        Xpr::new(42)
    }
}

fn main() {
    // create a new expression representing a chained addition
    let x = Xpr::new(10) + Xpr::new(15) + Xpr::new(17);
    println!("x = {:?}", x);

    println!("x evaluates to {}.", x.eval());
    assert_eq!(x.eval(), 42);

    // let's use the Fortytwoify folder
    println!("\n>>> Fortitwoify x <<<\n");
    let x = Fortytwoify.fold(&x);
    println!("x = {:?}", x);

    println!("x evaluates to {}.", x.eval());
    assert_eq!(x.eval(), 126);
}
