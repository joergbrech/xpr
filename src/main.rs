//! Delete me

use xpr::*;

struct Foo;
impl Fold for Foo {
    // possibly overwrite default functions
}

pub fn main() {
    let x = Xpr::new(5) + Xpr::new(8) + Xpr::new(17);

    // As a by-product, we have two ways to fold the
    // expression:

    println!("Method 1:");
    let y = Foo.fold(x); // <-- in the current implementation y consumes x

    println!("\nMethod 2:");
    y.fold(&mut Foo {});
}
