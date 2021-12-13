use xpr::{ops::*, *};

/// evaluates an expression of i32 terminals
pub struct Evaluator;
impl Fold for Evaluator {
    type TerminalType = i32;
    type Output = i32;
    /// replaces Terminal values with their wrapped type
    fn fold_term(&mut self, Term(x): &Term<i32>) -> i32 {
        *x
    }
}

// make every i32 terminal wrap "42"
struct Fortytwoify;
impl Fold for Fortytwoify {
    type TerminalType = i32;
    type Output = Xpr<Term<i32>>;
    // replaces i32 terminals with other terminals of the same type
    fn fold_term(&mut self, _: &Term<i32>) -> Self::Output {
        Xpr::new(42)
    }
}

#[derive(Debug)]
struct VecN<const N: usize>([f64; N]);
impl<const N: usize> VecN<{ N }> {
    fn into_xpr(self) -> Xpr<Term<Self>> {
        Xpr::new(self)
    }
}

struct IthElement<const N: usize>(usize);
impl<const N: usize> Fold for IthElement<{ N }> {
    type TerminalType = VecN<{ N }>;
    type Output = f64;
    // extracts the n-th element of a terminal
    fn fold_term(&mut self, Term(v): &Term<VecN<{ N }>>) -> f64 {
        v.0[self.0]
    }
}

impl<T, const N: usize> From<Xpr<T>> for VecN<{ N }>
where
    T: Foldable<IthElement<{ N }>, Output = f64>,
{
    fn from(expr: Xpr<T>) -> Self {
        let mut ret = VecN([0.; N]);
        for i in 0..N {
            ret.0[i] = IthElement(i).fold(&expr);
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
    let x1 = VecN([1., 2., 3., 4.]).into_xpr();
    let x2 = VecN([10., 20., 30., 40.]).into_xpr();
    let x3 = VecN([100., 200., 300., 400.]).into_xpr();
    let x4 = VecN([1000., 2000., 3000., 4000.]).into_xpr();
    let v = VecN::from(x1 + x2 + x3 + x4);
    println!("v = {:?}", v);
}
