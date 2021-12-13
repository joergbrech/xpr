use xpr::{ops::*, *};

// If we are writing a linear algebra library,
// we will need a statically sized vector type
#[derive(Debug)]
struct VecN<const N: usize>([f64; N]);

impl<const N: usize> VecN<{ N }> {
    // conversion into a vector expression
    fn into_xpr(self) -> Xpr<Term<Self>> {
        Xpr::new(self)
    }
}

struct IthElement<const N: usize>(usize);

impl<const N: usize> Fold for IthElement<{ N }> {
    // match all terminals wrapping a `VecN`
    type TerminalType = VecN<{ N }>;

    // replace by the value at the index in `IthElement`
    type Output = f64;

    // extracts the i-th element of a vector terminal
    fn fold_term(&mut self, Term(v): &Term<VecN<{ N }>>) -> f64 {
        v.0[self.0]
    }
}

impl<T, const N: usize> From<Xpr<T>> for VecN<{ N }>
where
    T: Foldable<IthElement<{ N }>, Output = f64>,
{
    // conversion from a vector expression to a VecN instance
    fn from(expr: Xpr<T>) -> Self {
        // initialize return vector
        let mut ret = VecN([Default::default(); N]);

        // apply the operations in the vector expression element-wise
        for i in 0..N {
            ret.0[i] = IthElement(i).fold(&expr);
        }
        ret
    }
}

pub fn main() {
    // Create a couple of vectors and convert to Xpr expressions
    let x1 = VecN([1., 2., 3., 4.]).into_xpr();
    let x2 = VecN([10., 20., 30., 40.]).into_xpr();
    let x3 = VecN([100., 200., 300., 400.]).into_xpr();
    let x4 = VecN([1000., 2000., 3000., 4000.]).into_xpr();

    // A chained addition without any VecN temporaries!
    let v = VecN::from(x1 + x2 + x3 + x4);
    println!("v = {:?}", v);
}
