use xpr::{ops::Term, Expression, Fold, Xpr};
use std::ops::{Index, Range};

// If we are writing a linear algebra library,
// we will need a statically sized vector type
#[derive(Debug)]
struct VecN<const N: usize>(Box<[f64; N]>);

impl<const N: usize> VecN<{ N }> {
    #[inline]
    fn new(array: [f64; N]) -> Self {
        Self(Box::new(array))
    }
}

impl<const N: usize> Index<Range<usize>> for VecN<N> 
{
    type Output = [f64];
    fn index(&self, index: Range<usize>) -> &Self::Output
    {
        &self.0[index]
    }
}

impl<const N: usize> Index<usize> for VecN<N> 
{
    type Output = f64;
    fn index(&self, index: usize) -> &Self::Output
    {
        &self.0[index]
    }
}

// a convenience trait for cnverting VecN instances to xpr terminals
impl<const N: usize> Expression for VecN<N> {}

// now lets implement conversion from an Xpr<T> expression to VecN

struct IthElement<const N: usize>(usize);

// match all terminals wrapping a `VecN`
impl<const N: usize> Fold<Term<VecN<{ N }>>> for IthElement<{ N }> {
    // replace by the value at the index in `IthElement`
    type Output = f64;

    // extracts the i-th element of a vector terminal
    #[inline]
    fn fold(&mut self, Term(v): &Term<VecN<{ N }>>) -> f64 {
        v[self.0]
    }
}

impl<T, const N: usize> From<Xpr<T>> for VecN<{ N }>
where
    IthElement<N>: Fold<Xpr<T>, Output = f64>,
{
    // conversion from a vector expression to a VecN instance
    #[inline]
    fn from(expr: Xpr<T>) -> Self {
        // scary unsafe uninitialized array
        let mut ret = VecN::new(unsafe { std::mem::MaybeUninit::uninit().assume_init() });

        // apply the operations in the vector expression element-wise
        for (i, e) in ret.0.iter_mut().enumerate() {
            *e = IthElement(i).fold(&expr);
        }
        ret
    }
}

pub fn main() {
    // Create a couple of vectors and convert to Xpr expressions
    let x1 = VecN::new([0.6; 5000]).into_xpr();
    let x2 = VecN::new([1.0; 5000]).into_xpr();
    let x3 = VecN::new([40.0; 5000]).into_xpr();
    let x4 = VecN::new([100.0; 5000]).into_xpr();
    let x5 = VecN::new([3000.0; 5000]).into_xpr();

    // A chained addition without any VecN temporaries!
    let v = VecN::from(x1 + x2 + x3 + x4 + x5);
    println!("v[0..5] = {:?}", &v[0..5]);
}
