use std::ops::{Index, Range};
use xpr::{ops::Term, ExpressionCast, Fold, Xpr};

// This is our statically sized vector type
#[derive(Debug)]
struct Vec<const N: usize>(Box<[f64; N]>);

impl<const N: usize> Vec<{ N }> {
    #[inline]
    fn new(array: [f64; N]) -> Self {
        Self(Box::new(array))
    }
}

impl<const N: usize> Index<Range<usize>> for Vec<N> {
    type Output = [f64];

    #[inline]
    fn index(&self, index: Range<usize>) -> &Self::Output {
        &self.0[index]
    }
}

impl<const N: usize> Index<usize> for Vec<N> {
    type Output = f64;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

// a convenience trait for cnverting Vec instances to xpr terminals
impl<const N: usize> ExpressionCast for Vec<N> {}

// now lets implement conversion from an Xpr<T> expression to Vec

struct IthElement<'a, const N: usize>(usize, std::marker::PhantomData<&'a ()>);

impl<'a, const N: usize> Fold<Term<&'a Vec<{ N }>>> for IthElement<'a, { N }> {
    // replace by the value at the index in `IthElement`
    type Output = f64;

    // extracts the i-th element of a vector terminal
    #[inline]
    fn fold(&mut self, Term(v): &Term<&'a Vec<{ N }>>) -> f64 {
        v[self.0]
    }
}

impl<'a, T, const N: usize> From<Xpr<T>> for Vec<{ N }>
where
    IthElement<'a, N>: Fold<Xpr<T>, Output = f64>,
{
    // conversion from a vector expression to a Vec instance
    #[inline]
    fn from(expr: Xpr<T>) -> Self {
        // scary unsafe uninitialized array
        let mut ret = Vec::new(unsafe { std::mem::MaybeUninit::uninit().assume_init() });

        // apply the operations in the vector expression element-wise
        for (i, e) in ret.0.iter_mut().enumerate() {
            *e = IthElement(i, std::marker::PhantomData).fold(&expr);
        }
        ret
    }
}

pub fn main() {
    // Create a couple of vectors
    let x1 = Vec::new([0.6; 5000]);
    let x2 = Vec::new([1.0; 5000]);
    let x3 = Vec::new([40.0; 5000]);
    let x4 = Vec::new([100.0; 5000]);
    let x5 = Vec::new([3000.0; 5000]);

    // A chained addition without any Vec temporaries!
    let v = Vec::from(x1.as_xpr() + &x2 + &x3 + &x4 + &x5);
    println!("v[0..5] = {:?}", &v[0..5]);
}
