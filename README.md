# xpr

***Disclaimer*: This is a pet project that was developed mainly for procrastination purposes.**

xpr is a rust library that allows you to use expression templates with your custom type.
It is inspired by [boost::yap](https://www.boost.org/doc/libs/1_74_0/doc/html/yap.html).
Expressions can be lazily evaluated and manipulated using a [Fold](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html) trait.

# Example usage
```rust
use xpr::*; 

// An expression representing an operation
let y = -Xpr::new(2)*Xpr::new(-21);

// lazy evaluation
assert_eq!(y.eval(), 42);
```