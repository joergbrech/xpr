# xpr

***Disclaimer*: This is a pet project that was developed mainly for procrastination purposes.**

xpr is a rust library that allows you to use expression templates with your custom type.
It is inspired by [boost::yap](https://www.boost.org/doc/libs/1_74_0/doc/html/yap.html).
Expressions can be evaluated or manipulated using transforms, which match subexpressions, 
and transform matched expressions, do any kind of calculations in between and return anything
that implements the operations of the used expression.
 
# Example usage
```rust
use xpr::*; 

// An expression 
let y = -Xpr::new(2)*Xpr::new(-21);
assert_eq!(y.eval(), Ok(42));
```

# Current restrictions

 - Only `Neg` and `Mul` for now
 - All input and output types must implement `'static + Copy`.
 - I can't match the types of the input arguments of the expressions. That can be changed easily enough.
 - I don't like that I can't have general generic transforms that return any type without using the type erasure
   indirection I currently use. As a consequence, 
    - Additional overhead in the transform for casting from and to `std::any::Any`
    - I cannot change the output and input types of any expression
    - I cannot actually return an expression, because the type erased return value of a transform is cast into the
      input type of the calling expression