# xpr

![crates.io](https://img.shields.io/crates/v/xpr.svg)
[![](https://img.shields.io/badge/Read%20the-docs-blue)](https://docs.rs/xpr/0.1.0/xpr/)
[![CI](https://github.com/joergbrech/xpr/actions/workflows/CI.yml/badge.svg)](https://github.com/joergbrech/xpr/actions/workflows/CI.yml)

***Disclaimer*: This is a toy project that was developed mainly for procrastination purposes.**

xpr is a rust library that allows you to use expression templates *(expression generics?)* with your custom type.
It is inspired by [boost::yap](https://www.boost.org/doc/libs/1_74_0/doc/html/yap.html).
Expressions can be lazily evaluated and manipulated using a [Fold](https://rust-unofficial.github.io/patterns/patterns/creational/fold.html) trait.

# Example usage
```rust
use xpr::Xpr; 

// An expression representing an operation
let y = -Xpr::new(2)*Xpr::new(-21);

// lazy evaluation
assert_eq!(y.eval(), 42);
```

Check out the [documentation](https://docs.rs/xpr/0.1.0/xpr/) or the [examples](https://github.com/joergbrech/xpr/tree/main/examples) for more useful examples.

## License

Dual-licensed to be compatible with the Rust project.

Licensed under the [Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0) or the [MIT license](https://opensource.org/licenses/MIT), at your option. This file may not be copied, modified, or distributed except according to those terms.
