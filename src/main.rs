//! Delete me
//! 

fn main()
{
    use xpr::*;
    
    // create a folder, which is any struct implementing `Fold`
    struct Folder;
    impl Fold for Folder {
        type Output = i32;
        fn fold<T>(&mut self, e: &Xpr<T>) -> Option<Self::Output> {
            match e {
                Xpr::Term(_) => { println!("I am a terminal!"); Some(42) },
                Xpr::Neg(_) =>  { println!("I am a negation!"); None },
                Xpr::Add(_) =>  { println!("I am an addition!"); Some(41) },
                Xpr::Mul(_) =>  { println!("I am a multiplication!"); None },
            }
        }
    }

    // create an expression
    let x = -Xpr::new(5)*(Xpr::new(7) + Xpr::new(-10));
    println!("{:?}",x);

    x.transform(&Folder{});
}