// compile-pass
#![allow(dead_code)]
// pretty-expanded FIXME(#23616)

mod a {
    pub enum Enum<T> {
        A(T),
    }

    pub trait X {
        fn dummy(&self) { }
    }
    impl X for isize {}

    pub struct Z<'a>(Enum<&'a (X+'a)>);
    fn foo() { let x: isize = 42; let z = Z(Enum::A(&x as &X)); let _ = z; }
}

mod b {
    trait X {
        fn dummy(&self) { }
    }
    impl X for isize {}
    struct Y<'a>{
        x:Option<&'a (X+'a)>,
    }

    fn bar() {
        let x: isize = 42;
        let _y = Y { x: Some(&x as &X) };
    }
}

mod c {
    pub trait X { fn f(&self); }
    impl X for isize { fn f(&self) {} }
    pub struct Z<'a>(Option<&'a (X+'a)>);
    fn main() { let x: isize = 42; let z = Z(Some(&x as &X)); let _ = z; }
}

pub fn main() {}
