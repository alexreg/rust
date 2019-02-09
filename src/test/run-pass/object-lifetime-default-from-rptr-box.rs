// Test that the lifetime from the enclosing `&` is "inherited"
// through the `Box` struct.

// pretty-expanded FIXME(#23616)

#![allow(dead_code)]

trait Test {
    fn foo(&self) { }
}

struct SomeStruct<'a> {
    t: &'a Box<Test>,
    u: &'a Box<Test+'a>,
}

fn a<'a>(t: &'a Box<Test>, mut ss: SomeStruct<'a>) {
    ss.t = t;
}

fn b<'a>(t: &'a Box<Test>, mut ss: SomeStruct<'a>) {
    ss.u = t;
}

// see also compile-fail/object-lifetime-default-from-rptr-box-error.rs

fn d<'a>(t: &'a Box<Test+'a>, mut ss: SomeStruct<'a>) {
    ss.u = t;
}

fn main() {
}
