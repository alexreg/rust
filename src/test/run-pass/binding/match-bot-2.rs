// run-pass

#![allow(unreachable_code)]
// N.B., this was only ever failing with optimization disabled.

fn a() -> isize { match return 1 { 2 => 3, _ => panic!() } }

pub fn main() { a(); }
