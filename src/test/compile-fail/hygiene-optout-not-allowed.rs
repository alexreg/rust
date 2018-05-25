// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(decl_macro)]
#![feature(macro_hygiene_optout)]

macro m1(#foo) {}
//~^ ERROR hygiene opt-out syntax is not allowed here

macro m2(#$x:ident) {}
//~^ ERROR hygiene opt-out syntax is not allowed here

macro m3($#x:ident) {}
//~^ ERROR hygiene opt-out syntax is not allowed here

macro m4($mod_name:ident) {
    pub mod #$mod_name {}
    //~^ ERROR hygiene opt-out syntax of the form `#$ident` is not allowed
    //~| ERROR expected identifier, found `#`
}

macro m5($mod_name:ident) {
    pub mod $#mod_name {}
    //~^ ERROR hygiene opt-out syntax of the form `$#ident` is not allowed
}

fn main() {
    m4!(foo);
    m5!(foo);
}
