// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(exclusive_range_pattern)]

pub fn main() {
    match 5_usize {
      1_usize...5_usize => {}
      _ => panic!("should match range"),
    }
    match 1_usize {
        1_usize..5_usize => {}
        _ => panic!("should match range start"),
    }
    match 5_usize {
      6_usize...7_usize => panic!("shouldn't match range"),
      _ => {}
    }
    match 7_usize {
        6_usize..7_usize => panic!("shouldn't match range end"),
        _ => {},
    }
    match 5_usize {
      1_usize => panic!("should match non-first range"),
      2_usize...6_usize => {}
      _ => panic!("math is broken")
    }
    match 'c' {
      'a'...'z' => {}
      _ => panic!("should suppport char ranges")
    }
    match -3 {
      -7...5 => {}
      _ => panic!("should match signed range")
    }
}
