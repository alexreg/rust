#![allow(non_upper_case_globals)]
#![allow(dead_code)]
// exec-env:RUST_MIN_STACK=16000000
// rustc-env:RUST_MIN_STACK=16000000
//
// Big stack is needed for pretty-printing, a little sad...

static a: isize =
(((((((((((((((((((((((((((((((((((((((((((((((((((
(((((((((((((((((((((((((((((((((((((((((((((((((((
(((((((((((((((((((((((((((((((((((((((((((((((((((
(((((((((((((((((((((((((((((((((((((((((((((((((((
(((((((((((((((((((((((((((((((((((((((((((((((((((
1
)))))))))))))))))))))))))))))))))))))))))))))))))))
)))))))))))))))))))))))))))))))))))))))))))))))))))
)))))))))))))))))))))))))))))))))))))))))))))))))))
)))))))))))))))))))))))))))))))))))))))))))))))))))
)))))))))))))))))))))))))))))))))))))))))))))))))))
;

pub fn main() {}
