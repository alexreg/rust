// ignore-msvc FIXME(#31306)

// note that these aux-build directives must be in this order
// aux-build:svh-a-base.rs
// aux-build:svh-b.rs
// aux-build:svh-a-change-type-arg.rs
// normalize-stderr-test: "(crate `(\w+)`:) .*" -> "$1 $$PATH_$2"

extern crate a;
extern crate b; //~ ERROR: found possibly newer version of crate `a` which `b` depends on

fn main() {
    b::foo()
}
