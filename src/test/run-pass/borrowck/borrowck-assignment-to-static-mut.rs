// Test taken from issue #45641.

// run-pass

#![allow(dead_code)]
// Test taken from #45641 (https://github.com/rust-lang/rust/issues/45641)

// revisions: ast mir
//[mir]compile-flags: -Z borrowck=mir

static mut Y: u32 = 0;

unsafe fn should_ok() {
    Y = 1;
}

fn main() {}
