// compile-pass
// #39665

fn batches(n: &u32) -> impl Iterator<Item = &u32> {
    std::iter::once(n)
}

fn main() {}
