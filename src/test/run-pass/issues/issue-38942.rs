// run-pass

#[repr(u64)]
pub enum NSEventType {
    NSEventTypePressure,
}

pub const A: u64 = NSEventType::NSEventTypePressure as u64;

fn banana() -> u64 {
    A
}

fn main() {
    println!("banana! {}", banana());
}
