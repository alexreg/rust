// run-pass
// pretty-expanded FIXME(#23616)

#![feature(box_syntax)]

struct A { a: isize, b: Box<isize> }

fn field_copy_after_field_borrow() {
    let mut x = A { a: 1, b: box 2 };
    let p = &mut x.b;
    drop(x.a);
    **p = 3;
}

fn fu_field_copy_after_field_borrow() {
    let mut x = A { a: 1, b: box 2 };
    let p = &mut x.b;
    let y = A { b: box 3, .. x };
    drop(y);
    **p = 4;
}

fn field_deref_after_field_borrow() {
    let mut x = A { a: 1, b: box 2 };
    let p = &mut x.a;
    drop(*x.b);
    *p = 3;
}

fn field_move_after_field_borrow() {
    let mut x = A { a: 1, b: box 2 };
    let p = &mut x.a;
    drop(x.b);
    *p = 3;
}

fn fu_field_move_after_field_borrow() {
    let mut x = A { a: 1, b: box 2 };
    let p = &mut x.a;
    let y = A { a: 3, .. x };
    drop(y);
    *p = 4;
}

fn main() {
    field_copy_after_field_borrow();
    fu_field_copy_after_field_borrow();
    field_deref_after_field_borrow();
    field_move_after_field_borrow();
    fu_field_move_after_field_borrow();
}
