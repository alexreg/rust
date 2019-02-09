// compile-flags: -C no-prepopulate-passes
// ignore-tidy-linelength

#![crate_type = "lib"]
#![feature(custom_attribute)]

pub struct S {
    _field: [i32; 8],
}

pub struct UnsafeInner {
    _field: std::cell::UnsafeCell<i16>,
}

// CHECK: zeroext i1 @boolean(i1 zeroext %x)
#[no_mangle]
pub fn boolean(x: bool) -> bool {
    x
}

// CHECK: @readonly_borrow(i32* noalias readonly align 4 dereferenceable(4) %arg0)
// FIXME(#25759): this should also have `nocapture`.
#[no_mangle]
pub fn readonly_borrow(_: &i32) {
}

// CHECK: @static_borrow(i32* noalias readonly align 4 dereferenceable(4) %arg0)
// Static borrow may be captured.
#[no_mangle]
pub fn static_borrow(_: &'static i32) {
}

// CHECK: @named_borrow(i32* noalias readonly align 4 dereferenceable(4) %arg0)
// Borrow with named lifetime may be captured.
#[no_mangle]
pub fn named_borrow<'r>(_: &'r i32) {
}

// CHECK: @unsafe_borrow(i16* align 2 dereferenceable(2) %arg0)
// Unsafe interior means this isn't actually readonly and there may be aliases ...
#[no_mangle]
pub fn unsafe_borrow(_: &UnsafeInner) {
}

// CHECK: @mutable_unsafe_borrow(i16* align 2 dereferenceable(2) %arg0)
// ... unless this is a mutable borrow, those never alias.
#[no_mangle]
pub fn mutable_unsafe_borrow(_: &mut UnsafeInner) {
}

// CHECK: @mutable_borrow(i32* align 4 dereferenceable(4) %arg0)
// FIXME(#25759): this should also have `nocapture`.
#[no_mangle]
pub fn mutable_borrow(_: &mut i32) {
}

// CHECK: @indirect_struct(%S* noalias nocapture dereferenceable(32) %arg0)
#[no_mangle]
pub fn indirect_struct(_: S) {
}

// CHECK: @borrowed_struct(%S* noalias readonly align 4 dereferenceable(32) %arg0)
// FIXME(#25759): this should also have `nocapture`.
#[no_mangle]
pub fn borrowed_struct(_: &S) {
}

// CHECK: noalias align 4 dereferenceable(4) i32* @_box(i32* noalias align 4 dereferenceable(4) %x)
#[no_mangle]
pub fn _box(x: Box<i32>) -> Box<i32> {
    x
}

// CHECK: @struct_return(%S* noalias nocapture sret dereferenceable(32))
#[no_mangle]
pub fn struct_return() -> S {
    S {
        _field: [0, 0, 0, 0, 0, 0, 0, 0]
    }
}

// Hack to get the correct size for the length part in slices.
// CHECK: @helper([[USIZE:i[0-9]+]] %arg0)
#[no_mangle]
pub fn helper(_: usize) {
}

// CHECK: @slice([0 x i8]* noalias nonnull readonly align 1 %arg0.0, [[USIZE]] %arg0.1)
// FIXME(#25759): this should also have `nocapture`.
#[no_mangle]
pub fn slice(_: &[u8]) {
}

// CHECK: @mutable_slice([0 x i8]* nonnull align 1 %arg0.0, [[USIZE]] %arg0.1)
// FIXME(#25759): this should also have `nocapture`.
#[no_mangle]
pub fn mutable_slice(_: &mut [u8]) {
}

// CHECK: @unsafe_slice([0 x i16]* nonnull align 2 %arg0.0, [[USIZE]] %arg0.1)
// Unsafe interior means this isn't actually readonly and there may be aliases ...
#[no_mangle]
pub fn unsafe_slice(_: &[UnsafeInner]) {
}

// CHECK: @str([0 x i8]* noalias nonnull readonly align 1 %arg0.0, [[USIZE]] %arg0.1)
// FIXME(#25759): this should also have `nocapture`.
#[no_mangle]
pub fn str(_: &[u8]) {
}

// CHECK: @trait_borrow({}* nonnull align 1 %arg0.0, [3 x [[USIZE]]]* noalias readonly align {{.*}} dereferenceable({{.*}}) %arg0.1)
// FIXME(#25759): this should also have `nocapture`.
#[no_mangle]
pub fn trait_borrow(_: &Drop) {
}

// CHECK: @trait_box({}* noalias nonnull align 1, [3 x [[USIZE]]]* noalias readonly align {{.*}} dereferenceable({{.*}}))
#[no_mangle]
pub fn trait_box(_: Box<Drop>) {
}

// CHECK: { i8*, i8* } @trait_option(i8* noalias align 1 %x.0, i8* %x.1)
#[no_mangle]
pub fn trait_option(x: Option<Box<Drop>>) -> Option<Box<Drop>> {
    x
}

// CHECK: { [0 x i16]*, [[USIZE]] } @return_slice([0 x i16]* noalias nonnull readonly align 2 %x.0, [[USIZE]] %x.1)
#[no_mangle]
pub fn return_slice(x: &[u16]) -> &[u16] {
    x
}

// CHECK: { i16, i16 } @enum_id_1(i16 %x.0, i16 %x.1)
#[no_mangle]
pub fn enum_id_1(x: Option<Result<u16, u16>>) -> Option<Result<u16, u16>> {
    x
}

// CHECK: { i8, i8 } @enum_id_2(i1 zeroext %x.0, i8 %x.1)
#[no_mangle]
pub fn enum_id_2(x: Option<u8>) -> Option<u8> {
    x
}

// CHECK: noalias i8* @allocator()
#[no_mangle]
#[allocator]
pub fn allocator() -> *const i8 {
    std::ptr::null()
}
