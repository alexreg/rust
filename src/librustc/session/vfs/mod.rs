//! This module provides an abstraction of a virtual filesystem (VFS).
//! It is currently used for accessing the incremental compilation cache.

mod disk;
mod memory;

pub use rustc_fs_util::LinkOrCopy;
pub use self::disk::*;
pub use self::memory::*;

use std::any::{Any, TypeId};
use std::fmt;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    message: String,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ErrorKind {
    NotFound,
    Other,
}

impl Error {
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &*self.message)
    }
}

impl From<String> for Error {
    fn from(s: String) -> Self {
        Self {
            kind: ErrorKind::Other,
            message: s,
        }
    }
}

impl From<&'_ str> for Error {
    fn from(s: &'_ str) -> Self {
        Self::from(s.to_owned())
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub enum Buf<'a> {
    Owned(Vec<u8>),
    Ref(&'a [u8]),
}

impl<'a> Buf<'a> {
    pub fn into_owned(self) -> Vec<u8> {
        match self {
            Self::Owned(buf) => buf,
            Self::Ref(slice) => slice.to_vec(),
        }
    }
}

impl<'a> std::ops::Deref for Buf<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match *self {
            Self::Owned(ref buf) => buf,
            Self::Ref(slice) => slice,
        }
    }
}

#[derive(Debug)]
pub struct VfsLock(Box<dyn Any>);

pub trait Vfs: Send + Sync + fmt::Debug {
    fn canonicalize_path(&self, path: &Path) -> Result<PathBuf>;

    fn read_dir(&self, path: &Path) -> Result<Vec<Result<PathBuf>>>;

    fn create_dir(&mut self, path: &Path) -> Result<()>;

    fn remove_dir(&mut self, path: &Path) -> Result<()>;

    fn rename_dir(&mut self, from: &Path, to: &Path) -> Result<()>;

    fn read_file(&self, path: &Path) -> Result<Buf<'_>>;

    fn write_file(&mut self, path: &Path, contents: Buf<'_>) -> Result<()>;

    fn link_or_copy_file(&mut self, from: &Path, to: &Path) -> Result<LinkOrCopy>;

    fn remove_file(&mut self, path: &Path) -> Result<()>;

    fn rename_file(&mut self, from: &Path, to: &Path) -> Result<()>;

    fn lock(&mut self, path: &Path, create: bool, exclusive: bool) -> Result<VfsLock>;

    fn type_id(&self, _: private::Internal) -> TypeId where Self: 'static {
        TypeId::of::<Self>()
    }
}

mod private {
    #[derive(Debug)]
    pub struct Internal;
}

impl dyn Vfs {
    #[inline]
    pub fn downcast<T: Vfs + 'static>(self: Box<Self>)
        -> std::result::Result<Box<T>, Box<dyn Vfs>>
    {
        if self.is::<T>() {
            unsafe {
                let raw: *mut dyn Vfs = Box::into_raw(self);
                Ok(Box::from_raw(raw as *mut T))
            }
        } else {
            Err(self)
        }
    }
}

impl dyn Vfs + 'static {
    #[inline]
    pub fn is<T: Vfs + 'static>(&self) -> bool {
        let t = TypeId::of::<T>();
        let boxed = self.type_id(private::Internal);
        t == boxed
    }

    #[inline]
    pub fn downcast_ref<T: Vfs + 'static>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe {
                Some(&*(self as *const dyn Vfs as *const T))
            }
        } else {
            None
        }
    }

    #[inline]
    pub fn downcast_mut<T: Vfs + 'static>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            unsafe {
                Some(&mut *(self as *mut dyn Vfs as *mut T))
            }
        } else {
            None
        }
    }
}
