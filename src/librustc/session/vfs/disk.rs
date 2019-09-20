use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use rustc_data_structures::flock;
use rustc_fs_util as fs_util;

use super::{Error, ErrorKind, Result, VfsLock, Buf, LinkOrCopy, Vfs};

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        let kind = match err.kind() {
            io::ErrorKind::NotFound => ErrorKind::NotFound,
            _ => ErrorKind::Other,
        };
        Self {
            kind,
            message: err.to_string(),
        }
    }
}

#[derive(Clone, Default, Debug)]
pub struct DiskVfs;

impl Vfs for DiskVfs {
    fn canonicalize_path(&self, path: &Path) -> Result<PathBuf> {
        Ok(path.canonicalize()?)
    }

    fn read_dir(&self, path: &Path) -> Result<Vec<Result<PathBuf>>> {
        let entries = fs::read_dir(path)?.into_iter()
            .map(|e| Ok(e?.path()))
            .collect();
        Ok(entries)
    }

    fn create_dir(&mut self, path: &Path) -> Result<()> {
        Ok(fs::create_dir_all(path)?)
    }

    fn remove_dir(&mut self, path: &Path) -> Result<()> {
        Ok(fs::remove_dir_all(path)?)
    }

    fn rename_dir(&mut self, from: &Path, to: &Path) -> Result<()> {
        Ok(fs::rename(from, to)?)
    }

    fn read_file(&self, path: &Path) -> Result<Buf<'_>> {
        Ok(Buf::Owned(fs::read(path)?))
    }

    fn write_file(&mut self, path: &Path, contents: Buf<'_>) -> Result<()> {
        Ok(fs::write(path, &*contents)?)
    }

    fn link_or_copy_file(&mut self, from: &Path, to: &Path) -> Result<LinkOrCopy> {
        Ok(fs_util::link_or_copy(from, to)?)
    }

    fn remove_file(&mut self, path: &Path) -> Result<()> {
        Ok(fs::remove_file(path)?)
    }

    fn rename_file(&mut self, from: &Path, to: &Path) -> Result<()> {
        Ok(fs::rename(from, to)?)
    }

    fn lock(&mut self, path: &Path, create: bool, exclusive: bool) -> Result<VfsLock> {
        let lock = flock::Lock::new(path, false, create, exclusive)
            .map(|lock| VfsLock(Box::new(lock)))?;
        Ok(lock)
    }
}
