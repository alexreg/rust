use std::ffi::OsString;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

use rustc_data_structures::fx::FxHashMap;

use super::{Result, VfsLock, Buf, LinkOrCopy, Vfs};

#[derive(Clone, Debug, RustcEncodable, RustcDecodable)]
pub struct MemoryVfs {
    pub root: FsObject,
}

impl Default for MemoryVfs {
    fn default() -> Self {
        Self {
            root: FsObject {
                kind: FsObjectKind::Directory {
                    entries: Default::default(),
                },
            },
        }
    }
}

#[derive(Clone, Debug, RustcEncodable, RustcDecodable)]
pub struct FsObject {
    pub kind: FsObjectKind,
}

#[derive(Clone, Debug, RustcEncodable, RustcDecodable)]
pub enum FsObjectKind {
    Directory {
        /// Entries in directory, keyed by file/dir name.
        entries: FxHashMap<OsString, FsObject>,
    },
    File {
        /// The contents of the file, as a byte stream.
        contents: Arc<Vec<u8>>,
    },
}

impl FsObject {
    pub fn is_dir(&self) -> bool {
        if let FsObjectKind::Directory { .. } = self.kind {
            true
        } else {
            false
        }
    }

    pub fn is_file(&self) -> bool {
        if let FsObjectKind::File { .. } = self.kind {
            true
        } else {
            false
        }
    }
}

macro_rules! declare_obj_fns {
    (
        [$get_obj_fn:ident, $get_entries_fn:ident],
        [&$($ref:tt)*],
        $get_fn:ident
    ) => {
        fn $get_obj_fn(& $($ref)* self, path: &Path) -> Result<& $($ref)* FsObject> {
            let mut cur_obj = & $($ref)* self.root;
            for component in path.components() {
                match component {
                    Component::Prefix(_) =>
                        return Err("prefixes not supported".into()),
                    Component::RootDir => {} // All paths are relative to root.
                    Component::CurDir => {}
                    Component::ParentDir =>
                        return Err("parent dir components not supported".into()),
                    Component::Normal(s) => {
                        let entries = if let FsObjectKind::Directory {
                            ref $($ref)* entries,
                        } = cur_obj.kind {
                            entries
                        } else {
                            return Err(format!("\"{}\" is not dir", path.display()).into());
                        };
                        cur_obj = entries.$get_fn(s)
                            .ok_or(format!("subdir \"{}\" not found", s.to_string_lossy()))?;
                    }
                }
            }
            Ok(cur_obj)
        }

        fn $get_entries_fn(& $($ref)* self, path: &Path)
            -> Result<& $($ref)* FxHashMap<OsString, FsObject>>
        {
            let obj = self.$get_obj_fn(path)?;
            if let FsObjectKind::Directory {
                ref $($ref)* entries,
            } = obj.kind {
                Ok(entries)
            } else {
                Err(format!("\"{}\" is not dir", path.display()).into())
            }
        }
    }
}

impl MemoryVfs {
    declare_obj_fns!([get_obj_ref, get_entries_ref], [&], get);
    declare_obj_fns!([get_obj_mut, get_entries_mut], [&mut], get_mut);
}

impl Vfs for MemoryVfs {
    fn canonicalize_path(&self, path: &Path) -> Result<PathBuf> {
        Ok(path.to_path_buf())
    }

    fn read_dir(&self, path: &Path) -> Result<Vec<Result<PathBuf>>> {
        let entries = self.get_entries_ref(path)?;
        Ok(entries.keys()
            .map(|e| Ok(path.join(e)))
            .collect()
        )
    }

    fn create_dir(&mut self, path: &Path) -> Result<()> {
        let mut cur_obj = &mut self.root;
        for component in path.components() {
            match component {
                Component::Prefix(_) =>
                    return Err("prefixes not supported".into()),
                Component::RootDir => {} // All paths are relative to root.
                Component::CurDir => {}
                Component::ParentDir =>
                    return Err("parent dir components not supported".into()),
                Component::Normal(s) => {
                    let entries = if let FsObjectKind::Directory {
                        ref mut entries,
                    } = cur_obj.kind {
                        entries
                    } else {
                        return Err(format!("\"{}\" is not dir", path.display()).into());
                    };
                    cur_obj = entries.entry(s.to_os_string())
                        .or_insert_with(|| FsObject {
                            kind: FsObjectKind::Directory {
                                entries: FxHashMap::default(),
                            }
                        });
                }
            }
        }

        Ok(())
    }

    fn remove_dir(&mut self, path: &Path) -> Result<()> {
        let parent = path.parent()
            .ok_or(format!("\"{}\" does not have parent dir", path.display()))?;
        let dir_name = path.file_name()
            .ok_or(format!("\"{}\" does not have dir name", path.display()))?;

        let entries = self.get_entries_mut(parent)?;
        if !entries.contains_key(dir_name) {
            return Err(format!("\"{}\" does not exist", path.display()).into());
        } else if !entries[dir_name].is_dir() {
            return Err(format!("\"{}\" is not a dir", path.display()).into());
        }
        entries.remove(dir_name);

        Ok(())
    }

    fn rename_dir(&mut self, from: &Path, to: &Path) -> Result<()> {
        let from_parent = from.parent()
            .ok_or(format!("\"{}\" does not have parent dir", from.display()))?;
        let from_dir_name = from.file_name()
            .ok_or(format!("\"{}\" does not have dir name", from.display()))?;
        let to_parent = to.parent()
            .ok_or(format!("\"{}\" does not have parent dir", to.display()))?;
        let to_dir_name = to.file_name()
            .ok_or(format!("\"{}\" does not have dir name", to.display()))?;

        let from_parent_entries = self.get_entries_mut(from_parent)?;
        if !from_parent_entries.contains_key(from_dir_name) {
            return Err(format!("\"{}\" does not exist", from.display()).into());
        } else if !from_parent_entries[from_dir_name].is_dir() {
            return Err(format!("\"{}\" is not a dir", from.display()).into());
        }
        let entry = from_parent_entries.remove(from_dir_name).unwrap();

        let to_parent_entries = self.get_entries_mut(to_parent)?;
        to_parent_entries.insert(to_dir_name.to_os_string(), entry);

        Ok(())
    }

    fn read_file(&self, path: &Path) -> Result<Buf<'_>> {
        let file_obj = self.get_obj_ref(path)?;
        if let FsObjectKind::File { ref contents } = file_obj.kind {
            Ok(Buf::Ref(contents))
        } else {
            Err(format!("\"{}\" is not a file", path.display()).into())
        }
    }

    fn write_file(&mut self, path: &Path, contents: Buf<'_>) -> Result<()> {
        use std::collections::hash_map::Entry;

        let parent = path.parent()
            .ok_or(format!("\"{}\" does not have parent dir", path.display()))?;
        let file_name = path.file_name()
            .ok_or(format!("\"{}\" does not have file name", path.display()))?;

        let file_obj = FsObject {
            kind: FsObjectKind::File {
                contents: Arc::new(contents.into_owned()),
            },
        };

        let parent_entries = self.get_entries_mut(parent)?;
        match parent_entries.entry(file_name.to_os_string()) {
            Entry::Vacant(entry) => { entry.insert(file_obj); }
            Entry::Occupied(mut entry) => { entry.insert(file_obj); }
        }

        Ok(())
    }

    fn link_or_copy_file(&mut self, from: &Path, to: &Path) -> Result<LinkOrCopy> {
        use std::collections::hash_map::Entry;

        let to_parent = to.parent()
            .ok_or(format!("\"{}\" does not have parent dir", to.display()))?;
        let to_file_name = to.file_name()
            .ok_or(format!("\"{}\" does not have file name", to.display()))?;

        let from_obj = self.get_obj_ref(from)?;
        let contents = if let FsObjectKind::File { ref contents } = from_obj.kind {
            contents
        } else {
            return Err(format!("\"{}\" is not a file", from.display()).into());
        };

        let copied_obj = FsObject {
            kind: FsObjectKind::File {
                contents: contents.clone(),
            },
        };

        let to_parent_entries = self.get_entries_mut(to_parent)?;
        match to_parent_entries.entry(to_file_name.to_os_string()) {
            Entry::Vacant(entry) => { entry.insert(copied_obj); }
            Entry::Occupied(mut entry) => { entry.insert(copied_obj); }
        }

        Ok(LinkOrCopy::Link)
    }

    fn remove_file(&mut self, path: &Path) -> Result<()> {
        let parent = path.parent()
            .ok_or(format!("\"{}\" does not have parent dir", path.display()))?;
        let file_name = path.file_name()
            .ok_or(format!("\"{}\" does not have file name", path.display()))?;

        let entries = self.get_entries_mut(parent)?;
        if !entries.contains_key(file_name) {
            return Err(format!("\"{}\" does not exist", path.display()).into());
        } else if !entries[file_name].is_dir() {
            return Err(format!("\"{}\" is not a file", path.display()).into());
        }
        entries.remove(file_name);

        Ok(())
    }

    fn rename_file(&mut self, from: &Path, to: &Path) -> Result<()> {
        let from_parent = from.parent()
            .ok_or(format!("\"{}\" does not have parent dir", from.display()))?;
        let from_file_name = from.file_name()
            .ok_or(format!("\"{}\" does not have file name", from.display()))?;
        let to_parent = to.parent()
            .ok_or(format!("\"{}\" does not have parent dir", to.display()))?;
        let to_file_name = to.file_name()
            .ok_or(format!("\"{}\" does not have file name", to.display()))?;

        let from_parent_entries = self.get_entries_mut(from_parent)?;
        if !from_parent_entries.contains_key(from_file_name) {
            return Err(format!("\"{}\" does not exist", from.display()).into());
        } else if !from_parent_entries[from_file_name].is_file() {
            return Err(format!("\"{}\" is not a file", from.display()).into());
        }
        let entry = from_parent_entries.remove(from_file_name).unwrap();

        let to_parent_entries = self.get_entries_mut(to_parent)?;
        to_parent_entries.insert(to_file_name.to_os_string(), entry);

        Ok(())
    }

    fn lock(&mut self, _path: &Path, _create: bool, _exclusive: bool) -> Result<VfsLock> {
        // No need to perform locking, since everything is in-memory.
        Ok(VfsLock(Box::new(())))
    }
}
