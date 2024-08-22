use std::path::{Path, PathBuf};

pub struct Platform {
    pub(crate) working_dir: PathBuf,
    system_header_paths: Vec<PathBuf>,
    normal_header_paths: Vec<PathBuf>,
}

impl Platform {
    pub(crate) fn new(working_dir: PathBuf) -> Self {
        Self {
            working_dir,
            system_header_paths: Vec::new(),
            normal_header_paths: Vec::new(),
        }
    }

    pub(crate) fn resolve_normal_include_path(&self, normal: &str) -> Option<PathBuf> {
        let working_dir_path = self.working_dir.join(normal);
        if Path::exists(&working_dir_path) {
            return Some(working_dir_path);
        }

        for normal_path in &self.normal_header_paths {
            let path = normal_path.join(normal);
            if Path::exists(&path) {
                return Some(path);
            }
        }

        None
    }

    pub(crate) fn resolve_system_include_path(&self, system: &str) -> Option<PathBuf> {
        let working_dir_path = self.working_dir.join(system);
        if Path::exists(&working_dir_path) {
            return Some(working_dir_path);
        }

        for system_path in &self.normal_header_paths {
            let path = system_path.join(system);
            if Path::exists(&path) {
                return Some(path);
            }
        }

        None
    }
}
