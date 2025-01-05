use std::path::{Path, PathBuf};

pub(crate) struct Platform {
    working_dir: PathBuf,
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

    pub(crate) fn working_dir(&self) -> &PathBuf {
        &self.working_dir
    }

    // resolves #include directives which use quotation marks
    // first look in working directory, then in normal_header_paths
    // to match gcc/clang behavior, these would be system paths like /usr/include as well as
    // any additional include paths specified using -I
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

    // resolves #include directives which use angle brackets
    // does not look in working directory first
    pub(crate) fn resolve_system_include_path(&self, system: &str) -> Option<PathBuf> {
        for system_path in &self.system_header_paths {
            let path = system_path.join(system);
            if Path::exists(&path) {
                return Some(path);
            }
        }

        None
    }
}
