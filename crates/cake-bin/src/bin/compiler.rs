use std::{path::PathBuf, process::Command};

use cake::{CraneliftBackend, Platform, Preprocessor, HandParser, Resolver};
use clap::Parser;

#[derive(Parser, Debug)]
struct Args {
    /// Only run preprocessor, print output to stdout
    #[arg(short = 'E')]
    preprocessor: bool,
    
    /// Output file
    #[arg(short = 'o')]
    output_file: Option<PathBuf>,

    file: PathBuf,
}

fn main() {
    let args = Args::parse();

    let cwd = std::env::current_dir().expect("unable to getcwd");
    let platform = Platform::new(cwd);

    let file_contents = std::fs::read_to_string(&args.file).expect("unable to read file");

    let (object_file, output_file) = if let Some(output_file) = &args.output_file {
        let mut copy = output_file.clone();
        copy.add_extension("o");
        (copy, output_file.clone())
    } else {
        (PathBuf::from("a.o"), PathBuf::from("a.out"))
    };

    let preprocessor = Preprocessor::new(args.file.clone(), file_contents, platform);
    let parser = HandParser::new(preprocessor);
    let parse_output = parser.parse().expect("failed to parse file");
    let resolver = Resolver::new(parse_output);
    let resolve_output = resolver.resolve_ast().expect("failed to resolve ast");

    let backend = CraneliftBackend::from_resolve_output(
        output_file.to_str().expect("weird filename"), 
        &resolve_output
    );
    
    backend.finish_and_write(&object_file);
    
    let mut compile_command = Command::new("clang");
    compile_command
        .arg("-no-pie") // cranelift is not generating position independent assembly
        .arg("-o")
        .arg(output_file)
        .arg(object_file);

    compile_command
        .spawn()
        .expect("unable to launch clang")
        .wait()
        .expect("it didn't run");
}
