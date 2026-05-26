use std::path::PathBuf;

use cake::{Platform, Preprocessor, HandParser, Resolver};
use clap::Parser;

#[derive(Parser, Debug)]
struct Args {
    /// Only run preprocessor, print output to stdout
    #[arg(short = 'E')]
    preprocessor: bool,
    
    /// Output file
    #[arg(short = 'o')]
    output_file: PathBuf,

    file: PathBuf,
}

fn main() {
    let args = Args::parse();

    let cwd = std::env::current_dir().expect("unable to getcwd");
    let platform = Platform::new(cwd);

    let file_contents = std::fs::read_to_string(&args.file).expect("unable to read file");
    
    let preprocessor = Preprocessor::new(args.file, file_contents, platform);
    let parser = HandParser::new(preprocessor);
    let parse_output = parser.parse().expect("failed to parse file");
    let resolver = Resolver::new(parse_output);
    let resolve_output = resolver.resolve_ast().expect("failed to resolve ast");
    
}
