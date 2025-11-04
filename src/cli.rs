use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
#[clap(about, version, long_about = None)]
pub struct Args {
    /// A script to evaluate
    #[clap(short, value_name = "COMMAND")]
    pub command: Option<String>,

    /// Path to script
    #[clap(value_name = "FILE")]
    pub script: Option<PathBuf>,

    /// Print full syntax tree
    #[clap(long, default_value_t = true, action(clap::ArgAction::Set))]
    pub syntax_tree: bool,
}
