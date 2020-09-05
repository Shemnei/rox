use std::ffi::OsString;

use rox::Rox;

fn main() -> rox::Result<()> {
    let args = std::env::args_os().skip(1).collect::<Vec<OsString>>();
    match args.as_slice() {
        [] => Rox::new().run_repl(),
        [path] => Rox::new().run_file(path),
        [..] => {
            eprintln!("Usage: {} [script]", env!("CARGO_PKG_NAME"));
            std::process::exit(64);
        }
    }
}
