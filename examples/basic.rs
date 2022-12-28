use std::io;

use log::error;
use reerror::{throw, Context, Result};

fn foo(f: &str) -> Result<()> {
    let _ = throw!(std::fs::File::open(f), "opening '{}'", f);

    Ok(())
}

fn bar() -> Result<()> {
    throw!(foo("foo").context("loading savefile"));
    Ok(())
}

fn main() {
    env_logger::init();
    if let Err(e) = bar() {
        error!("{e}");
    }
}
