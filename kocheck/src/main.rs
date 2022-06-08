//! A typechecker for the lambda-Pi calculus modulo rewriting.

use clap::Parser;
use kocheck::{process, Error, Opt};

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() -> Result<(), Error> {
    use env_logger::Env;
    // log warnings and errors by default
    // allow setting the logging level by using the environment variable "LOG"
    // e.g. `LOG=trace kocheck ...`
    env_logger::Builder::from_env(Env::default().filter_or("LOG", "warn")).init();

    let opt = Opt::parse();

    // if a precise number of parallel jobs has been given
    if let Some(Some(jobs)) = opt.jobs {
        rayon::ThreadPoolBuilder::new()
            .num_threads(jobs)
            .build_global()
            .unwrap();
    }

    match opt.channel_capacity {
        Some(capacity) => {
            let (sender, receiver) = match capacity {
                Some(capacity) => flume::bounded(capacity),
                None => flume::unbounded(),
            };

            let optr = opt.clone();
            let consume = std::thread::spawn(move || process::consume(receiver.into_iter(), &optr));

            process::produce(&opt.files, |event| sender.send(event))?;

            // signalise that we are done sending precommands
            // (otherwise the consumer will eventually wait forever)
            drop(sender);

            // wait for all commands to be consumed
            consume.join().unwrap()
        }
        None => process::run(&opt),
    }
}
