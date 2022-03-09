//! A typechecker for the lambda-Pi calculus modulo rewriting.

use clap::Parser;
use kocheck::{par, Error, Event, Opt, PathRead};
use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn produce<F, E>(opt: &Opt, send: F) -> Result<(), Error>
where
    F: Fn(Result<Event, Error>) -> Result<(), E>,
{
    for file in opt.files.iter() {
        let file = PathRead::try_from(file)?;

        use std::io::{BufRead, BufReader};
        let lines = BufReader::new(file.read).lines().map(|line| line.unwrap());
        let cmds = kontroli::parse::Lazy::new(lines)
            .inspect(|cmd| cmd.iter().for_each(kocheck::log_cmd))
            .map(|cmd| cmd.map_err(Error::Parse));

        let head = core::iter::once(Ok(Event::Module(file.path)));
        let tail = cmds.map(|cmd| cmd.map(Event::Command));

        // sending fails prematurely if consumption fails
        // in that case, handle the error after this function exits
        if head.chain(tail).try_for_each(&send).is_err() {
            return Ok(());
        }
    }
    Ok(())
}

fn main() -> Result<(), Error> {
    use env_logger::Env;
    // log warnings and errors by default
    // allow setting the logging level by using the environment variable "LOG"
    // e.g. `LOG=trace kocheck ...`
    env_logger::from_env(Env::default().filter_or("LOG", "warn")).init();

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
            let consumer = std::thread::spawn(move || par::consume(receiver.into_iter(), &optr));

            produce(&opt, |event| sender.send(event))?;

            // signalise that we are done sending precommands
            // (otherwise the consumer will eventually wait forever)
            drop(sender);

            // wait for all commands to be consumed
            consumer.join().unwrap()
        }
        None => par::run(&opt),
    }
}
