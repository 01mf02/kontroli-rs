//! A typechecker for the lambda-Pi calculus modulo rewriting.

use core::convert::TryFrom;
use kocheck::{par, parse, seq, Error, Event, Opt, PathRead};
use structopt::StructOpt;

fn produce<F, E>(opt: &Opt, send: F) -> Result<(), Error>
where
    F: Fn(Result<Event, Error>) -> Result<(), E>,
{
    for file in opt.files.iter() {
        let file = PathRead::try_from(file)?;

        use kontroli::parse::lexes;
        //use rayon::iter::{ParallelBridge, ParallelIterator};
        let cmds = lexes(&file.read)
            // TODO: investigate the effect of par_bridge!
            //.par_bridge()
            .map(|tokens| parse::<String>(tokens?, opt))
            .map(|res| res.map_err(|e| e.into()).transpose())
            .flatten();

        let head = core::iter::once(Ok(Event::Module(file.path)));
        let tail = cmds.map(|cmd| cmd.map(Event::Command));

        // sending fails prematurely if consumption fails
        // in that case, handle the error after this function exits
        if head.chain(tail).try_for_each(|event| send(event)).is_err() {
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

    let opt = Opt::from_args();

    // if a precise number of parallel jobs has been given
    if let Some(Some(jobs)) = opt.jobs {
        rayon::ThreadPoolBuilder::new()
            .num_threads(jobs)
            .build_global()
            .unwrap();
    }

    let parallel = opt.jobs.is_some();

    match opt.channel_capacity {
        Some(capacity) => {
            let (sender, receiver) = match capacity {
                Some(capacity) => flume::bounded(capacity),
                None => flume::unbounded(),
            };

            let optr = opt.clone();
            let consumer = std::thread::spawn(move || {
                if parallel {
                    par::consume(receiver.into_iter(), &optr)
                } else {
                    seq::consume(receiver.into_iter(), &optr)
                }
            });

            produce(&opt, |event| sender.send(event))?;

            // signalise that we are done sending precommands
            // (otherwise the consumer will eventually wait forever)
            drop(sender);

            // wait for all commands to be consumed
            consumer.join().unwrap()
        }
        None => {
            if parallel {
                par::run(&opt)
            } else {
                seq::run(&opt)
            }
        }
    }
}
