//! A typechecker for the lambda-Pi calculus modulo rewriting.

use core::convert::TryFrom;
use kocheck::{par, parse, seq, Error, Event, Opt, PathRead};
use structopt::StructOpt;

fn produce(pr: PathRead, opt: &Opt) -> impl Iterator<Item = Result<Event, Error>> {
    let path = std::iter::once(Ok(Event::Module(pr.path)));
    let cmds = parse(pr.read, &opt).map(|cmd| cmd.map(Event::Command));
    path.chain(cmds)
}

/// Flatten an iterator of results of iterators of results into an iterator of results.
///
/// Source: <https://www.reddit.com/r/rust/comments/9u6846/rust_puzzle_flatten_a_nested_iterator_of_results>
pub fn flatten_nested_results<O, I, T, E>(outer: O) -> impl Iterator<Item = Result<T, E>>
where
    O: Iterator<Item = Result<I, E>>,
    I: Iterator<Item = Result<T, E>>,
{
    outer.flat_map(|inner_result| {
        let (v, r) = match inner_result {
            Ok(v) => (Some(v), None),
            Err(e) => (None, Some(Err(e))),
        };
        v.into_iter().flatten().chain(r)
    })
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

    // lazily produce events from all specified files
    let iter = opt
        .files
        .iter()
        .map(PathRead::try_from)
        .map(|pr| Ok(produce(pr?, &opt)));
    let iter = flatten_nested_results(iter)
        .inspect(|r| r.iter().for_each(|event| log::info!("{}", event)));
    // box the iterator to control type size growth
    let mut iter = Box::new(iter);

    let parallel = opt.jobs.is_some();

    // if parallel execution is enabled, assume an unbounded channel by default
    let channel = if parallel {
        Some(opt.channel_capacity.unwrap_or(None))
    } else {
        opt.channel_capacity
    };

    match channel {
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

            // sending fails prematurely if consumption fails
            // in that case, get the error below
            let _ = iter.try_for_each(|cmd| sender.send(cmd));

            // signalise that we are done sending precommands
            // (otherwise the consumer will eventually wait forever)
            drop(sender);

            // wait for all commands to be consumed
            consumer.join().unwrap()?;
        }
        None => seq::consume(iter, &opt)?,
    }
    Ok(())
}
