use kocheck::{par, Error, Event, Opt};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

pub mod fetch;
pub mod parse_make;

#[wasm_bindgen(module = "/js/export.js")]
extern "C" {
    #[wasm_bindgen]
    fn add_hash_output(s: &str);

    #[wasm_bindgen]
    fn add_lambda_output(s: &str);

    #[wasm_bindgen]
    fn add_error(s: &str);
}

struct Module<S> {
    name: S,
    text: S,
}

fn produce_single<'a>(
    module: Module<&'a str>,
    opt: &'a Opt,
) -> impl Iterator<Item = Result<Event, Error>> + 'a {
    let head = std::iter::once(Ok(Event::Module(vec![module.name.to_string()])));
    /*
    let commands = kontroli::parse::lexes(module.text)
        .map(move |cmd| kocheck::parse::<String>(cmd?, opt))
        .map(|res| res.transpose())
        .flatten()
        .map(|cmd| cmd.map(Event::Command).map_err(|e| e.into()));
    // cmds
    head.chain(commands)
    */
    head
}

fn produce_multiple<'a>(
    modules: &'a [Module<String>],
    opt: &'a Opt,
) -> Vec<impl Iterator<Item = Result<Event, Error>> + 'a> {
    let mut result = vec![];
    for Module { name, text } in modules {
        result.push(produce_single(Module { text, name }, opt));
    }
    result
}

fn consume(iter: impl Iterator<Item = Result<Event, Error>> + Send, opt: &Opt) {
    let iter = iter.inspect(|r| r.iter().for_each(|ev| add_lambda_output(&ev.to_string())));

    if let Err(e) = par::consume(iter, &opt) {
        add_error(&format!("Error: {:?}", e))
    }
}

fn get_omit(do_until: usize) -> Option<kocheck::Stage> {
    match do_until {
        0 => Some(kocheck::Stage::Share),
        1 => Some(kocheck::Stage::Infer),
        2 => Some(kocheck::Stage::Check),
        3 => None,
        _ => panic!("omission index out of bounds"),
    }
}

#[wasm_bindgen]
pub fn check_single(text: &str, eta: bool, omit: usize) {
    //console_log::init_with_level(log::Level::Trace);
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let opt = Opt {
        eta,
        omit: get_omit(omit),
        channel_capacity: None,
        jobs: None,
        files: vec![],
    };

    consume(produce_single(Module { text, name: "main" }, &opt), &opt)
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
    name: String,
    dependency: Vec<String>,
    dependency_url_list: Vec<String>,
    raw_url: String,
}

#[wasm_bindgen]
pub async fn check_multiple(programs: JsValue, module_to_run: String, eta: bool, omit: usize) {
    //console_log::init_with_level(log::Level::Trace);
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let programs: Vec<Program> = programs.into_serde().unwrap();

    let opt = Opt {
        eta,
        omit: get_omit(omit),
        channel_capacity: None,
        jobs: None,
        files: vec![],
    };

    let main = if let Some(main) = programs.iter().find(|module| module.name == module_to_run) {
        main
    } else {
        return;
    };

    let mut modules = Vec::new();
    for (filename, url) in main.dependency.iter().zip(main.dependency_url_list.iter()) {
        add_hash_output(&filename);
        // remove suffix ".dk"
        let name = filename[0..filename.len() - 3].to_string();

        let text = fetch::fetch(&url)
            .await
            .expect("fetch did not return anything");

        modules.push(Module { name, text });
    }

    let vec_iter = produce_multiple(&modules, &opt);
    consume(vec_iter.into_iter().flatten(), &opt)
}
