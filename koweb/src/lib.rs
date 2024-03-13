use kocheck::{process, Error, Event, Opt};
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

fn produce(module: Module<&str>) -> impl Iterator<Item = Result<Event, Error>> + '_ {
    use dedukti_parse::{term, Strict, Symb};
    let head = std::iter::once(Ok(Event::Module(vec![module.name.to_string()])));
    let commands = Strict::<_, term::Atom<Symb<String>>, String>::new(module.text)
        .map(|cmd| Ok(Event::Command(cmd?.map_const(String::from))));
    head.chain(commands)
}

fn consume(iter: impl Iterator<Item = Result<Event, Error>> + Send, opt: &Opt) {
    let iter = iter.inspect(|r| r.iter().for_each(|ev| add_lambda_output(&ev.to_string())));

    if let Err(e) = process::consume(iter, opt) {
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

    consume(produce(Module { text, name: "main" }), &opt)
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

    let programs: Vec<Program> = serde_wasm_bindgen::from_value(programs).unwrap();

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
        add_hash_output(filename);
        // remove suffix ".dk"
        let name = filename[0..filename.len() - 3].to_string();

        let text = fetch::fetch(url)
            .await
            .expect("fetch did not return anything");

        modules.push(Module { name, text });
    }

    let modules = modules
        .iter()
        .flat_map(|Module { name, text }| produce(Module { name, text }));
    consume(modules, &opt)
}
