use error_chain::error_chain;

error_chain! {
    foreign_links {
        Io(std::io::Error);
    }
}

pub async fn fetch(url: &str) -> Result<String> {
    log::info!("Fetching {url} ...");
    // TODO: handle errors
    Ok(run(url).await.unwrap())
}

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;
use web_sys::Response;

// Adapted from: https://rustwasm.github.io/wasm-bindgen/examples/fetch.html
pub async fn run(url: &str) -> core::result::Result<String, JsValue> {
    let window = web_sys::window().unwrap();
    let resp_value = JsFuture::from(window.fetch_with_str(&url)).await?;

    // `resp_value` is a `Response` object.
    assert!(resp_value.is_instance_of::<Response>());
    let resp: Response = resp_value.dyn_into().unwrap();

    // Convert this other `Promise` into a rust `Future`.
    let json = JsFuture::from(resp.text()?).await?;

    Ok(json.as_string().unwrap())
}
