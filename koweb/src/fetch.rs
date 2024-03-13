use error_chain::error_chain;

error_chain! {
    foreign_links {
        Io(std::io::Error);
        Ureq(ureq::Error);
    }
}

// TODO: remove async
pub async fn fetch(url: &str) -> Result<String> {
    Ok(ureq::get(url).call()?.into_string()?)
}
