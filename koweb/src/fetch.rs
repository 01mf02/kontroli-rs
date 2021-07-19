use error_chain::error_chain;

error_chain! {
    foreign_links {
        Io(std::io::Error);
        Reqwest(reqwest::Error);
    }
}

pub async fn fetch(url: &str) -> Result<String> {
    Ok(reqwest::get(url).await?.text().await?)
}
