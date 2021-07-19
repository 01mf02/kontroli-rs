// Functions that are called from the Rust code.

export function add_hash_output(message) {
    let doc = window.document;
    let div = doc.createElement("div");
    let p = doc.createElement("p");
    let span = doc.createElement("span");
    span.className = "header_output";
    span.textContent = "#> ";
    p.className = "line";
    p.textContent = message;
    div.className = "prompt";
    div.appendChild(span);
    div.appendChild(p);

    output = document.getElementById("output");
    output.className = "display_output";
    output.appendChild(div);
}

export function add_lambda_output(message) {
    let doc = window.document;
    let div = doc.createElement("div");
    let p = doc.createElement("p");
    let span = doc.createElement("span");
    span.className = "lambda";
    span.textContent = "λ> ";
    p.className = "line";
    p.textContent = message;
    div.className = "prompt";
    div.appendChild(span);
    div.appendChild(p);

    output = document.getElementById("output");
    output.className = "display_output";
    output.appendChild(div);
}
