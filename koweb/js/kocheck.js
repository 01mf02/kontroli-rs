import init, {
    check_single,
    check_multiple,
    get_graph_rust,
} from "../pkg/koweb.js";

import { add_error } from "./export.js";

// result of loading a Makefile
let program_list = [];

function remove_all_outputs_dom() {
    document.querySelectorAll(".prompt").forEach(e => e.remove())
}

function remove_all_errors_dom() {
    document.querySelectorAll(".error").forEach(e => e.remove())
}

function check_fetch(response) {
    if (response.ok === false) {
        add_error("Fetch error: " + response.statusText + " - " + response.url);
        throw Error(response.statusText);
    }
    return response
}

function fetch_then(url, f) {
    remove_all_errors_dom();
    console.log("Fetching from URL: ", url);
    if (url == "") {
        add_error("No URL given");
        return
    }
    fetch(url).then(check_fetch).then(result => {
        result.text().then(string => {
            console.log("Content of the fetched file: ", string);
            f(string)
        }).catch(err => {
            console.log("Error: ", err);
            add_error(err);
        })
    }).catch(err => {
        console.log("Error: ", err);
        // add_error(err);
    });
}

async function run(program) {
    remove_all_errors_dom();
    remove_all_outputs_dom();

    let eta = document.getElementById("eta").checked;
    let omit = document.getElementById("omit").value;
    check_single(program, eta, omit);
}

document.getElementById("load_url").onclick = () => {
    fetch_then(document.getElementById("url").value, program_text =>
        document.getElementById('editor').value = program_text
    )
};

document.getElementById("run").onclick = async () => {
    await run(document.getElementById('editor').value);
};

document.getElementById("run_multiple").onclick = async () => {
    // if the program_list is empty and this is clicked show some error message
    remove_all_errors_dom();
    remove_all_outputs_dom();

    let module_to_run = document.getElementById("file_to_run").value;
    let eta = document.getElementById("eta").checked;
    let omit = document.getElementById("omit").value;
    await check_multiple(program_list, module_to_run, eta, omit);
};

// https://github.com/01mf02/kontroli-rs/blob/master/examples/sudoku/deps.mk
document.getElementById("load_make").onclick = () => {
    fetch_then(document.getElementById("urlmake").value, string =>
        use_graph_data(get_graph_rust(string))
    )
};

function use_graph_data(graph_data) {
    const dependency_list = graph_data;
    console.log("Graph data: ", graph_data);

    const files = dependency_list.map(dep => dep[0]);
    console.log("List of files: ", files);

    generate_run_options_html(graph_data);

    const urls = generate_gitraw_urls(files);
    const dependency_url_list = graph_data.map(node => generate_gitraw_urls(node[1]));

    program_list = make_program_list(graph_data, urls, dependency_url_list);
    console.log("Program list: ", program_list);
}

function generate_run_options_html(graph_data) {
    document.querySelectorAll(".select").forEach(e => e.remove())
    let parent_select = document.getElementById("file_to_run");
    for (const node of graph_data) {
        let option = document.createElement("option");
        let test = document.createTextNode(node[0]);
        option.appendChild(test);
        option.classList.add("select");
        option.value = node[0];
        parent_select.appendChild(option);
    }
}

function generate_gitraw_urls(files) {
    const top_url = document.getElementById("urlmake").value;
    const prefix = top_url.substring(0, top_url.lastIndexOf("/"));
    return files.map(file => prefix + "/" + file)
}

function make_program_list(graph_data, urls, dependency_url_list) {
    let l = [];
    for (let i = 0; i < graph_data.length; i++) {
        l.push({
            name: graph_data[i][0],
            dependency: graph_data[i][1],
            dependency_url_list: dependency_url_list[i],
            raw_url: urls[i],
        })
    }
    return l
}
