import init, {
    check_single,
    check_multiple,
    get_graph_rust,
} from "../pkg/koweb.js";

let program_list = [];

class Program {
    constructor(name, dependency, dependency_url_list, raw_url) {
        this.name = name;
        this.dependency = dependency;
        this.dependency_url_list = dependency_url_list;
        this.raw_url = raw_url;
    }
}

function remove_all_outputs_dom() {
    document.querySelectorAll(".prompt").forEach((e) => e.remove());
}

function remove_all_errors_dom() {
    document.querySelectorAll(".error").forEach((e) => e.remove());
}

let check_fetch = function check_fetch(response) {
    if (response.ok === false) {
        display_error_dom(
            "ERROR IN FETCH : " + response.statusText + " - " + response.url,
            "errors"
        );
        throw Error(response.statusText);
    }
    return response; //why do we return tho
};

function load_program_from_url(context_id) {
    remove_all_errors_dom();
    const url = document.getElementById("url").value;
    if (url != "") {
        fetch(url)
            .then(check_fetch)
            .then((result) => {
                result
                    .text() //if the string is 404 not found
                    .then((string) => {
                        console.log(
                            "THIS IS THE STRING WE GET FROM THE URL :: ",
                            string
                        );
                        load_text_from_url_in_editor(string);
                    })
                    .catch((err) => {
                        console.log("ERROR :", err);
                        display_error_dom(err, context_id);
                    });
            })
            .catch((err) => {
                console.log("ERROR :", err);
                // display_error_dom(err, context_id);
            });
    } else {
        display_error_dom("Empty url field", context_id);
    }
}

function run_program_from_url(context_id) {
    remove_all_errors_dom();
    const url = document.getElementById("url").value;
    if (url != "") {
        fetch(url)
            .then(check_fetch)
            .then((result) => {
                result
                    .text()
                    .then((string) => {
                        console.log(
                            "THIS IS THE STRING WE RUN FROM THE URL :: ",
                            string
                        );
                        run(string);
                    })
                    .catch((err) => {
                        console.log("ERROR :", err);
                        display_error_dom(err, context_id);
                    });
            })
            .catch((err) => {
                console.log("ERROR :", err);
                display_error_dom(err, context_id);
            });
    } else {
        display_error_dom("Empty url field", context_id);
    }
}

function display_error_dom(error_msg, context) {
    var error_msg_dom = document.createElement("p");
    var text = document.createTextNode(error_msg);
    error_msg_dom.classList.add("error");
    error_msg_dom.classList.add("bounce-in");
    error_msg_dom.appendChild(text);
    var element = document.getElementById(context);
    element.appendChild(error_msg_dom);
}

function load_text_from_url_in_editor(program_text) {
    document.getElementById('editor').value = program_text;
}

async function run(program = undefined) {
    // try {
    remove_all_outputs_dom();
    // await init();

    let text = program ?? document.getElementById('editor').value;
    let eta = document.getElementById("eta").checked;
    let omit = document.getElementById("omit").value;
    check_single(text, eta, omit);
}


function clear_all_output(){
    //hide the output box and remove all current output 
    document.querySelectorAll(".prompt").forEach((e) => e.remove());
}

document.getElementById("load_url").onclick = () => {
    load_program_from_url("errors");
};

document.getElementById("run_url").onclick = () => {
    run_program_from_url("errors");
};

document.getElementById("run").onclick = async () => {
    await run();
};

document.getElementById("run_multiple").onclick = async () => {
    // if the program_list is empty and this is clicked show some error message
    clear_all_output();
    let module_to_run = document.getElementById("file_to_run").value;
    let eta = document.getElementById("eta").checked;
    let omit = document.getElementById("omit").value;
    await check_multiple(program_list, module_to_run, eta, omit);
};


document.getElementById("load_make").onclick = () => {
    fetch_make_text_from_url();
};

// const url = "https://raw.githubusercontent.com//bachelorproject/master/examples/kontroli.mk";
// https://github.com/01mf02/kontroli-rs/blob/master/examples/sudoku/deps.mk
function fetch_make_text_from_url() {
    remove_all_errors_dom();
    const url = document.getElementById("urlmake").value; //continue here
    console.log(url);
    if (url != "") {
        fetch(url)
            .then(check_fetch)
            .then((result) => {
                result
                    .text() //if the string is 404 not found
                    .then((string) => {
                        console.log(
                            "THIS IS THE MAKE STRING WE GOT FROM :: ",
                            string
                        );
                        use_graph_data(get_graph_rust(string)); //donc ici je vais utiliser un fonction rust qui permetra de get let dependences
                    }) //here we need to call the get dep from rust then we can generate the html and the css from it and the raw urls
                    .catch((err) => {
                        console.log("ERROR :", err);
                        display_error_dom(err, "errors");
                    });
            })
            .catch((err) => {
                console.log("ERROR :", err);
                // display_error_dom(err, context_id);
            });
    } else {
        display_error_dom("Empty url field", "errors");
    }
}

function use_graph_data(graph_data) {
    const dependency_list = graph_data;
    console.log("GRAPH DATA : ", graph_data);

    let list_of_files = [];

    for (let i = 0; i < dependency_list.length; i++) {
        list_of_files.push(dependency_list[i][0]);
    }
    console.log("LIST OF FILES : ", list_of_files);

    generate_run_options_html(graph_data);
    const urls = generate_gitraw_urls(list_of_files);
    const dependency_url_list = dependencies_as_urls(graph_data, urls);
    save_to_program_list(graph_data, urls, dependency_url_list);
}

function remove_all_select_options() {
    document.querySelectorAll(".select").forEach((e) => e.remove());
}

function generate_run_options_html(graph_data) {
    remove_all_select_options();
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

function remove_all_files_dom() {
    document.querySelectorAll(".fs").forEach((e) => e.remove());
}

function generate_gitraw_urls(list_of_files) {
    const top_url = document.getElementById("urlmake").value;

    let result_list = [];

    for (let file of list_of_files) {
        // console.log("FILE : ", file);
        if (file.startsWith("../")) {
            let sub_dir_counter = 0;
            while (file.startsWith("../")) {
                sub_dir_counter += 1;
                console.log("file before remove ../", file);
                file = file.slice(3, file.length);
                console.log("file after remove ../", file);
            }

            let result_relative_url = top_url.substring(
                0,
                top_url.lastIndexOf("/")
            );
            while (sub_dir_counter != 0) {
                result_relative_url = result_relative_url.slice(0, -1);
                result_relative_url = result_relative_url.substring(
                    0,
                    result_relative_url.lastIndexOf("/")
                );
                sub_dir_counter -= 1;
            }
            result_list.push(result_relative_url + "/" + file);
        } else {
            //so i need to get the url till the last /
            var firstpart = top_url.substring(0, top_url.lastIndexOf("/"));
            let dkurl = firstpart + "/" + file; //this is not correct i need to remove the n.mk then add file
            // console.log("NEW GITRAW URL : ", dkurl);
            result_list.push(dkurl);
        }
    }
    // console.log("GENERATE URL FINAL :", result_list);
    return result_list;
}

// TODO fix this i think
// TODO i should only display run selected module when there is something loaded or display a message like nothing was loaded
function dependencies_as_urls(graph_data, urls) {
    // console.log("DEBUG DEPENDENCY AS URL");
    // let counter = 0;
    let dep_url_list_list = [];
    for (let node of graph_data) {
        let dep_url_list = generate_gitraw_urls(node[1]);
        // console.log("DEBUG LIST PUSHED IN LIST LIST", dep_url_list);
        dep_url_list_list.push(dep_url_list);
    }
    return dep_url_list_list;
}

function save_to_program_list(graph_data, urls, dependency_url_list) {
    for (let i = 0; i < graph_data.length; i++) {
        program_list.push(
            new Program(
                graph_data[i][0],
                graph_data[i][1],
                dependency_url_list[i],
                urls[i]
            )
        );
    }
    console.log("PROGRAM LIST : ", program_list);
}
