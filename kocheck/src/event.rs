use kontroli::parse::Command;

pub enum Event {
    Command(Command),
    Module(Vec<String>),
}
