

use colored::{ColoredString, Colorize};
use log::{Record, Level, Metadata};

pub struct SimpleLogger;
fn get_level_colored_string(level: Level) -> ColoredString {
    match level {
        Level::Error => level.to_string().red(),
        Level::Warn  => level.to_string().yellow(),
        Level::Info  => level.to_string().color("#d6d6d6"),
        Level::Debug => level.to_string().color("#d6d6d6"),
        Level::Trace => level.to_string().color("#d6d6d6"),
    }
}

impl log::Log for SimpleLogger {
    fn enabled(&self, _metadata: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            println!("{} - {}", get_level_colored_string(record.level()), record.args());
        }
    }

    fn flush(&self) {}
}

#[derive(Debug)]
pub struct LoggingOptions {
    pub print_log_options: bool,
    pub print_grammar: bool,
    pub print_firsts: bool,
    pub print_firsts_actions: bool,
    pub print_state_transitions: bool,
    pub print_states: bool,
    pub print_action_table: bool,
    pub print_actions: bool,
    pub print_syntax_tree: bool,
    pub print_types_trace: bool,
    pub print_types: bool,
    pub print_inter_funcs: bool,
}


