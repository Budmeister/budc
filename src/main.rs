//! Author:     Brian Smith
//! Year:       2023

// pub mod slp_names;
// pub mod lexer;
// use crate::lexer::*;
extern crate proc_macros;

pub mod bud;
pub mod error;
pub mod m68k;
pub mod grammar;
pub mod slp;

pub mod slp_symbols;
use logos::Logos;

pub mod tools;
pub mod logging;
use logging::SimpleLogger;
use log::*;

use crate::logging::LoggingOptions;

use std::collections::HashSet;
use std::env;
use std::io::Read;

pub mod parse;
use crate::parse::*;

use colored::Colorize;

static LOGGER: SimpleLogger = SimpleLogger;

fn get_logging_options(args: &HashSet<String>, log_options: &mut LoggingOptions) {
    if args.contains("-print_log_options") {
        log_options.print_log_options = true;
    } else if args.contains("!print_log_options") {
        log_options.print_log_options = false;
    }
    if args.contains("-print_grammar") {
        log_options.print_grammar = true;
    } else if args.contains("!print_grammar") {
        log_options.print_grammar = false;
    }
    if args.contains("-print_firsts") {
        log_options.print_firsts = true;
    } else if args.contains("!print_firsts") {
        log_options.print_firsts = false;
    }
    if args.contains("-print_firsts_actions") {
        log_options.print_firsts_actions = true;
    } else if args.contains("!print_firsts_actions") {
        log_options.print_firsts_actions = false;
    }
    if args.contains("-print_state_transitions") {
        log_options.print_state_transitions = true;
    } else if args.contains("!print_state_transitions") {
        log_options.print_state_transitions = false;
    }
    if args.contains("-print_states") {
        log_options.print_states = true;
    } else if args.contains("!print_states") {
        log_options.print_states = false;
    }
    if args.contains("-print_action_table") {
        log_options.print_action_table = true;
    } else if args.contains("!print_action_table") {
        log_options.print_action_table = false;
    }
    if args.contains("-print_actions") {
        log_options.print_actions = true;
    } else if args.contains("!print_actions") {
        log_options.print_actions = false;
    }
    if args.contains("-print_syntax_tree") {
        log_options.print_syntax_tree = true;
    } else if args.contains("!print_syntax_tree") {
        log_options.print_syntax_tree = false;
    }
    if args.contains("-print_types_trace") {
        log_options.print_types_trace = true;
    } else if args.contains("!print_types_trace") {
        log_options.print_types_trace = false;
    }
    if args.contains("-print_types") {
        log_options.print_types = true;
    } else if args.contains("!print_types") {
        log_options.print_types = false;
    }
    if args.contains("-print_inter_funcs") {
        log_options.print_inter_funcs = true;
    } else if args.contains("!print_inter_funcs") {
        log_options.print_inter_funcs = false;
    }

}

fn get_logging_level(args: &HashSet<String>) -> log::LevelFilter {
    use log::LevelFilter::*;
    if args.contains("-off") {
        println!("Logging level of off received");
        Off
    } else if args.contains("-trace") {
        println!("Logging level of trace received");
        Trace
    } else if args.contains("-debug") {
        println!("Logging level of debug received");
        Debug
    } else if args.contains("-info") {
        println!("Logging level of info received");
        Info
    } else if args.contains("-warn") {
        println!("Logging level of warn received");
        Warn
    } else if args.contains("-error") {
        println!("Logging level of error received");
        Error
    } else {
        println!("No logging level selected, info by default");
        Info
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} filename.bud", args[0]);
        return;
    }
    let filename = args[1].to_string();
    if !filename.ends_with(".bud") {
        println!("Input file must end with \".bud\". Given file: {}", filename);
    }
    let dot = filename.len() - ".bud".len();
    let output_filename = filename[..dot].to_string() + ".S";
    let args: HashSet<String> = args.into_iter().collect();
    // Set up logger
    match log::set_logger(&LOGGER)
            .map(|()| log::set_max_level(get_logging_level(&args))) {
        Ok(_) => {},
        Err(e) => {
            println!("{}", e.to_string().yellow());
            return;
        }
    }
    let mut log_options = LoggingOptions{
        print_log_options: false,
        print_grammar: false,
        print_firsts: false,
        print_firsts_actions: false,
        print_state_transitions: false,
        print_states: false,
        print_action_table: false,
        print_actions: false,
        print_syntax_tree: false,
        print_types_trace: false,
        print_types: false,
        print_inter_funcs: false,
    };
    get_logging_options(&args, &mut log_options);
    if log_options.print_log_options {
        debug!("{:?}", log_options);
    }

    // Read input file
    let mut file = match std::fs::File::open(filename.as_str()) {
        Ok(x) => x,
        Err(x) => {
            error!("IO Error: {}", x);
            return;
        }
    };
    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_) => {}
        Err(err) => {
            error!("{}", err);
            return;
        }
    }
    let lines = error::get_lines(&contents);

    use bud::BudNonTerminal::*;
    use bud::BudTerminal::*;
    let mut g = bud::get_bud_grammar();
    if log_options.print_grammar {
        grammar::print_grammar(&g);
    }

    let states;
    match grammar::lr1_generate(
        &log_options,
        &mut g,
        Start,
        vec![
            Start,
            Items,
            Item,
            FuncDecl,
            StructDecl,
            ImportDecl,
            Path,
            File,
            Args,
            VarDeclsPar,
            VarDecls,
            VarDecl,
            VarDeclAssgn,
            Expr,
            TypeExpr,
            Sqr,
            SqrExpr,
            Expr2,
            Exprs,
            NonBinExpr,
            BlockExpr,
            AssignExpr,
            ReturnExpr,
            CleanupCall,
            CleanupExpr,
            IdExpr,
            LitExpr,
            ParenExpr,
            UnaryExpr,
            BinaryExpr,
            IfExpr,
            IfElse,
            UnlExpr,
            UnlElse,
            WhileExpr,
            DoWhile,
            Unop,
            Binop
        ],
        vec![
            NumGen,
            StrGen,
            CharGen,
            LeftRound,
            RightRound,
            LeftSquiggly,
            RightSquiggly,
            LeftSquare,
            RightSquare,
            Semicolon,
            Colon,
            Dot,
            Assign,
            If,
            Unless,
            Else,
            Import,
            Return,
            Cleanup,
            Do,
            While,
            Break,
            Continue,
            IdGen,
            Plus,
            Minus,
            Star,
            Div,
            And,
            Or,
            BitAnd,
            BitOr,
            BitXor,
            Equal,
            NotEq,
            Greater,
            GrtrEq,
            Less,
            LessEq,
            Not,
            Reference,
        ],
    ) {
        Ok(s) => {
            states = s;
        }
        Err(msg) => {
            error!("{}", msg.yellow());
            return;
        }
    };
    let mut lex = bud::BudTerminal::lexer(&contents);
    let tree;
    match lr1_parse(&log_options, &states, &mut lex, EOF, Error, bud::load_bud_data) {
        Ok(node_stack) => {
            if log_options.print_syntax_tree {
                debug!("{}", "Syntax Tree:".color("#ff7f00").bold());
                print_tree_visitor(&node_stack[0], 0);
                println!();
                debug!("End syntax tree");
            }
            tree = node_stack.into_iter().next().unwrap();
        }
        Err(err) => {
            error::display_err(err, None, &lines);
            return;
        }
    }
    
    let expander = m68k::BudExpander::new();
    let env = match expander.code_generate(&log_options, tree) {
        Ok(env) => env,
        Err(errors) => {
            error!("{}", format!("Unable to compile due to {} errors", errors.len()));
            for (err, func) in errors {
                error::display_err(err, func.as_deref(), &lines);
            }
            return;
        }
    };

    match m68k::write_file(env, &output_filename) {
        Ok(_) => {},
        Err(err) => {
            println!("While saving to file: ");
            println!();
            println!("{}", format!("    User Error: {}", err.get_msg()).red().bold());
        }
    }

    println!();
    println!("{}", "Compilation complete!".green());
    
}
