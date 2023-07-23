//! SLR(1) parsing
//! 
//! Author:     Brian Smith
//! Year:       2023

use crate::{grammar::*, logging::LoggingOptions, error::*};
use colored::Colorize;
use logos::{Lexer, Logos};
use log::{debug, error};
use std::{
    fmt::{Debug, Display},
    hash::Hash, ops::Range,
};

#[derive(Clone)]
pub enum Node<T, N> {
    Tm{ t: T, range: Range<usize> },
    NonTm{ n: N, children: Vec<Node<T, N>>, range: Range<usize> },
}
impl<T, N> Ranged for Node<T, N> {
    fn get_range(&self) -> &Range<usize> {
        match self {
            Self::Tm { t: _, range } => range,
            Self::NonTm { n: _, children: _, range } => range,
        }
    }
}
impl<T: Display, N: Display> Display for Node<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tm{ t, range } => write!(f, "{} @ {:?}", t, range),
            Self::NonTm{ n, children: _, range } => write!(f, "{} @ {:?}", n, range),
        }
    }
}
impl<T: Display, N: Display> Debug for Node<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

// Return None if the token was used up, Some(tok) if it should still be used for a future action
fn do_action<'a, T, N>(
    log_options: &LoggingOptions,
    tok: T,
    slice: &str,
    slice_location: Range<usize>,
    action_opt: &Option<&Action<N>>,
    state_stack: &mut Vec<usize>,
    node_stack: &mut Vec<Node<T, N>>,
    mut state_num: usize,
    states: &Vec<State<T, N>>,
    load_data: fn(T, &str) -> T
) -> Result<Option<T>, String>
where
    T: PartialEq + Eq + Hash + Display + Debug + Logos<'a>,
    N: PartialEq + Eq + Hash + Display + Clone,
{
    if log_options.print_actions {
        debug!(
            "Acting on token {} in state {}/{}",
            tok.to_string().blue(),
            state_stack.last().unwrap(),
            state_num
        );
    }
    match action_opt {
        Some(Action::Reduce(n, len)) => {
            if log_options.print_actions {
                debug!("Reducing {} states to {}", len, n);
            }
            // Remove len items from the stack
            let mut children = vec![];
            let mut slice_location = slice_location;
            for _ in 0..*len {
                state_stack.pop();
                let node_opt = node_stack.pop();
                if let Some(node) = node_opt {
                    slice_location.start = usize::min(slice_location.start, node.get_range().start);
                    slice_location.end = usize::max(slice_location.end, node.get_range().end);
                    children.push(node);
                } else {
                    return Err(
                        "Mismatched stacks! Needed more children, but node_stack was empty!".to_string()
                    );
                }
            }
            children.reverse();
            let node = Node::NonTm{ n: n.clone(), children, range: slice_location };
            node_stack.push(node);
            // Goto new state
            state_num = *state_stack.last().unwrap();
            let goto_opt = &states[state_num].goto.get(n);
            match goto_opt {
                Some(goto) => {
                    if log_options.print_actions {
                        debug!("Going to state {} after reduction", goto);
                    }
                    state_stack.push(**goto);
                }
                None => {
                    return Err(format!(
                        "No goto available for {} in state {}. Quitting.",
                        n, state_num
                    ));
                }
            }
            if log_options.print_actions {
                debug!("Stack: {:?}", state_stack);
            }
            return Ok(Some(tok));
        }
        Some(Action::Shift(next_state)) => {
            if log_options.print_actions {
                debug!("Shifting state {} onto the stack", next_state);
            }
            state_stack.push(*next_state);
            let node = Node::Tm{ t: load_data(tok, slice), range: slice_location };
            node_stack.push(node);
            if log_options.print_actions {
                debug!("Stack: {:?}", state_stack);
            }
            return Ok(None);
        }
        Some(Action::Multi(actions)) => {
            let mut first_shift = None;
            let mut first_reduce = None;
            if actions.len() < 2 {
                return Err(format!("Invalid action - Multi-action with less than two actions: {:?}, in state {}:", actions, state_num));
            }
            for action in actions {
                match action {
                    Action::Shift(_) => {
                        if let None = first_shift {
                            first_shift = Some(action);
                        }
                    }
                    Action::Reduce(_, _) => {
                        if let None = first_reduce {
                            first_reduce = Some(action);
                        }
                    }
                    Action::Multi(_) => {
                        return Err(format!("Invalid action - Multi-action containing multi-actions: {:?}, in state {}", actions, state_num));
                    }
                }
            }
            let action;
            if let Some(a) = first_shift {
                action = a;
                debug!("Multiple actions available for {} in state {}: {:?}. Selecting the first shift action: {}",
                    tok.to_string().blue(), state_num, actions, action
                );
            } else if let Some(a) = first_reduce {
                action = a;
                debug!("Multiple actions available for {} in state {}: {:?}. No shifts, so selecting first reduce: {}",
                    tok.to_string().blue(), state_num, actions, action
                );
            } else {
                return Err(format!("Did not find a valid action in Multi-action: {:?}", actions));
            }
            do_action(
                log_options,
                tok,
                slice,
                slice_location,
                &Some(action),
                state_stack,
                node_stack,
                state_num,
                states,
                load_data,
            )
        }
        None => {
            let options = &states[*state_stack.last().unwrap()].next.keys();
            return Err(format!(
                "Syntax Error on token {} at location {} (state {}). Expected: {:?}",
                tok.to_string().blue(), slice_location.start, state_num.to_string().red(), options
            ));
        }
    }
}

pub fn lr1_parse<'a, T, N>(
    log_options: &LoggingOptions,
    states: &Vec<State<T, N>>,
    lex: &mut Lexer<'a, T>,
    eof: T,
    error: T,
    load_data: fn(T, &str) -> T
) -> Result<Vec<Node<T, N>>, String>
where
    T: PartialEq + Eq + Hash + Display + Debug + Clone + Logos<'a, Source = str>,
    N: PartialEq + Eq + Hash + Display + Clone,
{
    let mut state_stack: Vec<usize> = vec![0];
    let mut node_stack: Vec<Node<T, N>> = vec![];
    if log_options.print_actions {
        debug!("{}", "Actions:".color("#ff7f00").bold());
    }
    while state_stack.len() != 0 {
        if let Some(mut tok) = lex.next() {
            if tok == error {
                return Err(format!(
                    "Invalid token encountered {} at location {}", lex.slice().blue(), lex.span().start
                ));
            }
            loop {
                let state_num = *state_stack.last().unwrap();
                let action_opt = &states[state_num].next.get(&tok);
                if log_options.print_actions {
                    debug!(
                        "Calling do_action({}, {:?}, stack, {}, states)",
                        tok, action_opt, state_num
                    );
                }
                if let Some(t) = do_action(
                    log_options,
                    tok,
                    lex.slice(),
                    lex.span(),
                    action_opt,
                    &mut state_stack,
                    &mut node_stack,
                    state_num,
                    states,
                    load_data,
                )? {
                    tok = t;
                } else {
                    break;
                }
            }
        } else {
            break;
        }
    }
    if state_stack.len() != 0 {
        // Reached EOF before stack ran out, so act on EOF
        let mut tok = eof;
        loop {
            let state_num = *state_stack.last().unwrap();
            let action_opt = &states[state_num].next.get(&tok);
            if log_options.print_actions {
                debug!(
                    "Calling do_action({}, {:?}, stack, {}, states)",
                    tok, action_opt, state_num
                );
            }
            if let Some(t) = do_action(
                log_options,
                tok,
                "$",
                0..0,
                action_opt,
                &mut state_stack,
                &mut node_stack,
                state_num,
                states,
                load_data,
            )? {
                tok = t;
            } else {
                break;
            }
        }
        if log_options.print_actions {
            debug!("Stack remaining: {:?}", state_stack);
            debug!("Accepted!");
            debug!("End actions");
        }
        return Ok(node_stack);
    }
    // Stack ran out before EOF reached
    let remaining_tokens: Vec<T> = lex.collect();
    error!(
        "Tokens remaining: {}",
        remaining_tokens
            .iter()
            .fold(String::new(), |acc, new| acc + &new.to_string())
    );
    if log_options.print_actions {
        debug!("End actions");
    }
    Ok(node_stack)
}

const DEFAULT_INDENT: i32 = 4;

fn get_indent(indent: i32) -> String {
    let mut retval = "".to_string();
    for i in 0..indent {
        if i % DEFAULT_INDENT == 0 {
            retval = retval + &"|".color("#404040");
        } else {
            retval = retval + " ";
        }
    }
    retval
}

pub fn print_tree_visitor<T: Display, N: Display>(node: &Node<T, N>, indent: i32) {
    match node {
        Node::Tm{ t, range } => {
            print!("\"{}\": {:?}", t, range);
        },
        Node::NonTm{ n, children, range } => {
            let indent_str = get_indent(indent);
            let plus_1 = get_indent(indent + DEFAULT_INDENT);
            println!("\"{}\": {:?} {{", n, range);
            for child in children {
                print!("{}", plus_1);
                print_tree_visitor(child, indent + DEFAULT_INDENT);
                print!(",\n");
            }
            print!("{}}}", indent_str);
        },
    }
}
