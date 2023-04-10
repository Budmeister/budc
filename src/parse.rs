use crate::grammar::*;
use colored::Colorize;
use logos::{Lexer, Logos};
use std::{
    fmt::{Debug, Display},
    hash::Hash, ops::Range,
};

pub enum Node<T, N> {
    Tm(T),
    NonTm(N, Vec<Node<T, N>>),
}
impl<T: Display, N: Display> Display for Node<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tm(t) => write!(f, "{}", t),
            Self::NonTm(n, _) => write!(f, "{}", n),
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
    println!(
        "Acting on token {} in state {}/{}",
        tok.to_string().blue(),
        state_stack.last().unwrap(),
        state_num
    );
    match action_opt {
        Some(Action::Reduce(n, len)) => {
            println!("Reducing {} states to {}", len, n);
            // Remove len items from the stack
            let mut children = vec![];
            for _ in 0..*len {
                state_stack.pop();
                let node_opt = node_stack.pop();
                if let Some(node) = node_opt {
                    children.push(node);
                } else {
                    return Err(
                        "Mismatched stacks! Needed more children, but node_stack was empty!".to_string()
                    );
                }
            }
            children.reverse();
            let node = Node::NonTm(n.clone(), children);
            node_stack.push(node);
            // Goto new state
            state_num = *state_stack.last().unwrap();
            let goto_opt = &states[state_num].goto.get(n);
            match goto_opt {
                Some(goto) => {
                    println!("Going to state {} after reduction", goto);
                    state_stack.push(**goto);
                }
                None => {
                    return Err(format!(
                        "No goto available for {} in state {}. Quitting.",
                        n, state_num
                    ));
                }
            }
            println!("Stack: {:?}", state_stack);
            return Ok(Some(tok));
        }
        Some(Action::Shift(next_state)) => {
            println!("Shifting state {} onto the stack", next_state);
            state_stack.push(*next_state);
            let node = Node::Tm(load_data(tok, slice));
            node_stack.push(node);
            println!("Stack: {:?}", state_stack);
            return Ok(None);
        }
        Some(Action::Multi(actions)) => {
            println!(
                "Multiple actions available for {} in state {}: {:?}. Selecting first one, {}.",
                tok.to_string().blue(), state_num, actions, actions[0]
            );
            let action = &actions[0];
            return do_action(
                tok,
                slice,
                slice_location,
                &Some(action),
                state_stack,
                node_stack,
                state_num,
                states,
                load_data,
            );
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
                println!(
                    "Calling do_action({}, {:?}, stack, {}, states)",
                    tok, action_opt, state_num
                );
                if let Some(t) = do_action(
                    tok,
                    lex.slice(),
                    lex.span(),
                    &action_opt,
                    &mut state_stack,
                    &mut node_stack,
                    state_num,
                    &states,
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
            println!(
                "Calling do_action({}, {:?}, stack, {}, states)",
                tok, action_opt, state_num
            );
            if let Some(t) = do_action(
                tok,
                "$",
                0..0,
                &action_opt,
                &mut state_stack,
                &mut node_stack,
                state_num,
                &states,
                load_data,
            )? {
                tok = t;
            } else {
                break;
            }
        }
        println!("Stack remaining: {:?}", state_stack);
        println!("Accepted!");
        return Ok(node_stack);
    }
    // Stack ran out before EOF reached
    let remaining_tokens: Vec<T> = lex.collect();
    print!(
        "Tokens remaining: {}",
        remaining_tokens
            .iter()
            .fold(String::new(), |acc, new| acc + &new.to_string())
    );
    Ok(node_stack)
}

const DEFAULT_INDENT: i32 = 4;

fn get_indent(indent: i32) -> String {
    let mut retval = "".to_string();
    for _ in 0..indent {
        retval = retval + " ";
    }
    retval
}

pub fn print_tree_visitor<T: Display, N: Display>(node: &Node<T, N>, indent: i32) {
    match node {
        Node::Tm(t) => {
            print!("\"{}\"", t);
        },
        Node::NonTm(n, children) => {
            let indent_str = get_indent(indent);
            let plus_1 = get_indent(indent + DEFAULT_INDENT);
            println!("\"{}\" {{", n);
            for child in children {
                print!("{}", plus_1);
                print_tree_visitor(child, indent + DEFAULT_INDENT);
                print!(",\n");
            }
            print!("{}}}", indent_str);
        },
    }
}
