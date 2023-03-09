use crate::grammar::*;
use logos::{Lexer, Logos};
use std::{hash::Hash, fmt::Display};
use colored::Colorize;

// Return true if the token was used up, false if it should still be used for a future action
fn do_action<'a, T, N>(tok: &T, action_opt: &Option<&Action<N>>, stack: &mut Vec<usize>, mut state_num: usize, states: &Vec<State<T, N>>) -> Result<bool, String>
where
    T: PartialEq + Eq + Hash + Display + Logos<'a>,
    N: PartialEq + Eq + Hash + Display,
{
    println!("Acting on token {} in state {}/{}", tok.to_string().blue(), stack[stack.len() - 1], state_num);
    let token_used;
    match action_opt {
        Some(Action::Reduce(n, len)) => {
            println!("Reducing {} states to {}", len, n);
            // Remove len items from the stack
            for _ in 0..*len {
                stack.pop();
            }
            // Goto new state
            state_num = *stack.last().unwrap();
            let goto_opt = &states[state_num].goto.get(n);
            match goto_opt {
                Some(goto) => {
                    println!("Going to state {} after reduction", goto);
                    stack.push(**goto);
                },
                None => {
                    return Err(format!("No goto available for {} in state {}. Quitting.", n, state_num));
                }
            }
            token_used = false;
        },
        Some(Action::Shift(next_state)) => {
            println!("Shifting state {} onto the stack", next_state);
            stack.push(*next_state);
            token_used = true;
        },
        Some(Action::Multi(actions)) => {
            println!("Multiple actions available for {} in state {}: {:?}. Selecting first one, {}.", tok, state_num, actions, actions[0]);
            let action = &actions[0];
            return do_action(tok, &Some(action), stack, state_num, states);
        },
        None => {
            return Err(format!("No action available for {} in state {}. Quitting.", tok, state_num));
        },
    }
    println!("Stack: {:?}", stack);
    Ok(token_used)

}

pub fn lr1_parse<'a, T, N>(states: &Vec<State<T, N>>, lex: &mut Lexer<'a, T>, eof: T) -> Result<(), String>
where
    T: PartialEq + Eq + Hash + Display + Logos<'a>,
    N: PartialEq + Eq + Hash + Display,
{
    let mut stack: Vec<usize> = vec![0];
    while stack.len() != 0 {
        if let Some(tok) = lex.next() {
            loop {
                let state_num = *stack.last().unwrap();
                let action_opt = &states[state_num].next.get(&tok);
                println!("Calling do_action({}, {:?}, stack, {}, states)", tok, action_opt, state_num);
                if do_action(&tok, &action_opt, &mut stack, state_num, &states)? {
                    break;
                }
            } 
        } else {
            break;
        }

    }
    if stack.len() != 0 {
        // Reached EOF before stack ran out, so act on EOF
        loop {
            let state_num = *stack.last().unwrap();
            let action_opt = &states[state_num].next.get(&eof);
            println!("Calling do_action({}, {:?}, stack, {}, states)", eof, action_opt, state_num);
            if do_action(&eof, &action_opt, &mut stack, state_num, &states)? {
                break;
            }
        } 
        println!("Stack remaining: {:?}", stack);
        println!("Accepted!");
        return Ok(());
    }
    // Stack ran out before EOF reached
    let remaining_tokens: Vec<T> = lex.collect();
    print!("Tokens remaining: {}", remaining_tokens.iter().fold(String::new(), |acc, new| acc + &new.to_string()));
    Ok(())
}