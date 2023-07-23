use std::ops::Range;

use colored::Colorize;
use log::*;



pub enum BudErr {
    CompilerErr(CompilerErr),
    UserErr(UserErr),
}

impl BudErr {
    pub fn get_msg(&self) -> &str {
        match self {
            Self::CompilerErr(comp) => &comp.msg,
            Self::UserErr(user) => &user.msg,
        }
    }
    pub fn get_location(&self) -> Option<&Range<usize>> {
        match self {
            Self::CompilerErr(comp) => comp.location.as_ref(),
            Self::UserErr(user) => user.location.as_ref(),
        }
    }
}

impl From<CompilerErr> for BudErr {
    fn from(value: CompilerErr) -> Self {
        Self::CompilerErr(value)
    }
}

impl From<UserErr> for BudErr {
    fn from(value: UserErr) -> Self {
        Self::UserErr(value)
    }
}

pub struct CompilerErr {
    pub msg: String,
    pub location: Option<Range<usize>>,
}

pub struct UserErr {
    pub msg: String,
    pub location: Option<Range<usize>>,
}

pub fn get_lines(source: &str) -> Vec<&str> {
    source.lines().collect()
}

pub fn display_err(err: BudErr, func: Option<&str>, lines: &[&str]) {
    match func {
        Some(func) => {
            error!("In function {}:", func);
        }
        None => {
            error!("In unknown function");
        }
    }
    println!();
    if let Some(range) = err.get_location() {
        let (start, end, delta) = find_line_numbers(range.clone(), lines);
        let num_width = format!("{}", end).len();
        let mut i = range.start - delta;
        for line_num in start..end {
            // Print out the line
            let line_num_str = format!("{}", line_num);
            let line_num_str = pad_spaces_beginning(&line_num_str, num_width);
            let line_num_str = format!("  {} |    ", line_num_str);
            println!("{}{}", line_num_str, lines[line_num]);

            // Print out the indicator
            print!("{}", " ".repeat(line_num_str.len()));
            for _ in 0..lines[line_num].len() {
                if i >= range.start && i < range.end {
                    print!("{}", "^".yellow());
                } else {
                    print!(" ");
                } 
                i += 1;
            }
            print!("\n");
        }
        println!();
    }
    match err {
        BudErr::CompilerErr(err) => {
            println!("{}", format!("    Compiler Error: {}", err.msg).red());
        }
        BudErr::UserErr(err) => {
            println!("{}", format!("    User Error: {}", err.msg).red().bold());
        }
    }
    // error!("\t{}\n")
}

fn pad_spaces_beginning(string: &str, length: usize) -> String {
    " ".repeat(length - string.len()) + string
}

/// (start, end, delta)
/// 
/// `delta` is the difference between the start of the line where `start` is and `start` itself
fn find_line_numbers(range: Range<usize>, lines: &[&str]) -> (usize, usize, usize) {
    let (start, start_line_char) = find_line_number0(range.start, lines);
    let end = start + find_line_number0(range.end - start_line_char, &lines[start..]).0;
    (start, end, start - start_line_char)
}

/// Returns the index of the line and the char index of the first char of that line
fn find_line_number0(location: usize, lines: &[&str]) -> (usize, usize) {
    let mut i = 0;
    for (line_num, line) in lines.iter().enumerate() {
        if i + line.len() >= location {
            return (line_num, i);
        }
        i += line.len();
    }
    (0, 0)
}

pub trait Ranged {
    fn get_range(&self) -> &Range<usize>;
}
pub trait RangedOwned {
    fn get_range_owned(&self) -> Range<usize>;
}
impl<T> RangedOwned for T 
where T: Ranged {
    fn get_range_owned(&self) -> Range<usize> {
        self.get_range().to_owned()
    }
}


#[macro_export]
macro_rules! c_err_opt {
    ($location:expr, $msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::CompilerErr {
            msg: format!($msg $(, $($arg)*)?),
            location: $location.map(|r| r.to_owned()),
        }.into())
    };
}

#[macro_export]
macro_rules! u_err_opt {
    ($location:expr, $msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::UserErr {
            msg: format!($msg $(, $($arg)*)?),
            location: $location.map(|r| r.to_owned()),
        }.into())
    };
}

#[macro_export]
macro_rules! c_err {
    ($location:expr, $msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::CompilerErr {
            msg: format!($msg $(, $($arg)*)?),
            location: Some($location.to_owned()),
        }.into())
    };
    ($msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::CompilerErr {
            msg: format!($msg $(, $($arg)*)?),
            location: None,
        }.into())
    };
}

#[macro_export]
macro_rules! u_err {
    ($location:expr, $msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::UserErr {
            msg: format!($msg $(, $($arg)*)?),
            location: Some($location.to_owned()),
        }.into())
    };
    ($msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::UserErr {
            msg: format!($msg $(, $($arg)*)?),
            location: None,
        }.into())
    };
}