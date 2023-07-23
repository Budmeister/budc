use std::{collections::{HashMap, HashSet}, fmt::Display};

// To string map of set
pub fn to_string_mos<K: Display, V: ToStringCollection>(map: &HashMap<K, V>) -> String {
    let mut string = "{".to_owned();
    let mut first = true;
    for (k, v) in map {
        if !first {
            string += ", ";
        }
        first = false;
        string += &format!("{}: {}", k, v.to_string());
    }
    string + "}"

}
pub fn to_string<K: Display, V: Display>(map: &HashMap<K, V>) -> String {
    let mut string = "{".to_owned();
    let mut first = true;
    for (k, v) in map {
        if !first {
            string += ", ";
        }
        first = false;
        string += &format!("{}: {}", k, v);
    }
    string + "}"

}

pub trait ToStringCollection {
    fn to_string(&self) -> String;
}
impl<T: Display> ToStringCollection for Vec<T> {
    fn to_string(&self) -> String {
        let mut string = "[".to_owned();
        let mut first = true;
        for t in self {
            if !first {
                string += ", ";
            }
            first = false;
            string += &t.to_string();
        }
        string + "]"
    }
}
// impl<K: Display, V: Display + !ToStringCollection> ToStringCollection for HashMap<K, V> {
//     fn to_string(&self) -> String {
//         let mut string = "{".to_owned();
//         let mut first = true;
//         for (k, v) in self {
//             if !first {
//                 string += ", ";
//             }
//             first = false;
//             string += &format!("{}: {}", k, v);
//         }
//         string + "]"
//     }
// }
// impl<K: Display, V: ToStringCollection> ToStringCollection for HashMap<K, V> {
//     fn to_string(&self) -> String {
//         let mut string = "{".to_owned();
//         let mut first = true;
//         for (k, v) in self {
//             if !first {
//                 string += ", ";
//             }
//             first = false;
//             string += &format!("{}: {}", k, v.to_string());
//         }
//         string + "]"
//     }
// }
impl<T: Display> ToStringCollection for HashSet<T> {
    fn to_string(&self) -> String {
        let mut string = "{".to_owned();
        let mut first = true;
        for t in self {
            if !first {
                string += ", ";
            }
            first = false;
            string += &t.to_string();
        }
        string + "}"
    }
}

/// Iterator yielding every line in a string. The line includes newline character(s).
/// Modified from: https://stackoverflow.com/questions/40455997/iterate-over-lines-in-a-string-including-the-newline-characters
pub struct LinesWithEndings<'a> {
    input: &'a str,
}

impl<'a> From<&'a str> for LinesWithEndings<'a> {
    fn from(input: &'a str) -> LinesWithEndings<'a> {
        LinesWithEndings {
            input: input,
        }
    }
}

impl<'a> Iterator for LinesWithEndings<'a> {
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<&'a str> {
        if self.input.is_empty() {
            return None;
        }
        // let mut split = None;
        // for (i, chars) in self.input.chars().zip(self.input.chars().skip(1)).enumerate() {
        //     match chars {
        //         ('\n', '\r') | ('\r', '\n') => {
        //             // Handle two new lines
        //             split = Some(i + 2);
        //             break;
        //         }
        //         ('\n', _) | ('\r', _) => {
        //             // Handle one new line
        //             split = Some(i + 1);
        //             break;
        //         }
        //         _ => {}
        //     }
        // }
        // let split = match split {
        //     Some(i) => i,
        //     None => self.input.len(),
        // };
        let split = self.input.find('\n').map(|i| i + 1).unwrap_or(self.input.len());
        let (line, rest) = self.input.split_at(split);
        self.input = rest;
        Some(line)
    }
}