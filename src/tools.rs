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