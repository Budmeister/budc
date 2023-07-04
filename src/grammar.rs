//! Definitions of components for the grammar
//! 
//! Author:     Brian Smith
//! Year:       2023

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use log::*;

use colored::Colorize;
pub enum Option2<T, U> {
    Some1(T),
    Some2(U),
    None,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Symbol<T, N> {
    Tm(T),
    NonTm(N),
}
impl<T: Display, N: Display> Display for Symbol<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tm(t) => write!(f, "{}", t),
            Self::NonTm(n) => write!(f, "{}", n),
        }
    }
}
impl<T: Display, N: Display> Debug for Symbol<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, PartialEq)]
pub struct Der<T, N> {
    pub from: N,
    pub to: Vec<Symbol<T, N>>,
}
impl<T: Display, N: Display> Display for Der<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}",
            self.from,
            self.to
                .iter()
                .map(|sym| sym.to_string())
                .fold(String::new(), |acc, s| acc + &s),
        )
    }
}
impl<T: Display, N: Display> Debug for Der<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

pub enum SubseqData<T, N> {
    Less(Symbol<T, N>),
    Greater(Symbol<T, N>),
    Equal,
    NotEq,
}

// Less    means that der1 is a subsequence of der2
// Greater means that der2 is a subsequence of der1
// If they are equal, a warning will be printed. 
// This will happen if the Ders are equal but the
// DottedDers are different.
fn is_subsequence<
    T: Display + Clone + Eq + Hash + Ord,
    N: Display + Clone + Eq + Hash,
>(der1: &DottedDer<T, N>, der2: &DottedDer<T, N>) -> SubseqData<T, N> {
    let n = der1.der.to.len();
    let m = der2.der.to.len();
    let mut i = 0;

    loop {
        if i == n {
            if i == m {
                // warn!("Comparisons between ders {} and {} were equal; None was returned", der1, der2);
                return SubseqData::Equal;
            } else {
                return SubseqData::Less(der2.der.to[i].clone());
            }
        } else if i == m {
            return SubseqData::Greater(der1.der.to[i].clone());
        }
        if der1.der.to[i] != der2.der.to[i] {
            return SubseqData::NotEq;
        }
        i += 1;
    }
}

pub struct Grammar<T, N> {
    derivations: Vec<Der<T, N>>,
    cached_firsts: HashMap<N, Vec<T>>,
}
impl<T, N> Grammar<T, N> {
    pub fn new(derivations: Vec<Der<T, N>>) -> Grammar<T, N> {
        Grammar {
            derivations,
            cached_firsts: HashMap::new(),
        }
    }
    pub fn get_derivations_for(&self, n: &N) -> Vec<Der<T, N>>
    where
        T: Clone + PartialEq,
        N: Clone + PartialEq,
    {
        let mut ders = Vec::new();
        for der in &self.derivations {
            if der.from == *n {
                ders.push(der.clone())
            }
        }
        ders
    }

    // Returns an empty vector if n is in skip
    // TODO Remove this function
    pub fn first(&mut self, n: &N, skip: Option<&HashSet<N>>) -> Vec<T>
    where
        N: Clone + PartialEq + Eq + Hash + Display,
        T: Clone + Display,
    {
        // println!("Calculating first({}).", n);
        let mut new_skip;
        match skip {
            Some(set) => {
                if set.contains(n) {
                    // println!("{} in skip", n);
                    return Vec::new();
                }
                new_skip = set.clone();
            }
            None => {
                new_skip = {
                    // If there are no skips, use the cached value
                    if let Some(first) = self.cached_firsts.get(n) {
                        // println!("===Using cached value===");
                        // for x in first {
                        //     println!("{}", x);
                        // }
                        // println!("---End cached value---");
                        return first.clone();
                    }
                    HashSet::new()
                }
            }
        }
        // println!("{} not in skip", n);
        new_skip.insert(n.clone());
        // for x in &new_skip {
        //     println!("{}", x);
        // }
        let first = self
            .derivations
            .iter()
            .filter(|der| der.from == *n)
            .map(|der| der.clone())
            .collect::<Vec<Der<T, N>>>()
            // ;let first_b = first
            .iter()
            .map(|der| match &der.to[0] {
                Symbol::NonTm(n) => self.first(&n, Some(&new_skip)),
                Symbol::Tm(t) => vec![t.clone()],
            })
            .collect::<Vec<Vec<T>>>()
            .concat();
        if skip == None {
            self.cached_firsts.insert(n.clone(), first.clone());
        }
        first
    }
}

pub fn print_grammar<T: Display, N: Display>(g: &Grammar<T, N>) {
    debug!("Grammar:");
    for der in &g.derivations {
        debug!(
            "{} -> {}",
            der.from,
            der.to
                .iter()
                .map(|sym| sym.to_string())
                .fold(String::new(), |acc, s| acc + &s),
        );
    }
    debug!("End grammar");
}

#[derive(Clone, PartialEq)]
pub struct DottedDer<T, N>
where
    T: Eq + Hash,
{
    pub der: Der<T, N>,
    pub dot: usize,
    pub look: HashSet<T>,
}
impl<T: Eq + Hash, N> DottedDer<T, N> {
    pub fn new(der: Der<T, N>) -> DottedDer<T, N> {
        DottedDer {
            der,
            dot: 0,
            look: HashSet::new(),
        }
    }

    pub fn advance(&self) -> DottedDer<T, N>
    where
        T: Clone,
        N: Clone,
    {
        let mut advanced: DottedDer<T, N> = self.clone();
        advanced.dot += 1;
        advanced
    }

    pub fn add_looks(&mut self, other_looks: &HashSet<T>) -> bool
    where
        T: Clone,
    {
        let mut added = false;
        let mut iter = other_looks.iter();
        while let Some(t) = iter.next() {
            if !self.look.contains(t) {
                self.look.insert(t.clone());
                added = true;
            }
        }
        added
    }

    pub fn dotted_sym(&self) -> Option<&Symbol<T, N>> {
        if self.dot >= self.der.to.len() {
            None
        } else {
            Some(&self.der.to[self.dot])
        }
    }

    pub fn equals_ignore_look(&self, other: &Self) -> bool
    where
        T: PartialEq,
        N: PartialEq,
    {
        self.der == other.der && self.dot == other.dot
    }

    pub fn next_dotted_sym(&self) -> Option<&Symbol<T, N>> {
        if self.dot + 1 >= self.der.to.len() {
            None
        } else {
            Some(&self.der.to[self.dot + 1])
        }
    }

    pub fn fields(self) -> (Der<T, N>, usize, HashSet<T>) {
        (self.der, self.dot, self.look)
    }

    // Get all derivations for the nonterminal right after the dot
    // or an empty vector if the dotted symbol is terminal
    pub fn get_sub_derivations(&self, g: &Grammar<T, N>) -> Vec<DottedDer<T, N>>
    where
        N: Clone + PartialEq,
        T: Clone + PartialEq,
    {
        let after_dot = self.dotted_sym();
        match after_dot {
            Some(Symbol::NonTm(n)) => g
                .get_derivations_for(n)
                .iter()
                .map(|der| DottedDer::new(der.clone()))
                .collect(),
            Some(Symbol::Tm(_)) | None => vec![],
        }
    }
}
impl<T: Display + Eq + Hash + Ord + Clone, N: Display> Display for DottedDer<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}, {}",
            self.der.from,
            self.der
                .to
                .iter()
                .map(|sym| sym.to_string())
                .enumerate()
                .fold(String::new(), |acc, (i, s)| acc
                    + if i == self.dot { "." } else { "" }
                    + &s)
                + if self.dot == self.der.to.len() {
                    "."
                } else {
                    ""
                },
            "[".to_string()
                + &{
                    let mut sorted = self
                        .look
                        .iter()
                        .map(|t| t.clone())
                        .collect::<Vec<T>>();
                    sorted.sort();
                    sorted
                }
                    .into_iter()
                    .enumerate()
                    .map(|(i, t)| if i == 0 { "" } else { ", " }.to_owned() + &t.to_string())
                    .fold(String::new(), |acc, new| acc + &new)
                + "]"
        )
    }
}
impl<T: Display + Eq + Hash + Ord + Clone, N: Display> Debug for DottedDer<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

pub enum Action<N> {
    Shift(usize),     // new state
    Reduce(N, usize), // Reduction rule and length
    Multi(Vec<Action<N>>),
}
impl<N: Display> Display for Action<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Action::Shift(s) => write!(f, "s{}", s),
            Action::Reduce(_, l) => write!(f, "r{}", l),
            Action::Multi(actions) => write!(f, "{:?}", actions),
        }
    }
}
impl<N: Display> Debug for Action<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
pub fn print_action_table<T, N>(states: &Vec<State<T, N>>, ns: Vec<N>, ts: Vec<T>)
where
    T: PartialEq + Eq + Hash + Display,
    N: PartialEq + Eq + Hash + Display,
{
    print!("\t");
    // Print header
    for t in &ts {
        print!("{}\t", t.to_string().blue());
    }
    for n in &ns {
        print!("{}\t", n.to_string().blue());
    }
    print!("\n");
    for (i, state) in states.iter().enumerate() {
        print!("{}\t", i.to_string().red());
        for t in &ts {
            if let Some(action) = state.next.get(&t) {
                print!("{}\t", action);
            } else {
                print!("\t");
            }
        }
        for n in &ns {
            if let Some(goto) = state.goto.get(&n) {
                print!("g{}\t", goto);
            } else {
                print!("\t");
            }
        }
        print!("\n");
    }
}

#[derive(Copy, Clone, Debug)]
enum ChildGraphIndex {
    GraphInd(usize),
    StackInd(usize),
}

pub struct State<T, N>
where
    T: PartialEq + Eq + Hash,
    N: PartialEq,
{
    pub ders: Vec<DottedDer<T, N>>,
    pub next: HashMap<T, Action<N>>,
    pub goto: HashMap<N, usize>,
}
impl<T: Clone + Display + PartialEq + Eq + Hash + Ord, N: Clone + Display + PartialEq + Eq + Hash>
    State<T, N>
{
    
    // Recursively replace all ChildGraphIndex::StackInd(from) in state_ders with ChildGraphIndex::GraphInd(to)
    // starting with the children at state_ders[to]
    fn replace_stack_ptr(from: usize, to: usize, state_ders: &mut Vec<(DottedDer<T, N>, Vec<ChildGraphIndex>)>) {
        if to >= state_ders.len() {
            error!("Tried to replace stack pointers starting at index {}, but there are only {} ders", to, state_ders.len());
            panic!();
        }
        // Don't actually use recursion, but use a queue with a checked set
        let mut checked = HashSet::new();
        let mut queue = vec![to];
        let mut i = 0;
        while i < queue.len() {
            if !checked.contains(&queue[i]) {
                let g_ind = queue[i];
                state_ders[g_ind].1 = state_ders[g_ind].1
                    .iter()
                    .map(|cgi| {
                        match cgi {
                            ChildGraphIndex::GraphInd(g_ind2) => {
                                queue.push(*g_ind2);
                                *cgi
                            },
                            ChildGraphIndex::StackInd(s_ind) => {
                                if *s_ind == from {
                                    ChildGraphIndex::GraphInd(to)
                                } else {
                                    *cgi
                                }
                            },
                        }
                    })
                    .collect();
                checked.insert(queue[i]);
            }
            i += 1;
        }
    }

    fn update_child_graph(der: &DottedDer<T, N>, state_ders: &mut Vec<(DottedDer<T, N>, Vec<ChildGraphIndex>)>, stack: &mut Vec<DottedDer<T, N>>, g: &Grammar<T, N>, should_print: bool) -> ChildGraphIndex {
        match state_ders
            .iter()
            .position(|(prev_der, _)| prev_der == der) {
                Some(g_ind) => {
                    return ChildGraphIndex::GraphInd(g_ind);
                },
                None => {},
            }
        match stack
            .iter()
            .position(|prev_der| der == prev_der) {
                Some(s_ind) => {
                    return ChildGraphIndex::StackInd(s_ind);
                },
                None => {},
            }
        stack.push(der.clone());
        let sub_ders = der.get_sub_derivations(g);
        let sub_der_indices = sub_ders
            .iter()
            .map(|sub_der|
                Self::update_child_graph(sub_der, state_ders, stack, g, should_print)
            )
            .collect();
        stack.pop();
        state_ders.push((der.clone(), sub_der_indices));
        Self::replace_stack_ptr(stack.len(), state_ders.len() - 1, state_ders);
        ChildGraphIndex::GraphInd(state_ders.len() - 1)
    }

    fn get_sub_der_graph(starting_syms: &Vec<DottedDer<T, N>>, g: &Grammar<T, N>, should_print: bool) -> Vec<(DottedDer<T, N>, Vec<ChildGraphIndex>)> {
        // Assume that none of the starting_syms are children of each other
        let mut state_ders = Vec::new();
        for der in starting_syms {
            Self::update_child_graph(der, &mut state_ders, &mut Vec::new(), g, should_print);
        }

        state_ders
    }

    // Add looks to children if children's dots are at the end
    // Repeat until all children have correct looks
    fn add_looks(state_ders: &mut Vec<(DottedDer<T, N>, Vec<usize>)>, firsts: &HashMap<N, HashSet<T>>) {

        // Continue pushing looks to children with dots at end until
        // there are no changes
        let mut change = true;
        while change {
            change = false;
            let mut syms_to_add = Vec::new();
            let mut looks_to_add = Vec::new();
            for (index, (_, sub_der_indices)) in state_ders.iter().enumerate() {
                for sub_der_index in sub_der_indices {
                    if *sub_der_index >= state_ders.len() {
                        error!("sub_der_index {} too big for state_ders.len() {}", sub_der_index, state_ders.len());
                        panic!();
                    }
                    match state_ders[index].0.next_dotted_sym() {
                        Some(Symbol::Tm(t)) => {
                            syms_to_add.push((*sub_der_index, {
                                let mut set = HashSet::new();
                                set.insert(t.clone());
                                set
                            }));
                        }
                        Some(Symbol::NonTm(n)) => {
                            // Paste first(n) into look
                            match firsts.get(&n) {
                                Some(first) => {
                                    syms_to_add.push((*sub_der_index, first.clone()))
                                }
                                None => {
                                    error!("No firsts for NonTM, {}", n);
                                    panic!();
                                }
                            }
                        }
                        None => {
                            looks_to_add.push((index, *sub_der_index));
                        }
                    }
                }
            }
            for (sub_der_index, syms) in syms_to_add {
                if state_ders[sub_der_index].0.add_looks(&syms) {
                    change = true;
                }
            }
            for (index, sub_der_index) in looks_to_add {
                let look = state_ders[index].0.look.clone();
                // Clone because look is a reference to state_ders
                if state_ders[sub_der_index].0.add_looks(&look) {
                    change = true;
                }
            }
        }
    }

    pub fn new(
        mut starting_syms: Vec<DottedDer<T, N>>,
        greater_look: &HashSet<T>,
        g: &mut Grammar<T, N>,
        firsts: &HashMap<N, HashSet<T>>,
    ) -> State<T, N>
    where
        T: Display,
        N: Display,
    {
        
        let mut should_print = false;
        if format!("{:?}", starting_syms) == "[S' -> .Is$, []]" {
            should_print = true;
        }
        let mut state = State {
            ders: Vec::new(),
            next: HashMap::new(),
            goto: HashMap::new(),
        };

        // Create child tree
        // Before you call add_looks, put greater_look into starting_syms if their dots are at the end
        for der in &mut starting_syms {
            if let None = der.dotted_sym() {
                der.add_looks(greater_look);
            }
        }
        let state_ders = Self::get_sub_der_graph(&starting_syms, g, should_print);

        // Replace ChildGraphIndex with usize
        let mut state_ders: Vec<(DottedDer<T, N>, Vec<usize>)> = match state_ders
            .into_iter()
            .map(|(der, cgis)| {
                Ok((der, cgis
                    .into_iter()
                    .map(|cgi| match cgi {
                        ChildGraphIndex::GraphInd(g_ind) => Ok(g_ind),
                        ChildGraphIndex::StackInd(s_ind) => Err(format!("Stack index {} still in state ders", s_ind)),
                    })
                    .collect::<Result<Vec<usize>, String>>()?
                ))
            })
            .collect::<Result<Vec<(DottedDer<T, N>, Vec<usize>)>, String>>() {
                Ok(sd) => sd,
                Err(msg) => {
                    error!("{}", msg);
                    panic!();
                }
            };


        // Add looks to child tree
        Self::add_looks(&mut state_ders, firsts);

        state.ders = state_ders
            .into_iter()
            .map(|(der, _)| der)
            .collect();
        state
    }

    pub fn add_next(&mut self, t: T, action: Action<N>)
    where
        T: Eq + Hash,
    {
        let existing_action = self.next.get_mut(&t);
        match existing_action {
            None => {
                self.next.insert(t, action);
            }
            Some(Action::Multi(actions)) => {
                actions.push(action);
            }
            Some(_) => {
                // So many bad things going on here!
                let old_action = self
                    .next
                    .insert(t.clone(), Action::Multi(vec![action]))
                    .unwrap();
                match self.next.get_mut(&t) {
                    Some(Action::Multi(actions)) => actions.insert(0, old_action),
                    _ => (),
                };
            }
        }
    }

    pub fn add_goto(&mut self, n: N, goto: usize)
    where
        N: Eq + Hash + Display,
    {
        let existing_goto = self.goto.get_mut(&n);
        match existing_goto {
            None => {
                self.goto.insert(n, goto);
            }
            Some(exist) => {
                error!(
                    "Cannot add goto from {} to {} on state, because state already has goto {}.",
                    n, goto, exist
                );
            }
        }
    }
}
impl<T: Display + PartialEq + Eq + Hash + Ord + Clone, N: Display + PartialEq> Display for State<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.ders)
    }
}
impl<T: PartialEq + Eq + Hash, N: PartialEq> PartialEq for State<T, N> {
    fn eq(&self, other: &Self) -> bool {
        self.ders == other.ders
    }
}

pub fn get_firsts_single<
    T: Eq + Clone + Display + Hash,
    N: Eq + Clone + Display + Hash
>(
    log_options: &LoggingOptions,
    g: &Grammar<T, N>,
    n: N,
    stack: &mut HashSet<N>,
    firsts: &mut HashMap<N, HashSet<T>>,
) -> Result<(), String> {
    if let Some(_) = firsts.get(&n) {
        // If the first for n2 is already calculated, no work to be done
        // This is the base case
        return Ok(());
    } else {
        firsts.insert(n.clone(), HashSet::new());
    }
    if log_options.print_firsts_actions {
        info!("Getting derivatons for {}", n.to_string().blue());
    }
    let ders = &g.get_derivations_for(&n);
    if log_options.print_firsts_actions {
        info!("Derivations for {}: {:?}", n.to_string().blue(), ders);
    }
    for der in ders {
        let sym = &der.to[0];
        match sym {
            Symbol::Tm(t) => {
                let first = firsts.get_mut(&der.from).unwrap();     // This will unwrap because we initialized it at the beginning of this function
                // The first of this derivation is just a terminal
                if log_options.print_firsts_actions {
                    info!("Adding {} to the list of firsts for {}", t.to_string().blue(), n.to_string().blue());
                }
                first.insert(t.clone());
            }
            Symbol::NonTm(n2) => {
                if stack.contains(n2) {
                    // Loop in grammar; just continue
                    if log_options.print_firsts_actions {
                        info!("First token of {}, {} already on stack, skipping", n.to_string().blue(), n2.to_string().blue());
                    }
                    continue;
                }
                // We need to calculate the first of n2 and insert it into
                // the first of n
                stack.insert(n.clone());
                if log_options.print_firsts_actions {
                    info!("First token of {} was nonterminal, so calling get_firsts_single on {}", n.to_string().blue(), n2.to_string().blue());
                }
                get_firsts_single(log_options, g, n2.clone(), stack, firsts)?;
                stack.remove(&n);
                let first2: Vec<T>;
                if let Some(f) = firsts.get(&n2) {
                    first2 = f
                        .iter()
                        .map(|t| t.clone())
                        .collect();
                } else {
                    return Err(format!("get_firsts_single subcall did not get firsts for {}", n2.to_string().blue()));
                }
                // Add all firsts from n2 to the firsts of n
                let first = firsts.get_mut(&der.from).unwrap();     // This will unwrap because we initialized it at the beginning of this function
                // info!("First for {} before extension: [{}]", n, first.iter().fold(String::new(), |s, t| s + &t.to_string() + ", "));
                first.extend(first2);
                // info!("First for {} after extension: [{}]", n, first.iter().fold(String::new(), |s, t| s + &t.to_string() + ", "));

            }
        }
    }
    // println!("{{\n{}}}", firsts
    //                 .iter()
    //                 .fold(String::new(), |s, (n, f)| s + "\"" + &n.to_string() + "\": [" + &f
    //                                                                     .iter()
    //                                                                     .fold(String::new(), |s2, f2| s2 + &f2.to_string() + ", ")
    //                                                                 + "],\n"
    //                 ));
    Ok(())
}

pub fn get_firsts<
    T: Eq + Clone + Display + Hash,
    N: Eq + Clone + Display + Hash
>(
    log_options: &LoggingOptions,
    g: &Grammar<T, N>,
    ns: &Vec<N>,
) -> Result<HashMap<N, HashSet<T>>, String> {
    // todo!("get_firsts is not working");
    let mut firsts = HashMap::new();
    // for n in ns {
    //     println!("Initializing for {}", n.to_string().blue());
    //     firsts.insert(n.clone(), HashSet::new());
    // }
    for n in ns {
        // println!("Getting firsts for {}", n.to_string().blue());
        get_firsts_single(log_options, g, n.clone(), &mut HashSet::new(), &mut firsts)?;
    }
    // let firsts = firsts
    //         .into_iter()
    //         .map(|(n, hs)| (n, hs.into_iter().collect()))
    //         .collect();
    Ok(firsts)
}

pub fn get_firsts_new<
    T: Eq + Clone + Display + Hash,
    N: Eq + Clone + Display + Hash,
>(
    _log_options: &LoggingOptions,
    g: &Grammar<T, N>,
    ns: &Vec<N>,
) -> Result<HashMap<N, HashSet<T>>, String> {
    let mut firsts: HashMap<N, HashSet<Symbol<T, N>>> = ns
        .iter()
        .map(|n|
            Ok((n.clone(), 
                g.get_derivations_for(n)
                    .iter()
                    .map(|der| {
                        match der.to.first() {
                            Some(sym) => Ok(sym.clone()),
                            None => Err(format!("Empty Derivation: {}", der.to_string().red()))
                        }
                    })
                    .collect::<Result<HashSet<Symbol<T, N>>, String>>()?
            ))
        )
        .collect::<Result<HashMap<N, HashSet<Symbol<T, N>>>, String>>()?;

    let mut non_ts = true;
    while non_ts {
        non_ts = false;
        for n in ns {
            let mut first = firsts.get(n).unwrap().clone();
            let n_as_sym = Symbol::NonTm(n.clone());
            first.remove(&n_as_sym);
            let mut first_new = HashSet::new();
            first
                .iter()
                .try_for_each(|sym| {
                    match sym {
                        Symbol::Tm(t) => { first_new.insert(Symbol::Tm(t.clone())); },
                        Symbol::NonTm(n2) => {
                            first_new.extend(match firsts.get(n2) {
                                Some(set) => set.clone(),
                                None => return Err(format!("NonTm {} in firsts but not in ns; while finding firsts for {}", n2.to_string().blue(), n.to_string().blue()))
                            });
                            non_ts = true;
                        }
                    }
                    Ok(())
                })?;
            first_new.remove(&n_as_sym);
            firsts.insert(n.clone(), first_new);
        }
    }

    firsts
        .into_iter()
        .try_fold(HashMap::new(), |mut map, (n, hs)| {
            let hs_new = hs
                .into_iter()
                .try_fold(HashSet::new(), |mut hs, sym| {
                    match sym {
                        Symbol::Tm(t) => {
                            hs.insert(t);
                        }
                        Symbol::NonTm(n) => {
                            return Err(format!("Error finding firsts, nonterminal {} not elimintated", n.to_string().yellow()));
                        }
                    }
                    Ok(hs)
                })?;
            map.insert(n, hs_new);
            Ok::<HashMap<N, HashSet<T>>, String>(map)
        })
}

pub fn print_firsts<
    T: Eq + Clone + Display + Hash,
    N: Eq + Clone + Display + Hash
>(firsts: &HashMap<N, HashSet<T>>) {
    debug!("{}", "Firsts: ".color("#ff7f00").bold());
    for (n, f) in firsts {
        debug!("{}: {}", n.to_string().blue(), f
                                            .iter()
                                            .fold(String::new(), |s, t| s + &t.to_string() + " ")
        );
    }
    debug!("End firsts");
}

pub fn print_firsts_as_syms<
    T: Eq + Clone + Display + Hash,
    N: Eq + Clone + Display + Hash
>(firsts: &HashMap<N, HashSet<Symbol<T, N>>>) {
    debug!("{}", "Firsts: ".color("#ff7f00").bold());
    for (n, f) in firsts {
        debug!("{}: {}", n.to_string().blue(), f
                                            .iter()
                                            .fold(String::new(), |s, sym| s + &sym.to_string() + " ")
        );
    }
    debug!("End firsts");
}

pub fn update_look_subseq<
    T: Display + Clone + Eq + Hash + Ord,
    N: Display + Clone + Eq + Hash,
>(
    firsts: &HashMap<N, HashSet<T>>,
    all_similar: &mut Vec<DottedDer<T, N>>
) {
    let mut to_add = Vec::new();
    // Check if any derivation is a subsequence of another
    for i in 0..all_similar.len() {
        let der1 = &all_similar[i];
        for j in i+1..all_similar.len() {
            let der2 = &all_similar[j];

            // Check if der1 is a subsequence of der2 or vice versa
            match is_subsequence(&der1, &der2) {
                SubseqData::Less(sym) => {
                    // Add FIRST of sym to der1.look
                    match sym {
                        Symbol::Tm(t) => {
                            to_add.push((i, {
                                let mut set = HashSet::new();
                                set.insert(t);
                                set
                            }));
                        }
                        Symbol::NonTm(n) => {
                            match firsts.get(&n) {
                                Some(first) => {
                                    to_add.push((i, first
                                        // .iter()
                                        // .map(|t| t.clone())
                                        // .collect()
                                        .clone()
                                    ));
                                }
                                None => {
                                    error!("First set does not contain firsts for {}", n.to_string().blue());
                                    panic!();
                                }
                            }
                        }
                    }
                }
                SubseqData::Greater(sym) => {
                    match sym {
                        Symbol::Tm(t) => {
                            to_add.push((j, {
                                let mut set = HashSet::new();
                                set.insert(t);
                                set
                            }));
                        }
                        Symbol::NonTm(n) => {
                            match firsts.get(&n) {
                                Some(first) => {
                                    to_add.push((j, first.clone()));
                                }
                                None => {
                                    error!("First set does not contain firsts for {}", n.to_string().blue());
                                    panic!();
                                }
                            }
                        }
                    }

                }
                SubseqData::NotEq => {
                    // Not a subsequence
                }
                SubseqData::Equal => {
                    // todo!()
                }
            }
        }
    }
    for (i, syms) in to_add {
        all_similar[i].add_looks(&syms);
    }
    
}

pub fn lr1_generate<
    T: PartialEq + Eq + Clone + Display + Hash + Ord,
    N: PartialEq + Eq + Clone + Display + Hash,
>(
    log_options: &LoggingOptions,
    g: &mut Grammar<T, N>,
    start: N,
    ns: Vec<N>,
    ts: Vec<T>,
) -> Result<Vec<State<T, N>>, String> {
    let firsts = get_firsts_new(log_options, g, &ns)?;
    if log_options.print_firsts {
        print_firsts(&firsts);
    }
    let start_ders = g.get_derivations_for(&start);
    if start_ders.len() >= 2 {
        return Err(format!("Multiple starting derivations: {:?}", start_ders));
    }
    if start_ders.len() == 0 {
        return Err(format!(
            "No starting derivation found; looking for {}",
            start
        ));
    }
    let start_der = start_ders.into_iter().next().unwrap();
    let state0 = State::new(vec![DottedDer::new(start_der)], &HashSet::new(), g, &firsts);
    let mut states = vec![state0];
    let mut cur_state;
    let mut i = 0;
    if log_options.print_state_transitions {
        debug!("{}", "State Transitions:".color("#ff7f00").bold());
    }
    while i < states.len() {
        // Handle 1 state
        cur_state = &mut states[i];
        let mut states_to_add = Vec::new();
        let mut queue = cur_state.ders.clone();
        while let Some(cur_der) = queue.pop() {
            let cloned_der = cur_der.clone();
            let (der, dot, look) = cur_der.fields();
            if dot >= der.to.len() {
                // Completed derivation (dot at end)
                // Put a reduction at every column in the lookahead
                for sym in &look {
                    let action = Action::Reduce(der.from.clone(), der.to.len());
                    cur_state.add_next(sym.clone(), action);
                }
                continue;
            } else {
                let sym = &der.to[dot];
                // Find all derivations that would advance on the same symbol as der
                let mut all_similar = Vec::new();
                all_similar.append(
                    &mut queue
                        .iter()
                        .filter(|der| {
                            der.dotted_sym() == Some(sym)
                        })
                        .map(|der| der.clone())
                        .collect(),
                );
                queue.retain(|der| der.dotted_sym() != Some(sym));
                all_similar.push(cloned_der);
                
                update_look_subseq(&firsts, &mut all_similar);
                let mut new_state = State::new(
                    all_similar.into_iter().map(|der| der.advance()).collect(),
                    &look,
                    g,
                    &firsts
                );
                update_look_subseq(&firsts, &mut new_state.ders);
                states_to_add.push((new_state, der.to.into_iter().nth(dot).unwrap()));
            };
        }
        for (new_state, sym) in states_to_add {
            match sym {
                Symbol::Tm(t) => {
                    let action: Action<N>;
                    let shift_to;
                    let index_opt = states.iter().position(|state| *state == new_state);
                    if let Some(index) = index_opt {
                        action = Action::Shift(index);
                        shift_to = index;
                    } else {
                        action = Action::Shift(states.len());
                        shift_to = states.len();
                        states.push(new_state);
                    }
                    if log_options.print_state_transitions {
                        debug!(
                            "{}: {} -> {}",
                            t.to_string().blue(),
                            i.to_string().red(),
                            shift_to.to_string().red()
                        );
                    }
                    states[i].add_next(t, action);
                }
                Symbol::NonTm(n) => {
                    let goto;
                    let index_opt = states.iter().position(|state| *state == new_state);
                    if let Some(index) = index_opt {
                        goto = index;
                    } else {
                        goto = states.len();
                        states.push(new_state);
                    }
                    if log_options.print_state_transitions {
                        debug!("Goto {} on {}", goto, n);
                    }
                    states[i].add_goto(n, goto);
                }
            }
        }
        i += 1;
    }
    if log_options.print_state_transitions {
        debug!("End state transitions");
    }
    if log_options.print_states {
        debug!("{}", "States:".color("#ff7f00").bold());
        for (i, state) in states.iter().enumerate() {
            debug!("{}: {}", i.to_string().red(), state);
        }
        debug!("End states");
    }
    if log_options.print_action_table {
        debug!("{}", "Action Table:".color("#ff7f00").bold());
        print_action_table(&states, ns, ts);
        debug!("End action table");
    }
    Ok(states)
}

macro_rules! grammar {
    (
        $term_name:ident : {
            $( $(#[$meta:meta])* $terminal:ident $(($($term_param:ty),*))?),+ $(,)?
        },
        $nonterm_name:ident : {
            $($non_terminal:ident),+ $(,)?
        },
        $grammarfunc_name:ident : {
            $($lhs:ident => $($rhs:tt),+);+ $(;)?
        }
    ) => {
        #[derive(logos::Logos, ::std::fmt::Debug, ::std::cmp::PartialEq, ::std::cmp::Eq, ::std::clone::Clone, ::std::hash::Hash, ::std::cmp::Ord, ::std::cmp::PartialOrd)]
        pub enum $term_name {
            $( $(#[$meta])* $terminal $(($($term_param),*))?),+,
        }
        #[derive(::std::cmp::PartialEq, ::std::cmp::Eq, ::std::clone::Clone, ::std::hash::Hash)]
        pub enum $nonterm_name {
            $($non_terminal),+
        }

        pub fn $grammarfunc_name() -> grammar::Grammar<$term_name, $nonterm_name> {
            grammar::Grammar::new(
                    proc_macros::get_ders!{$term_name; $nonterm_name; $($terminal),+; $($non_terminal),+; $($lhs $($rhs),+);+;}
            )
        }
    };
}
pub(crate) use grammar;
use log::info;

use crate::logging::LoggingOptions;
