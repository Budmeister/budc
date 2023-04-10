use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use colored::Colorize;

#[derive(Clone, PartialEq, Hash)]
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
    for der in &g.derivations {
        println!(
            "{} -> {}",
            der.from,
            der.to
                .iter()
                .map(|sym| sym.to_string())
                .fold(String::new(), |acc, s| acc + &s),
        );
    }
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

    pub fn add_looks(&mut self, other_looks: &HashSet<T>)
    where
        T: Clone,
    {
        let mut iter = other_looks.iter();
        while let Some(t) = iter.next() {
            self.look.insert(t.clone());
        }
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

    // Get all derivations that we must include as a result of
    // a nonterminal being right after the dot
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
impl<T: Display + Eq + Hash, N: Display> Display for DottedDer<T, N> {
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
                + &self
                    .look
                    .iter()
                    .enumerate()
                    .map(|(i, t)| if i == 0 { "" } else { ", " }.to_owned() + &t.to_string())
                    .fold(String::new(), |acc, new| acc + &new)
                + "]"
        )
    }
}
impl<T: Display + Eq + Hash, N: Display> Debug for DottedDer<T, N> {
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

// pub type State<T, N> = Vec<DottedDer<T, N>>;
// #[derive(PartialEq)]
pub struct State<T, N>
where
    T: PartialEq + Eq + Hash,
    N: PartialEq,
{
    pub ders: Vec<DottedDer<T, N>>,
    pub next: HashMap<T, Action<N>>,
    pub goto: HashMap<N, usize>,
}
impl<T: Clone + Display + PartialEq + Eq + Hash, N: Clone + Display + PartialEq + Eq + Hash>
    State<T, N>
{
    // Create new state from initial derivations
    // Automatically include sub-derivations
    pub fn new(
        starting_syms: Vec<DottedDer<T, N>>,
        greater_look: &HashSet<T>,
        g: &mut Grammar<T, N>,
    ) -> State<T, N>
    where
        T: Display,
        N: Display,
    {
        let mut queue: Vec<(DottedDer<T, N>, HashSet<T>)> = starting_syms
            .iter()
            .map(|der| (der.clone(), greater_look.clone()))
            .collect();
        let mut state = State {
            ders: Vec::new(),
            next: HashMap::new(),
            goto: HashMap::new(),
        };
        // Keep filling state.ders with sub-derivations until there are no more
        // Potential infinite loop here
        while let Some((cur_der, cur_greater_look)) = queue.pop() {
            // println!("State ders: {:?}, cur_der: {}, queue: {:?}", state.ders, cur_der, queue);
            state.ders.push(cur_der.clone());
            let sub_ders = cur_der.get_sub_derivations(g);
            let mut look = HashSet::new();
            match cur_der.next_dotted_sym() {
                Some(Symbol::Tm(t)) => {
                    look.insert(t.clone());
                }
                Some(Symbol::NonTm(n)) => {
                    // Paste first(n) into look
                    // println!("Getting first({}) from grammar", n);
                    look.extend(g.first(&n, None));
                }
                None => {
                    // Dot at end; paste greater_der into looks
                    look.extend(cur_greater_look.clone());
                }
            }
            for mut sub_der in sub_ders {
                let mut should_revisit = true;
                for (prev_der, _) in &mut queue {
                    if sub_der.equals_ignore_look(prev_der) {
                        prev_der.add_looks(&mut look);
                        should_revisit = false;
                        break;
                    }
                }
                for prev_der in &mut state.ders {
                    if sub_der.equals_ignore_look(prev_der) {
                        prev_der.add_looks(&mut look);
                        should_revisit = false;
                        break;
                    }
                }
                if should_revisit {
                    sub_der.add_looks(&look);
                    queue.push((sub_der, look.clone()));
                }
            }
            // queue.append(
            //     &mut sub_ders
            //         .into_iter()
            //         .map(|mut der| {
            //             der.add_looks(&mut look.clone());
            //             der
            //         })
            //         .filter(|der| !state.ders.contains(der) && !queue.contains(der))
            //         .collect(),
            // );
        }
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
                println!(
                    "Cannot add goto from {} to {} on state, because state already has goto {}.",
                    n, goto, exist
                );
            }
        }
    }
}
impl<T: Display + PartialEq + Eq + Hash, N: Display + PartialEq> Display for State<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.ders)
    }
}
impl<T: PartialEq + Eq + Hash, N: PartialEq> PartialEq for State<T, N> {
    fn eq(&self, other: &Self) -> bool {
        self.ders == other.ders
    }
}

pub fn lr1_generate<
    T: PartialEq + Eq + Clone + Display + Hash,
    N: PartialEq + Eq + Clone + Display + Hash,
>(
    g: &mut Grammar<T, N>,
    start: N,
    ns: Vec<N>,
    ts: Vec<T>,
) -> Result<Vec<State<T, N>>, String> {
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
    let state0 = State::new(vec![DottedDer::new(start_der)], &HashSet::new(), g);
    let mut states = vec![state0];
    let mut cur_state;
    let mut i = 0;
    while i < states.len() {
        // Handle 1 state
        cur_state = &mut states[i];
        let mut states_to_add = Vec::new();
        // println!("Creating child states for state {}: {}", i, cur_state);
        let mut queue = cur_state.ders.clone();
        while let Some(cur_der) = queue.pop() {
            // let cur_sym = cur_der.dotted_sym();
            let cloned_der = cur_der.clone();
            let (der, dot, look) = cur_der.fields();
            if dot >= der.to.len() {
                // println!("Not handling derivation for completion: {}", cur_der);
                // Completed derivation (dot at end)
                // Put a reduction at every column in the lookahead
                for sym in &look {
                    let action = Action::Reduce(der.from.clone(), der.to.len());
                    cur_state.add_next(sym.clone(), action);
                }
                continue;
            } else {
                let sym = &der.to[dot];
                // println!("Handling derivation: {}", cur_der);
                // Find all derivations that would advance on the same symbol as der
                let mut all_similar = Vec::new();
                all_similar.append(
                    &mut queue
                        .iter()
                        .filter(|der| der.dotted_sym() == Some(sym))
                        .map(|der| der.clone())
                        .collect(),
                );
                queue.retain(|der| der.dotted_sym() != Some(sym));
                all_similar.push(cloned_der);
                // println!("All similar derivations: {:?}", all_similar);

                let new_state = State::new(
                    all_similar.into_iter().map(|der| der.advance()).collect(),
                    &look,
                    g,
                );
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
                        // println!("State already exists: {}", new_state);
                        action = Action::Shift(index);
                        shift_to = index;
                    } else {
                        // println!("Creating new state {}: {}", states.len(), new_state);
                        action = Action::Shift(states.len());
                        shift_to = states.len();
                        states.push(new_state);
                    }
                    println!(
                        "{}: {} -> {}",
                        t.to_string().blue(),
                        i.to_string().red(),
                        shift_to.to_string().red()
                    );
                    states[i].add_next(t, action);
                    // println!("");
                    // if states.len() >= 30 {
                    //     return Err("Too many states".to_string());
                    // }
                }
                Symbol::NonTm(n) => {
                    let goto;
                    let index_opt = states.iter().position(|state| *state == new_state);
                    if let Some(index) = index_opt {
                        // println!("State already exists: {}", new_state);
                        goto = index;
                    } else {
                        // println!("Creating new state {}: {}", states.len(), new_state);
                        goto = states.len();
                        states.push(new_state);
                    }
                    println!("Goto {} on {}", goto, n);
                    states[i].add_goto(n, goto);
                }
            }
        }
        i += 1;
    }
    for (i, state) in states.iter().enumerate() {
        println!("{}: {}", i.to_string().red(), state);
    }
    print_action_table(&states, ns, ts);
    Ok(states)
}


#[macro_export]
macro_rules! parse_symbol {
    (true, false, $rhs:ident) => {
        Node::Tm($rhs)
    };
    (false, true, $rhs:ident) => {
        Node::NonTm($rhs)
    };
    (true, true, $rhs:ident) => {
        compile_error!("Terminal and non-terminal token: {}", $rhs);
    };
    (false, false, $rhs:ident) => {
        compile_error!("Invalid token: {}", $rhs);
    };
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
        #[derive(logos::Logos, ::std::fmt::Debug, ::std::cmp::PartialEq, ::std::cmp::Eq, ::std::clone::Clone, ::std::hash::Hash)]
        pub enum $term_name {
            $( $(#[$meta])* $terminal $(($($term_param),*))?),+,
        }
        pub enum $nonterm_name {
            $($non_terminal),+
        }

        pub fn $grammarfunc_name() -> grammar::Grammar<$term_name, $nonterm_name> {
            grammar::Grammar::new(
                    proc_macros::get_ders!{$term_name; $nonterm_name; $($terminal),+; $($non_terminal),+; $($lhs $($rhs),+);+}
            )
        }
    };
}
pub(crate) use grammar;
