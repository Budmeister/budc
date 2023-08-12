//! The function environment holds lots of information about the function
//! and stack frame. For example
//! * The literal strings and the labels that point to them
//! * All variables, their types, and their locations on the stack frame
//! * The mapping of temp registers to physical registers
//! * The current height of the stack beyond its starting point
//! 
//! `FunctionEnvironment` gives many functions for converting between
//! various types of locations (`Place`, `AddrMode`, `DTemp`, `DReg`, etc.).
//! 
//! Author:     Brian Smith
//! Year:       2023

use std::{collections::{HashMap, HashSet}, ops::{RangeFrom, Range}};

use Proxy::*;
use log::{error, debug};

use crate::{m68k::*, c_err, error::*};

use super::instruction::*;

use ADReg::*;

use AReg::FP;

#[derive(Hash, Clone, Eq, PartialEq, Debug)]
pub enum StackItem {
    Var(String),
    DTemp(DTemp),
    ATemp(ATemp),
    PC, // Program Counter
    FP, // Frame Pointer
}
impl StackItem {
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }
    pub fn is_dtemp(&self) -> bool {
        matches!(self, Self::DTemp(_))
    }
    pub fn is_atemp(&self) -> bool {
        matches!(self, Self::ATemp(_))
    }
}

pub struct StackFrame {
    pub frame_map: HashMap<StackItem, StackHeight>,
    /// The address of the top of the top item
    pub ceiling: StackHeight,
    /// The amount of stack height we are responsible for (is not provided by the caller)
    pub responsible_stack_height: StackHeight,
}
impl StackFrame {
    pub fn get(&self, item: &StackItem) -> Option<&StackHeight> {
        self.frame_map.get(item)
    }
    pub fn get_as_addr_mode(&self, item: &StackItem) -> Option<AddrMode> {
        self.frame_map.get(item).map(|sh| {
            AddrMode::AIndDisp((*sh).into(), FP)
        })
    }
    pub fn get_rsh(&self) -> StackHeight {
        self.responsible_stack_height
    }
}

/// The bool represents if the DReg is live or not. If not,
/// changes to the DReg do not affect the original Place
type Proxied = (DReg, bool);
/// The bool represents if the AReg is live or not. If not,
/// changes to the AReg do not affect the original Place
type ProxiedAddr = (AReg, bool);

/// Represents a relative stack index (usually positive but can be negative)
pub type StackHeight = i32;

pub struct FunctionEnvironment {
    pub lit_strings: Vec<(usize, String)>,
    pub vars: Vec<Field>,
    name: Field,
    dtemp_map: HashMap<DTemp, Option<DReg>>,
    atemp_map: HashMap<ATemp, Option<AReg>>,
    pub stack_frame: StackFrame,
    pub label_gen: RangeFrom<usize>,
}
impl FunctionEnvironment {
    pub fn new(
        signature: Signature,
        lit_strings: Vec<(usize, String)>,
        vars: Vec<Field>,
        dtemp_map: HashMap<DTemp, Option<DReg>>,
        atemp_map: HashMap<ATemp, Option<AReg>>,
        label_gen: RangeFrom<usize>,
        env: &Environment,
    ) -> Result<FunctionEnvironment, CompilerErr> {
        let stack_frame =
            Self::generate_stack_frame(&signature, &vars, &dtemp_map, &atemp_map, env)?;
        Ok(FunctionEnvironment {
            lit_strings,
            vars,
            name: signature.name,
            dtemp_map,
            atemp_map,
            label_gen,
            stack_frame,
        })
    }
    /// Generates the stack frame. The stack frame is organized like this:
    /// ```txt
    ///                                         higher addresses
    /// +-----------------------------------+  <--- ceiling
    /// |               params              |
    /// +-----------------------------------+
    /// |                 PC                |
    /// +-----------------------------------+
    /// |          old frame pointer        |
    /// +-----------------------------------+  <--- frame pointer (FP)
    /// |            spilled regs           |      |
    /// +-----------------------------------+      | responsible_stack_height (positive value)
    /// |             local vars            |      |
    /// +-----------------------------------+  <---/
    /// |    saved regs for function call   |
    /// +-----------------------------------+
    ///                                         lower addresses
    /// ```
    fn generate_stack_frame(
        signature: &Signature,
        vars: &[Field],
        dtemp_map: &HashMap<DTemp, Option<DReg>>,
        atemp_map: &HashMap<ATemp, Option<AReg>>,
        env: &Environment,
    ) -> Result<StackFrame, CompilerErr> {
        
        fn add_stack_height(height: &mut StackHeight, add: StackHeight) {
            *height += add;
            if *height % 2 == 1 {
                *height += 1;
            }
        }

        // Center on the frame pointer
        // Top half
        let mut stack_height: StackHeight = 0;
        let params = signature.args.iter().cloned().collect::<HashSet<Field>>();
        let mut frame_map = HashMap::new();
        let mut ceiling;

        // Add caller's FP to the stack frame
        let item = StackItem::FP;
        frame_map.insert(item, stack_height);
        add_stack_height(&mut stack_height, 4);

        // Add caller's PC to the stack frame
        let item = StackItem::PC;
        frame_map.insert(item, stack_height);
        add_stack_height(&mut stack_height, 4);
        ceiling = stack_height;

        // Add params to stack frame
        for param in &signature.args {
            let size = match param.tt.get_size(env, None) {
                Ok(size) => size,
                Err(err) => {
                    error!("Unable to find size of local variable {} {}", param.tt, param.name);
                    return Err(err);
                }
            };
            let item = StackItem::Var(param.name.clone());
            frame_map.insert(item, stack_height);
            add_stack_height(&mut stack_height, size as StackHeight);
            ceiling = stack_height;
        }

        // Bottom half
        stack_height = 0;
        // Add local vars to stack frame
        for var in vars.iter().rev() {
            if params.contains(var) {
                continue;
            }
            let size = match var.tt.get_size(env, None) {
                Ok(size) => size,
                Err(err) => {
                    error!("Unable to find size of local variable {} {}", var.tt, var.name);
                    return Err(err);
                }
            };
            add_stack_height(&mut stack_height, -(size as StackHeight));
            let item = StackItem::Var(var.name.clone());
            frame_map.insert(item, stack_height);
        }

        let responsible_stack_height = -stack_height;

        // Add spilled regs to stack frame
        for (dtemp, dreg) in dtemp_map {
            if dreg.is_some() {
                // Only add spilled regs to the stack frame
                continue;
            }
            // We are just going to assume all dregs are 4 bytes, even though they could be 2 or 1
            let size = 4;
            add_stack_height(&mut stack_height, -(size as StackHeight));
            let item = StackItem::DTemp(*dtemp);
            frame_map.insert(item, stack_height);
        }
        for (atemp, areg) in atemp_map {
            if areg.is_some() {
                continue;
            }
            let size = 4;
            add_stack_height(&mut stack_height, -(size as StackHeight));
            let item = StackItem::ATemp(*atemp);
            frame_map.insert(item, stack_height);
        }

        Ok(StackFrame { frame_map, ceiling, responsible_stack_height })
    }
    /// Returns an addressing mode that points to this var exactly as if 
    /// `place_to_addr_mode` was called. The difference is that the caller
    /// does not need to know the type
    /// 
    /// This function is intended for finding the address of a variable and
    /// loading it using `Lea`. The returned AddrMode will always be
    /// AIndDisp and will therefore be a valid source for `Lea`
    pub fn var_as_addr_mode(
        &self,
        name: String,
    ) -> Result<AddrMode, CompilerErr> {
        self.stack_item_to_addr_mode(&StackItem::Var(name))
    }
    /// Returns an addressing mode that points to the given place.
    ///
    /// The returned AddrMode is always live, but it may use a proxy if the
    /// given Place is Ref.
    /// 
    /// If the given Place is not Ref, then the proxy will not be used.
    pub fn place_to_addr_mode(
        &self,
        place: Place,
        range: Range<usize>,
        instrs: &mut Vec<ValidInstruction>,
        n: Proxy,
    ) -> Result<AddrMode, CompilerErr> {
        match place {
            Place::Var(name) => self.var_as_addr_mode(name.name),
            Place::ATemp(atemp) => match self.atemp_map.get(&atemp).unwrap() {
                Some(areg) => Ok(AddrMode::A(*areg)),
                None => Ok(self.stack_item_to_addr_mode(&StackItem::ATemp(atemp))?),
            },
            Place::DTemp(dtemp, tt) => {
                if tt.is_array() || tt.is_struct() {
                    return c_err!(
                        range,
                        "Array or struct {} being stored in dtemp D{}",
                        tt, dtemp
                    );
                }
                match self.dtemp_map.get(&dtemp).unwrap() {
                    Some(dreg) => Ok(AddrMode::D(*dreg)),
                    None => self.stack_item_to_addr_mode(&StackItem::DTemp(dtemp)),
                }
            }
            Place::Ref(atemp, dtemp, off, _tt) => {
                // What do we do with tt?
                let (areg, _) = self.atemp_as_areg(atemp, instrs, n)?;
                match dtemp {
                    Some(dtemp) => {
                        let (dreg, _) = self.dtemp_as_dreg(dtemp, instrs, n)?;
                        Ok(AddrMode::AIndIdxDisp(NumOrLbl::Num(off), areg, D(dreg)))
                    }
                    None => {
                        if off != 0 {
                            Ok(AddrMode::AIndDisp(NumOrLbl::Num(off), areg))
                        } else {
                            Ok(AddrMode::AInd(areg))
                        }
                    }
                }
            }
        }
    }
    /// The returned AddrMode is always live. It will always be AIndDisp
    pub fn stack_item_to_addr_mode(&self, item: &StackItem) -> Result<AddrMode, CompilerErr> {
        if let Some(addr_mode) = self.stack_frame.get_as_addr_mode(item) {
            return Ok(addr_mode);
        }

        // Else, must be DTemp or ATemp that wasn't spilled
        match item {
            StackItem::DTemp(dtemp) => match self.dtemp_map.get(&dtemp) {
                Some(Some(dreg)) => Ok((*dreg).into()),
                _ => c_err!("DTemp {} not in dtemp_map or stack_frame", dtemp),
            }
            StackItem::ATemp(atemp) => match self.atemp_map.get(&atemp) {
                Some(Some(areg)) => Ok((*areg).into()),
                _ => c_err!("ATemp {} not in atemp_map or stack_frame", atemp),
            }
            _ => c_err!("StackItem {:?} not found on stack", item),
        }
    }
    pub fn temps_to_adbitfield(&self, temps: &[ADTemp]) -> Result<ADBitField, CompilerErr> {
        // There should be no instrs added to dummy_instrs because it should only be passed
        // in where no instrs are needed
        let mut dummy_instrs = Vec::new();
        let n = Proxy1;
        let adbf = ADBitField::new(&temps
            .iter()
            .filter_map(|temp| {
                match temp {
                    ADTemp::A(atemp) => {
                        if self.atemp_is_areg(*atemp) {
                            // This is Rust like I've never seen!
                            Some((|| Ok(A(self.atemp_as_areg(*atemp, &mut dummy_instrs, n)?.0)))())
                        } else {
                            None
                        }
                    }
                    ADTemp::D(dtemp) => {
                        if self.dtemp_is_dreg(*dtemp) {
                            Some((|| Ok(D(self.dtemp_as_dreg(*dtemp, &mut dummy_instrs, n)?.0)))())
                        } else {
                            None
                        }
                    }
                }
            })
            .collect::<Result<Vec<ADReg>, CompilerErr>>()?
        );
        if !dummy_instrs.is_empty() {
            return c_err!("Trying to save temps that aren't in regs: {:?}", temps);
        }
        Ok(adbf)
    }
    /// Returns true precicely when a call to `place_to_dreg` would return a live DReg.
    pub fn place_is_dreg(
        &self,
        place: &Place
    ) -> bool {
        if let Place::DTemp(dtemp, _) = place {
            self.dtemp_map.contains_key(dtemp)
        } else {
            false
        }
    }
    /// Returns true precicely when a call to `place_to_areg` would return a live AReg.
    pub fn place_is_areg(
        &self,
        place: &Place
    ) -> bool {
        if let Place::ATemp(atemp) = place {
            self.atemp_map.contains_key(atemp)
        } else {
            false
        }
    }
    /// Returns true if the DReg is live. Otherwise, modifications to the value do not affect the original Place.
    /// 
    /// The DReg is live if and only if the proxy was used. 
    pub fn place_to_dreg(
        &self,
        place: Place,
        range: Range<usize>,
        instrs: &mut Vec<ValidInstruction>,
        env: &Environment,
        n: Proxy,
    ) -> Result<Proxied, CompilerErr> {
        match place {
            Place::Var(name) => {
                if name.tt.is_array() || name.tt.is_struct() {
                    return c_err!(
                        "Cannot move array or struct {} into data reg",
                        name
                    );
                }
                if name.tt.is_void() {
                    return c_err!(
                        "Cannot move void into data reg"
                    );
                }
                let proxy: DReg = n.into();
                let size = name.tt.get_data_size(env, Some(&range))?.unwrap();
                let from = self.stack_item_to_addr_mode(&StackItem::Var(name.name))?;
                let to = proxy.into();
                let instr = Instruction::Move(size, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
            Place::ATemp(atemp) => {
                let proxy: DReg = n.into();
                let from = self.atemp_to_addr_mode(atemp)?;
                let to = proxy.into();
                let size = DataSize::LWord;
                let instr = Instruction::Move(size, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
            Place::DTemp(dtemp, _tt) => {
                // What do we do with tt?
                self.dtemp_as_dreg(dtemp, instrs, n)
            }
            Place::Ref(atemp, dtemp, off, tt) => {
                if tt.is_array() || tt.is_struct() {
                    return c_err!(
                        "Cannot move value of type {} into data reg",
                        tt
                    );
                }
                let areg = self.atemp_as_areg(atemp, instrs, n)?.0;
                let dreg = self.opt_dtemp_as_opt_dreg(dtemp, instrs, n)?
                        .map(|dreg| dreg.0);
                let size = tt.get_data_size(env, Some(&range))?.unwrap();
                let from = (off, areg, dreg).into();
                let proxy: DReg = n.into();
                let to = proxy.into();
                let instr = Instruction::Move(size, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            },
        }
    }
    /// Returns true if the AReg is live. Otherwise, modifications to the value do not affect the original Place.
    /// 
    /// The AReg is live if and only if the proxy was used. 
    pub fn place_to_areg(
        &self,
        place: Place,
        range: Range<usize>,
        instrs: &mut Vec<ValidInstruction>,
        env: &Environment,
        n: Proxy
    ) -> Result<ProxiedAddr, CompilerErr> {
        match place {
            Place::Var(name) => {
                if name.tt.is_array() || name.tt.is_struct() {
                    return c_err!(
                        "Cannot move array or struct {} into addr reg",
                        name
                    );
                }
                let proxy: AReg = n.into();
                let size = name.tt.get_data_size(env, Some(&range))?.unwrap();
                let from = self.stack_item_to_addr_mode(&StackItem::Var(name.name))?;
                let to = proxy.into();
                let instr = Instruction::Move(size, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
            Place::ATemp(atemp) => {
                self.atemp_as_areg(atemp, instrs, n)
            }
            Place::DTemp(dtemp, tt) => {
                let proxy: AReg = n.into();
                let size = tt.get_data_size(env, Some(&range))?.unwrap();
                let from = self.dtemp_to_addr_mode(dtemp)?;
                let to = proxy.into();
                let instr = Instruction::Move(size, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
            Place::Ref(atemp, dtemp, off, tt) => {
                if tt.is_array() || tt.is_struct() {
                    return c_err!(
                        "Cannot move array or struct {} into data reg",
                        tt
                    );
                }
                let areg = self.atemp_as_areg(atemp, instrs, n)?.0;
                let dreg = self.opt_dtemp_as_opt_dreg(dtemp, instrs, n)?
                        .map(|dreg| dreg.0);
                let size = tt.get_data_size(env, Some(&range))?.unwrap();
                let from = (off, areg, dreg).into();
                let proxy: AReg = n.into();
                let to = proxy.into();
                let instr = Instruction::Move(size, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            },

        }
    }
    fn dtemp_to_addr_mode(&self, dtemp: DTemp) -> Result<AddrMode, CompilerErr> {
        match self.dtemp_map.get(&dtemp) {
            Some(Some(dreg)) => Ok((*dreg).into()),
            Some(None) => self.stack_item_to_addr_mode(&StackItem::DTemp(dtemp)),
            None => c_err!("Given dtemp, D{}, was not in dreg or on stack", dtemp),
        }
    }
    fn atemp_to_addr_mode(&self, atemp: ATemp) -> Result<AddrMode, CompilerErr> {
        match self.atemp_map.get(&atemp) {
            Some(Some(areg)) => Ok((*areg).into()),
            Some(None) => self.stack_item_to_addr_mode(&StackItem::ATemp(atemp)),
            None => c_err!("Given atemp, A{}, was not in areg or on stack", atemp),
        }
    }
    /// If the given AddrMode is DReg, returns the DReg. Otherwise, moves the value into
    /// the given Proxy and returns that Proxy.
    pub fn addr_mode_to_dreg(
        &self,
        addr_mode: AddrMode,
        size: DataSize,
        instrs: &mut Vec<ValidInstruction>,
        n: Proxy
    ) -> Result<Proxied, CompilerErr> {
        if let AddrMode::D(dreg) = addr_mode {
            Ok((dreg, true))
        } else {
            let proxy = n.into();
            let to = AddrMode::D(proxy);
            let instr = Instruction::Move(size, addr_mode, to).validate()?;
            instrs.push(instr);
            Ok((proxy, true))
        }
    }
    pub fn atemp_as_areg(
        &self,
        atemp: ATemp,
        instrs: &mut Vec<ValidInstruction>,
        n: Proxy,
    ) -> Result<(AReg, bool), CompilerErr> {
        match self.atemp_map.get(&atemp).unwrap() {
            Some(areg) => Ok((*areg, true)),
            None => {
                let proxy = n.into();
                let from = self.stack_item_to_addr_mode(&StackItem::ATemp(atemp))?;
                let to = AddrMode::A(proxy);
                let instr = Instruction::Move(DataSize::LWord, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
        }
    }
    pub fn atemp_is_areg(
        &self,
        atemp: ATemp,
    ) -> bool {
        self.atemp_map.get(&atemp).unwrap().is_some()
    }
    pub fn dtemp_as_dreg(
        &self,
        dtemp: DTemp,
        instrs: &mut Vec<ValidInstruction>,
        n: Proxy,
    ) -> Result<Proxied, CompilerErr> {
        match self.dtemp_map.get(&dtemp).unwrap() {
            Some(dreg) => Ok((*dreg, true)),
            None => {
                let proxy = n.into();
                let from = self.stack_item_to_addr_mode(&StackItem::DTemp(dtemp))?;
                let to = AddrMode::D(proxy);
                // Data temps are all assumed to be 4 bytes here
                let instr = Instruction::Move(DataSize::LWord, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
        }
    }
    pub fn dtemp_is_dreg(
        &self,
        dtemp: DTemp,
    ) -> bool {
        self.dtemp_map.get(&dtemp).unwrap().is_some()
    }
    pub fn get_new_label(&mut self) -> usize {
        self.label_gen.next().unwrap()
    }
    pub fn opt_dtemp_as_opt_dreg(
        &self,
        dtemp: Option<DTemp>,
        instrs: &mut Vec<ValidInstruction>,
        n: Proxy,
    ) -> Result<Option<Proxied>, CompilerErr> {
        dtemp
                .map(|dtemp| self.dtemp_as_dreg(dtemp, instrs, n))
                .transpose()
    }

    pub fn print_stack_frame<'a>(&'a self) -> Result<(), CompilerErr> {
        /// Represents a horizontal bar in the printed stack frame
        #[derive(Copy, Clone, Debug)]
        struct StackComponent<'b> {
            height: StackHeight,
            item: Option<&'b StackItem>,
            smarker: Option<usize>,
            reg_space: Option<(usize, ADBitField)>,
        }

        let mut components: HashMap<StackHeight, StackComponent<'a>> = HashMap::new();

        // Add stack items
        for (item, height) in &self.stack_frame.frame_map {
            components.insert(*height, StackComponent {
                height: *height,
                item: Some(item),
                smarker: None,
                reg_space: None,
            });
        }

        if components.is_empty() {
            debug!("Function {} had an empty stack frame", self.name.name);
            return Ok(());
        }

        let mut components: Vec<StackComponent<'_>> = components
            .into_values()
            .collect();
        // Sort descending
        components.sort_by(|comp1, comp2| comp2.height.cmp(&comp1.height));

        let mut prev_height = self.stack_frame.ceiling;

        let horiz = "--------------------------";
        let blank = " ".repeat(horiz.len());
        println!("+{}+ {}", horiz, self.stack_frame.ceiling);

        for comp in components {
            let diff = prev_height - comp.height;
            let string = format!("{} {} {}", match comp.item {
                Some(StackItem::ATemp(atemp)) => format!("A{}", atemp),
                Some(StackItem::DTemp(dtemp)) => format!("D{}", dtemp),
                Some(StackItem::Var(var)) => var.to_owned(),
                Some(StackItem::PC) => "PC".to_owned(),
                Some(StackItem::FP) => "FP".to_owned(),
                None => "".to_owned(),
            }, match comp.smarker {
                Some(smarker) => format!("Smarker{}", smarker),
                None => "".to_owned(),
            }, match comp.reg_space {
                Some((rs_lbl, adbf)) => format!("RegSpace{}: {}", rs_lbl, adbf),
                None => "".to_owned(),
            }).trim().to_owned();
            if diff >= 0 {
                let temp = (diff - 1) as usize;
                if temp > 10 {
                    for _ in 0..10 {
                        println!("|{}|", blank);
                    }
                } else {
                    for _ in 0..temp {
                        println!("|{}|", blank);
                    }
                }
                let bytes = format!("{} bytes", diff);
                let left = if bytes.len() >= horiz.len() { 0 } else { (horiz.len() - bytes.len()) / 2 };
                let right = if bytes.len() >= horiz.len() { 0 } else { horiz.len() - bytes.len() - left };
                println!("|{}{}{}|", " ".repeat(left), bytes, " ".repeat(right));
                
                let left = if string.len() >= horiz.len() { 0 } else { (horiz.len() - string.len()) / 2 };
                let right = if string.len() >= horiz.len() { 0 } else { horiz.len() - string.len() - left };
                println!("|{}{}{}|", " ".repeat(left), string, " ".repeat(right));
            }
            println!("+{}+ {} {}", horiz, comp.height, if comp.height == 0 { "<--- start (esh == 0)" } else { "" });
            prev_height = comp.height;
        }

        Ok(())
    }
}

#[derive(Copy, Clone)]
pub enum Proxy {
    Proxy1,
    Proxy2,
}
impl Proxy {
    pub const DATA_PROXY1: DReg = DReg::D6;
    pub const DATA_PROXY2: DReg = DReg::D7;
    pub const ADDR_PROXY1: AReg = AReg::A4;
    pub const ADDR_PROXY2: AReg = AReg::A5;
}
impl From<Proxy> for DReg {
    fn from(val: Proxy) -> Self {
        match val {
            Proxy1 => Proxy::DATA_PROXY1,
            Proxy2 => Proxy::DATA_PROXY2,
        }
    }
}
impl From<Proxy> for AReg {
    fn from(val: Proxy) -> Self {
        match val {
            Proxy1 => Proxy::ADDR_PROXY1,
            Proxy2 => Proxy::ADDR_PROXY2,
        }
    }
}