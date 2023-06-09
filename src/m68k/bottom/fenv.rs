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

use std::{collections::{HashMap, HashSet}, ops::RangeFrom};

use Proxy::*;

use crate::m68k::*;

use super::instruction::*;

use ADReg::*;
use AReg::SP;

#[derive(Hash, Clone, Eq, PartialEq)]
pub enum StackItem {
    Var(String),
    DTemp(DTemp),
    ATemp(ATemp),
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

/// The bool represents if the DReg is live or not. If not,
/// changes to the DReg do not affect the original Place
type Proxied = (DReg, bool);
/// The bool represents if the AReg is live or not. If not,
/// changes to the AReg do not affect the original Place
type ProxiedAddr = (AReg, bool);

pub struct FunctionEnvironment {
    pub lit_strings: Vec<(usize, String)>,
    pub vars: Vec<Field>,
    dtemp_map: HashMap<DTemp, Option<DReg>>,
    atemp_map: HashMap<ATemp, Option<AReg>>,
    extra_stack_height: usize,
    stack_frame: HashMap<StackItem, i32>,
    label_gen: RangeFrom<usize>,
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
    ) -> FunctionEnvironment {
        let stack_frame =
            Self::generate_stack_frame(&signature, &vars, &dtemp_map, &atemp_map, env);
        FunctionEnvironment {
            lit_strings,
            vars,
            dtemp_map,
            atemp_map,
            label_gen,
            extra_stack_height: 0,
            stack_frame,
        }
    }
    /// Generates the stack frame. The stack frame is organized like this:
    /// ```txt
    /// +-----------------------------------+   higher addresses
    /// |               params              |
    /// +-----------------------------------+
    /// |            spilled regs           |
    /// +-----------------------------------+
    /// |             local vars            |
    /// +-----------------------------------+  <--- extra_stack_height = 0
    /// |    saved regs for function call   |
    /// +-----------------------------------+   lower addresses
    /// ```
    fn generate_stack_frame(
        signature: &Signature,
        vars: &[Field],
        dtemp_map: &HashMap<DTemp, Option<DReg>>,
        atemp_map: &HashMap<ATemp, Option<AReg>>,
        env: &Environment,
    ) -> HashMap<StackItem, i32> {
        // Build stack frame from the bottom up
        let mut stack_height: i32 = 0;
        let params = signature.args.iter().cloned().collect::<HashSet<Field>>();
        let mut stack_frame = HashMap::new();

        // Add local vars to stack frame
        for var in vars.iter().rev() {
            if params.contains(var) {
                continue;
            }
            let size = var.tt.get_size(env);
            let item = StackItem::Var(var.name.clone());
            stack_frame.insert(item, stack_height);
            stack_height += size as i32;
        }

        // Add spilled regs to stack frame
        for (dtemp, dreg) in dtemp_map {
            if dreg.is_some() {
                // Only add spilled regs to the stack frame
                continue;
            }
            // We are just going to assume all dregs are 4 bytes, even though they could be 2 or 1
            let size = 4;
            let item = StackItem::DTemp(*dtemp);
            stack_frame.insert(item, stack_height);
            stack_height += size;
        }
        for (atemp, areg) in atemp_map {
            if areg.is_some() {
                continue;
            }
            let size = 4;
            let item = StackItem::ATemp(*atemp);
            stack_frame.insert(item, stack_height);
            stack_height += size;
        }

        // Add params to stack frame
        for param in &signature.args {
            let size = param.tt.get_size(env);
            let item = StackItem::Var(param.name.clone());
            stack_frame.insert(item, stack_height);
            stack_height += size as i32;
        }

        stack_frame
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
    ) -> Result<AddrMode, String> {
        self.stack_item_to_addr_mode(StackItem::Var(name))
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
        instrs: &mut Vec<ValidInstruction>,
        n: Proxy,
    ) -> Result<AddrMode, String> {
        match place {
            Place::Var(name) => self.var_as_addr_mode(name.name),
            Place::ATemp(atemp) => match self.atemp_map.get(&atemp).unwrap() {
                Some(areg) => Ok(AddrMode::A(*areg)),
                None => Ok(self.stack_item_to_addr_mode(StackItem::ATemp(atemp))?),
            },
            Place::DTemp(dtemp, tt) => {
                if tt.is_array() || tt.is_struct() {
                    return Err(format!(
                        "Array or struct {} being stored in dtemp D{}",
                        tt, dtemp
                    ));
                }
                match self.dtemp_map.get(&dtemp).unwrap() {
                    Some(dreg) => Ok(AddrMode::D(*dreg)),
                    None => self.stack_item_to_addr_mode(StackItem::DTemp(dtemp)),
                }
            }
            Place::Ref(atemp, dtemp, off, tt) => {
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
    /// The returned AddrMode is always live. It will alwyas be AIndDisp
    pub fn stack_item_to_addr_mode(&self, stack_item: StackItem) -> Result<AddrMode, String> {
        match stack_item.clone() {
            StackItem::Var(name) => match self.stack_frame.get(&stack_item) {
                Some(stack_diff) => Ok(AddrMode::AIndDisp(NumOrLbl::Num(*stack_diff), SP)),
                None => Err(format!("Unrecognized variable {}", name)),
            },
            StackItem::DTemp(dtemp) => match self.dtemp_map.get(&dtemp) {
                Some(Some(dreg)) => Ok((*dreg).into()),
                _ => match self.stack_frame.get(&stack_item) {
                    Some(stack_diff) => Ok(AddrMode::AIndDisp(NumOrLbl::Num(*stack_diff), SP)),
                    None => Err(format!("DTemp {} not in dtemp_map or stack_frame", dtemp)),
                },
            },
            StackItem::ATemp(atemp) => match self.atemp_map.get(&atemp) {
                Some(Some(areg)) => Ok((*areg).into()),
                _ => match self.stack_frame.get(&stack_item) {
                    Some(stack_diff) => Ok(AddrMode::AIndDisp(NumOrLbl::Num(*stack_diff), SP)),
                    None => Err(format!("ATemp {} not in atemp_map or stack_frame", atemp)),
                },
            },
        }
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
        instrs: &mut Vec<ValidInstruction>,
        env: &Environment,
        n: Proxy,
    ) -> Result<Proxied, String> {
        match place {
            Place::Var(name) => {
                if name.tt.is_array() || name.tt.is_struct() {
                    return Err(format!(
                        "Cannot move array or struct {} into data reg",
                        name
                    ));
                }
                let proxy: DReg = n.into();
                let size = name.tt.get_data_size(env).unwrap();
                let from = self.stack_item_to_addr_mode(StackItem::Var(name.name))?;
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
            Place::DTemp(dtemp, tt) => {
                // What do we do with tt?
                self.dtemp_as_dreg(dtemp, instrs, n)
            }
            Place::Ref(atemp, dtemp, off, tt) => {
                if tt.is_array() || tt.is_struct() {
                    return Err(format!(
                        "Cannot move value of type {} into data reg",
                        tt
                    ));
                }
                let areg = self.atemp_as_areg(atemp, instrs, n)?.0;
                let dreg = self.opt_dtemp_as_opt_dreg(dtemp, instrs, n)?
                        .map(|dreg| dreg.0);
                let size = tt.get_data_size(env).unwrap();
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
        instrs: &mut Vec<ValidInstruction>,
        env: &Environment,
        n: Proxy
    ) -> Result<ProxiedAddr, String> {
        match place {
            Place::Var(name) => {
                if name.tt.is_array() || name.tt.is_struct() {
                    return Err(format!(
                        "Cannot move array or struct {} into addr reg",
                        name
                    ));
                }
                let proxy: AReg = n.into();
                let size = name.tt.get_data_size(env).unwrap();
                let from = self.stack_item_to_addr_mode(StackItem::Var(name.name))?;
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
                let size = tt.get_data_size(env).unwrap();
                let from = self.dtemp_to_addr_mode(dtemp)?;
                let to = proxy.into();
                let instr = Instruction::Move(size, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
            Place::Ref(atemp, dtemp, off, tt) => {
                if tt.is_array() || tt.is_struct() {
                    return Err(format!(
                        "Cannot move value of type {} into data reg",
                        tt
                    ));
                }
                let areg = self.atemp_as_areg(atemp, instrs, n)?.0;
                let dreg = self.opt_dtemp_as_opt_dreg(dtemp, instrs, n)?
                        .map(|dreg| dreg.0);
                let size = tt.get_data_size(env).unwrap();
                let from = (off, areg, dreg).into();
                let proxy: AReg = n.into();
                let to = proxy.into();
                let instr = Instruction::Move(size, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            },

        }
    }
    fn dtemp_to_addr_mode(&self, dtemp: DTemp) -> Result<AddrMode, String> {
        match self.dtemp_map.get(&dtemp) {
            Some(Some(dreg)) => Ok((*dreg).into()),
            Some(None) => self.stack_item_to_addr_mode(StackItem::DTemp(dtemp)),
            None => Err(format!("Given dtemp, D{}, was not in dreg or on stack", dtemp)),
        }
    }
    fn atemp_to_addr_mode(&self, atemp: ATemp) -> Result<AddrMode, String> {
        match self.atemp_map.get(&atemp) {
            Some(Some(areg)) => Ok((*areg).into()),
            Some(None) => self.stack_item_to_addr_mode(StackItem::ATemp(atemp)),
            None => Err(format!("Given atemp, A{}, was not in areg or on stack", atemp)),
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
    ) -> Result<Proxied, String> {
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
    ) -> Result<(AReg, bool), String> {
        match self.atemp_map.get(&atemp).unwrap() {
            Some(areg) => Ok((*areg, true)),
            None => {
                let proxy = n.into();
                let from = self.stack_item_to_addr_mode(StackItem::ATemp(atemp))?;
                let to = AddrMode::A(proxy);
                let instr = Instruction::Move(DataSize::LWord, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
        }
    }
    pub fn dtemp_as_dreg(
        &self,
        dtemp: DTemp,
        instrs: &mut Vec<ValidInstruction>,
        n: Proxy,
    ) -> Result<Proxied, String> {
        match self.dtemp_map.get(&dtemp).unwrap() {
            Some(dreg) => Ok((*dreg, true)),
            None => {
                let proxy = n.into();
                let from = self.stack_item_to_addr_mode(StackItem::DTemp(dtemp))?;
                let to = AddrMode::D(proxy);
                // Data temps are all assumed to be 4 bytes here
                let instr = Instruction::Move(DataSize::LWord, from, to).validate()?;
                instrs.push(instr);
                Ok((proxy, false))
            }
        }
    }
    pub fn get_new_label(&mut self) -> usize {
        self.label_gen.next().unwrap()
    }
    pub fn opt_dtemp_as_opt_dreg(
        &self,
        dtemp: Option<DTemp>,
        instrs: &mut Vec<ValidInstruction>,
        n: Proxy,
    ) -> Result<Option<Proxied>, String> {
        dtemp
                .map(|dtemp| self.dtemp_as_dreg(dtemp, instrs, n))
                .transpose()
    }
}

pub type StackMarker = usize;

#[derive(Copy, Clone)]
pub enum Proxy {
    Proxy1,
    Proxy2,
}
impl Proxy {
    pub const DATA_PROXY1: DReg = DReg::D6;
    pub const DATA_PROXY2: DReg = DReg::D7;
    pub const ADDR_PROXY1: AReg = AReg::A5;
    pub const ADDR_PROXY2: AReg = AReg::A6;
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