use std::ops::RangeFrom;

use crate::m68k::*;

use log::*;

use super::place::*;


pub struct LoopEnvironment {
    break_label: usize,
    continue_label: usize,
}

pub struct FunctionInterEnvironment {
    pub sig: Signature,
    /// If this function returns an array or struct (even if its size < 4 bytes),
    /// then an extra variable, `"[retval]"`, will be passed in
    pub vars: Vec<Field>,
    // (label number, string)
    pub lit_strings: Vec<(usize, String)>,
    pub cleanup_label: Option<usize>,
    pub cleanup_expr_created: bool,
    loop_stack: Vec<LoopEnvironment>,
    pub label_gen: RangeFrom<usize>,
    // dtemps and atemps can only hold variables up to 4 bytes
    // Variables greater than 4 bytes must be stored in vars.
    // They cannot be returned from expressions (until that is 
    // implemented) except to be pushed onto the stack. But
    // Move operations between reference Places move to or 
    // from the thing pointed to and can move large values (>4 bytes). 
    pub dtemps: Vec<bool>,
    pub atemps: Vec<bool>,
}
impl FunctionInterEnvironment {
    pub fn new(sig: Signature) -> FunctionInterEnvironment {
        FunctionInterEnvironment {
            vars: sig.args.clone(),
            sig,
            lit_strings: Vec::new(),
            cleanup_label: None,
            cleanup_expr_created: false,
            loop_stack: Vec::new(),
            label_gen: 0..,
            dtemps: Vec::new(),
            atemps: Vec::new(),
        }
    }
    pub fn get_name(&self) -> String {
        self.sig.name.name.clone()
    }
    pub fn return_type(&self) -> TypeType {
        self.sig.name.tt.clone()
    }
    /// Data temps are stored in dtemps.
    /// Data temps compile to data registers, so this function fails if the given type is a large type (size > 4 bytes).
    /// Arrays and structs cannot be stored in data registers, either, so this function will fail if the given type array or struct.
    /// However, it is not the case that only magic types can be stored in data temps, since pointers are not magic.
    pub fn get_data_temp(&mut self, tt: TypeType) -> Result<usize, String> {
        if tt.is_array() || tt.is_struct() {
            return Err(format!("Cannot store arrays or structs in data temps. TypeType {} given", tt));
        }
        // If a data temp is available, give it out
        for (temp, used) in self.dtemps.iter_mut().enumerate() {
            if !*used {
                *used = true;
                return Ok(temp);
            }
        }
        let place = self.dtemps.len();
        self.dtemps.push(true);
        Ok(place)
    }
    // Addr temps can point to anything
    pub fn get_addr_temp(&mut self) -> Result<usize, String> {
        // If a addr temp is available, give it out
        for (temp, used) in self.atemps.iter_mut().enumerate() {
            if !*used {
                *used = true;
                return Ok(temp);
            }
        }
        let place = self.atemps.len();
        self.atemps.push(true);
        Ok(place)
    }
    fn _free_temp(temps: &mut Vec<bool>, temp: usize, label: &str) {
        if temp < temps.len() {
            if temps[temp] {
                warn!("Trying to free {}{}, which is already freed", label, temp);
            }
            temps[temp] = false;
        }
    }
    pub fn free_data_temp(&mut self, dtemp: DTemp) {
        Self::_free_temp(&mut self.dtemps, dtemp, "D")
    }
    pub fn free_addr_temp(&mut self, atemp: ATemp) {
        Self::_free_temp(&mut self.atemps, atemp, "A")
    }
    pub fn push_loop_stack(&mut self, break_label: usize, continue_label: usize) {
        self.loop_stack.push(LoopEnvironment { break_label, continue_label })
    }
    pub fn get_break_label(&self) -> Option<usize> {
        // Maybe someday, you could break 2 layers out using `break 2;`
        self.loop_stack.last().map(|loop_environment| loop_environment.break_label)
    }
    pub fn get_continue_label(&self) -> Option<usize> {
        self.loop_stack.last().map(|loop_environment| loop_environment.continue_label)
    }
    pub fn pop_loop_stack(&mut self) -> Option<LoopEnvironment>{
        self.loop_stack.pop()
    }
    pub fn add_lit_string(&mut self, string: String) -> usize {
        let ind = self.get_new_label();
        self.lit_strings.push((ind, string));
        ind
    }
    pub fn get_var(&self, name: &String) -> Option<Field> {
        for field in &self.vars {
            if field.name == *name {
                return Some(field.clone());
            }
        }
        None
    }
    pub fn has_var(&self, name: &String) -> bool {
        matches!(self.get_var(name), Some(_))
    }
    pub fn add_var(&mut self, field: &Field) -> Result<(), String> {
        if let Some(existing_field) = self.get_var(&field.name) {
            if existing_field.tt == field.tt {
                // Var already exists, so do nothing
                Ok(())
            } else {
                // Var exists with a different type
                Err(format!("Cannot create var {}, because var {} already exists", field, existing_field))
            }
        } else {
            self.vars.push(field.clone());
            Ok(())
        }
    }
    pub fn get_new_label(&mut self) -> usize {
        self.label_gen.next().unwrap()
    }
    pub fn get_cleanup_label(&mut self) -> usize {
        if let Some(label) = self.cleanup_label {
            label
        } else {
            let label = self.get_new_label();
            self.cleanup_label = Some(label);
            label
        }
    }
    pub fn ret(&mut self, place: Place, instrs: &mut Vec<InterInstr>, env: &Environment) -> Result<(), String> {
        let tt = place.get_type();
        if tt != self.return_type() {
            return Err(format!("Function has return type {} but tried to return object of type {}", self.return_type(), tt));
        }
        let to = env.ret_place(self.get_name())?;
        let instr = InterInstr::Move(place, to);
        instrs.push(instr);
        let instr = InterInstr::Rts;
        instrs.push(instr);
        Ok(())
    }
    pub fn reti(&mut self, imm: Imm, instrs: &mut Vec<InterInstr>, env: &Environment) -> Result<(), String> {
        let imm = imm.as_type(self.return_type(), env)?;
        let to = env.ret_place(self.get_name())?;
        let instr = InterInstr::Movi(imm, to);
        instrs.push(instr);
        let instr = InterInstr::Rts;
        instrs.push(instr);
        Ok(())
    }
    pub fn rets(&mut self, string: usize, instrs: &mut Vec<InterInstr>, env: &Environment) -> Result<(), String> {
        let tt = Environment::get_str_tt();
        if tt != self.return_type() {
            return Err(format!("Function has return type {} but tried to return literal string of type {}", self.return_type(), tt));
        }
        let to = env.ret_place(self.get_name())?;
        let instr = InterInstr::Movs(string, to);
        instrs.push(instr);
        let instr = InterInstr::Rts;
        instrs.push(instr);
        Ok(())
    }
    pub fn retva(&mut self, name: &String, instrs: &mut Vec<InterInstr>, env: &Environment) -> Result<(), String> {
        let var = match self.get_var(name) {
            Some(name) => name,
            None => {
                return Err(format!("Trying to return pointer to local variable {} which does not exist", name));
            }
        };
        let tt = TypeType::Pointer(Box::new(var.tt.clone()));
        if tt != self.return_type() {
            return Err(format!("Function has return type {} but tried to return pointer to local variable {} of type {}", self.return_type(), var.name, var.tt));
        }
        let to = env.ret_place(self.get_name())?;
        let instr = InterInstr::MoVA(var.name, to);
        instrs.push(instr);
        let instr = InterInstr::Rts;
        instrs.push(instr);
        Ok(())
    }
}