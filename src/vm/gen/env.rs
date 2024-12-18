use std::{
    collections::{hash_map::Entry, HashMap},
    mem,
};

use crate::vm::Word;

#[derive(Debug)]
pub enum Symbol {
    Variable(u8),
    Function(Word),
}

#[derive(Debug)]
pub struct EnvManager<'syn> {
    envs: Vec<Env<'syn>>,
}

impl<'syn> EnvManager<'syn> {
    pub fn with_global_env() -> Self {
        Self {
            envs: vec![Env::new()],
        }
    }

    #[inline(always)]
    pub fn push(&mut self) {
        self.envs.push(Env::new());
    }

    #[inline(always)]
    pub fn pop(&mut self) {
        self.envs
            .pop()
            .expect("There should aways be at least one env to pop (the global one).");
    }

    pub fn active_function_frame(&mut self) -> &mut FunctionFrame {
        self.try_active_function_frame().unwrap()
    }

    fn try_active_function_frame(&mut self) -> Option<&mut FunctionFrame> {
        for env in self.envs.iter_mut().rev() {
            if let Some(value) = env.function_frames.last_mut() {
                // NOTE: This silly transmute can be removed once the borrow checker sees that the
                // lifetimes are fine.
                return Some(unsafe {
                    mem::transmute::<&mut FunctionFrame, &mut FunctionFrame>(value)
                });
            }
        }

        None
    }

    pub fn find_symbol(&mut self, identifier: &'syn str) -> Option<&mut Symbol> {
        for env in self.envs.iter_mut().rev() {
            if let Some(value) = env.symbols.get_mut(identifier) {
                // NOTE: This silly transmute can be removed once the borrow checker sees that the
                // lifetimes are fine.
                return Some(unsafe { mem::transmute::<&mut Symbol, &mut Symbol>(value) });
            }
        }

        None
    }

    pub fn declare_symbol(&mut self, identifier: &'syn str, symbol: Symbol) -> Option<()> {
        // Disallow declaring symbols with the same name as the current function, if we are inside
        // one.
        if let Some(frame) = self.try_active_function_frame() {
            if identifier == frame.function_name {
                return None;
            }
        }

        match self.active().symbols.entry(identifier) {
            Entry::Occupied(mut entry) => match (entry.get_mut(), symbol) {
                // Allow variables to shadow other variables.
                (Symbol::Variable(reg), Symbol::Variable(new_reg)) => *reg = new_reg,
                // Disallow any other type of shadowing.
                _ => return None,
            },
            Entry::Vacant(entry) => {
                entry.insert(symbol);
            }
        }

        Some(())
    }

    #[inline(always)]
    pub fn push_function_frame(&mut self, function_name: &'syn str) {
        self.active()
            .function_frames
            .push(FunctionFrame::new(function_name))
    }

    #[inline(always)]
    pub fn pop_function_frame(&mut self) {
        self.active()
            .function_frames
            .pop()
            .expect("A function frame should've just been pushed.");
    }

    #[inline(always)]
    fn active(&mut self) -> &mut Env<'syn> {
        self.envs
            .last_mut()
            .expect("There should always be at least one env.")
    }
}

#[derive(Debug)]
pub struct Env<'syn> {
    function_frames: Vec<FunctionFrame<'syn>>,
    symbols: HashMap<&'syn str, Symbol>,
}

impl<'syn> Env<'syn> {
    fn new() -> Self {
        Self {
            function_frames: Vec::new(),
            symbols: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionFrame<'syn> {
    pub num_registers: u8,
    pub function_name: &'syn str,
}

impl<'syn> FunctionFrame<'syn> {
    fn new(function_name: &'syn str) -> Self {
        Self {
            num_registers: 0,
            function_name,
        }
    }
}
