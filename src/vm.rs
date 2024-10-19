use std::{
    collections::{hash_map::Entry, HashMap},
    mem,
};

use crate::{
    lex::Spanned,
    pool::{Handle, Pool},
    syn::{BinaryOp, Expr, Literal, Stmt, SymbolDecl, SymbolInfo, VarInfo},
};

pub struct Processor<'gen> {
    program: &'gen [Op],
    pc: Word,

    regs: Vec<Reg>,
    bp: Word,
}

impl<'gen> Processor<'gen> {
    pub fn new(program: &'gen [Op]) -> Self {
        Self {
            program,
            pc: 0,

            regs: Vec::new(),
            bp: 0,
        }
    }

    pub fn run(mut self) {
        loop {
            let op = *unsafe { self.program.get_unchecked(self.pc as usize) };
            self.pc += 1;
            match op {
                Op::Call { addr } => {
                    let ret_addr = mem::replace(&mut self.pc, addr);
                    self.regs.push(Reg(ret_addr));
                }
                Op::Reserve { num_regs } => {
                    self.regs.push(Reg(self.bp));
                    self.bp = self.regs.len() as Word;

                    self.regs
                        .resize_with(self.regs.len() + num_regs as usize, Default::default);
                }
                Op::Ret => {
                    self.regs.truncate(self.bp as usize);

                    self.bp = unsafe { self.regs.pop().unwrap_unchecked().0 };
                    self.pc = unsafe { self.regs.pop().unwrap_unchecked().0 };
                }

                Op::Load { dst, src } => *self.reg(dst) = *self.reg(src),
                Op::LoadImm { dst, imm } => *self.reg(dst) = Reg(imm),

                Op::Add { dst, left, right } => *self.reg(dst) = *self.reg(left) + *self.reg(right),
                Op::Sub { dst, left, right } => *self.reg(dst) = *self.reg(left) - *self.reg(right),
                Op::Div { dst, left, right } => *self.reg(dst) = *self.reg(left) / *self.reg(right),
                Op::Mul { dst, left, right } => *self.reg(dst) = *self.reg(left) * *self.reg(right),

                Op::Print { reg } => {
                    println!("(vm): {}", self.reg(reg).0);
                }
                Op::Halt => {
                    break;
                }
            }
            #[cfg(debug_assertions)]
            {
                dbg!(op);
                dbg!(&self.regs);
            }
        }
    }

    #[inline(always)]
    fn reg(&mut self, idx: u8) -> &mut Reg {
        unsafe { self.regs.get_unchecked_mut(self.bp as usize + idx as usize) }
    }
}

type Word = u32;

#[derive(Debug, Copy, Clone, Default)]
#[repr(transparent)]
pub struct Reg(Word);

impl std::ops::Add for Reg {
    type Output = Reg;

    #[inline(always)]
    fn add(self, rhs: Self) -> Self::Output {
        Reg(self.0 + rhs.0)
    }
}

impl std::ops::Add<Word> for Reg {
    type Output = Reg;

    #[inline(always)]
    fn add(self, rhs: Word) -> Self::Output {
        Reg(self.0 + rhs)
    }
}

impl std::ops::Add<Reg> for Word {
    type Output = Reg;

    #[inline(always)]
    fn add(self, rhs: Reg) -> Self::Output {
        Reg(self + rhs.0)
    }
}

impl std::ops::Sub for Reg {
    type Output = Reg;

    #[inline(always)]
    fn sub(self, rhs: Self) -> Self::Output {
        Reg(self.0 - rhs.0)
    }
}

impl std::ops::Sub<Word> for Reg {
    type Output = Reg;

    #[inline(always)]
    fn sub(self, rhs: Word) -> Self::Output {
        Reg(self.0 - rhs)
    }
}

impl std::ops::Sub<Reg> for Word {
    type Output = Reg;

    #[inline(always)]
    fn sub(self, rhs: Reg) -> Self::Output {
        Reg(self - rhs.0)
    }
}

impl std::ops::Div for Reg {
    type Output = Reg;

    #[inline(always)]
    fn div(self, rhs: Self) -> Self::Output {
        Reg(self.0 / rhs.0)
    }
}

impl std::ops::Div<Word> for Reg {
    type Output = Reg;

    #[inline(always)]
    fn div(self, rhs: Word) -> Self::Output {
        Reg(self.0 / rhs)
    }
}

impl std::ops::Div<Reg> for Word {
    type Output = Reg;

    #[inline(always)]
    fn div(self, rhs: Reg) -> Self::Output {
        Reg(self / rhs.0)
    }
}

impl std::ops::Mul for Reg {
    type Output = Reg;

    #[inline(always)]
    fn mul(self, rhs: Self) -> Self::Output {
        Reg(self.0 * rhs.0)
    }
}

impl std::ops::Mul<Word> for Reg {
    type Output = Reg;

    #[inline(always)]
    fn mul(self, rhs: Word) -> Self::Output {
        Reg(self.0 * rhs)
    }
}

impl std::ops::Mul<Reg> for Word {
    type Output = Reg;

    #[inline(always)]
    fn mul(self, rhs: Reg) -> Self::Output {
        Reg(self * rhs.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Op {
    Call { addr: Word },
    Reserve { num_regs: u8 },
    Ret,

    Load { dst: u8, src: u8 },
    LoadImm { dst: u8, imm: Word },

    Add { dst: u8, left: u8, right: u8 },
    Sub { dst: u8, left: u8, right: u8 },
    Div { dst: u8, left: u8, right: u8 },
    Mul { dst: u8, left: u8, right: u8 },

    Print { reg: u8 },

    Halt,
}

use Op::*;

pub struct Generator<'syn> {
    program: Vec<Op>,
    envs: EnvManager<'syn>,
    override_next_allocated_register: Option<u8>,
    function_generation_queue: Vec<SymbolDecl<'syn>>,
}

impl<'syn> Generator<'syn> {
    pub fn new() -> Self {
        Self {
            program: Vec::new(),
            envs: EnvManager::with_global_env(),
            override_next_allocated_register: None,
            function_generation_queue: Vec::new(),
        }
    }

    pub fn generate(
        mut self,
        outer_stmts: Pool<Spanned<Stmt<'syn>>>,
        stmts: Pool<Spanned<Stmt<'syn>>>,
        exprs: Pool<Spanned<Expr<'syn>>>,
    ) -> Result<Vec<Op>, Error<'syn>> {
        // The address here is the entry point, which will be patched up later when we find the
        // `main` symbol.
        self.program = vec![Call { addr: 0 }, Halt];

        for stmt in outer_stmts.handles() {
            let Spanned(stmt_data, span) = outer_stmts.get(stmt);
            match stmt_data {
                Stmt::SymbolDecl(SymbolDecl {
                    identifier,
                    info: SymbolInfo::Fn(expr),
                }) => {
                    self.queue_function_generation(identifier, &stmts, stmt, *expr)?;
                }
                Stmt::SymbolDecl(SymbolDecl {
                    // 'main' symbol in the outer scope that isn't a function.
                    identifier: "main",
                    info: _,
                }) => return Err(Error(Spanned(ErrorKind::InvalidMainDeclaration, *span))),

                _ => return Err(Error(Spanned(ErrorKind::NotInsideFunction, *span))),
            }

            while let Some(SymbolDecl {
                identifier,
                info: SymbolInfo::Fn(expr),
            }) = self.function_generation_queue.pop()
            {
                let addr = self.generate_function(&stmts, &exprs, identifier, expr)?;
                if identifier == "main" {
                    self.program[0] = Call { addr };
                }
            }
        }

        Ok(self.program)
    }

    fn generate_statement(
        &mut self,
        stmts: &Pool<Spanned<Stmt<'syn>>>,
        exprs: &Pool<Spanned<Expr<'syn>>>,
        stmt: Handle<Spanned<Stmt<'syn>>>,
    ) -> Result<(), Error<'syn>> {
        let Spanned(stmt_data, span) = stmts.get(stmt);
        match stmt_data {
            Stmt::Expr(expr) => {
                self.generate_expression(stmts, exprs, *expr)?;
            }
            Stmt::SymbolDecl(SymbolDecl { identifier, info }) => match info {
                SymbolInfo::Fn(expr) => {
                    self.queue_function_generation(identifier, stmts, stmt, *expr)?;
                }
                SymbolInfo::Var(VarInfo::Value(expr)) => {
                    let reg = self.allocate_reg();

                    self.override_next_allocated_register = Some(reg);
                    self.generate_expression(stmts, exprs, *expr)?;

                    self.envs
                        .declare_symbol(identifier, Symbol::Variable(reg))
                        .map_err(|err| Error(Spanned(err, *span)))?;
                }
                _ => unimplemented!(),
            },
            Stmt::Print(expr) => {
                let reg = self.generate_expression(stmts, exprs, *expr)?;
                self.program.push(Print { reg });
            }
        }
        Ok(())
    }

    fn generate_expression(
        &mut self,
        stmts: &Pool<Spanned<Stmt<'syn>>>,
        exprs: &Pool<Spanned<Expr<'syn>>>,
        expr: Handle<Spanned<Expr<'syn>>>,
    ) -> Result<u8, Error<'syn>> {
        let Spanned(expr_data, span) = exprs.get(expr);
        match expr_data {
            Expr::Binary { left, op, right } => {
                let dst = self.allocate_reg();
                let left = self.generate_expression(stmts, exprs, *left)?;
                let right = self.generate_expression(stmts, exprs, *right)?;
                let op = match op {
                    BinaryOp::Add => Add { dst, left, right },
                    BinaryOp::Sub => Sub { dst, left, right },
                    BinaryOp::Div => Div { dst, left, right },
                    BinaryOp::Mul => Mul { dst, left, right },
                };
                self.program.push(op);
                Ok(dst)
            }
            Expr::Literal(literal) => match literal {
                Literal::Number(number) => {
                    let dst = self.allocate_reg();
                    self.program.push(LoadImm { dst, imm: *number });
                    Ok(dst)
                }
            },
            Expr::Symbol(identifier) => match self
                .envs
                .find_symbol(identifier)
                .map_err(|err| Error(Spanned(err, *span)))?
            {
                Symbol::Variable(reg) => Ok(*reg),
                _ => Err(Error(Spanned(ErrorKind::SymbolIsNotVariable, *span))),
            },
            Expr::Block(block_stmts) => {
                self.envs.push();

                // TODO: Populate this register.
                let dst = self.allocate_reg();
                for stmt in block_stmts {
                    self.generate_statement(stmts, exprs, *stmt)?;
                }

                self.envs.pop();

                Ok(dst)
            }
        }
    }

    fn generate_function(
        &mut self,
        stmts: &Pool<Spanned<Stmt<'syn>>>,
        exprs: &Pool<Spanned<Expr<'syn>>>,
        identifier: &'syn str,
        expr: Handle<Spanned<Expr<'syn>>>,
    ) -> Result<Word, Error<'syn>> {
        let prologue_addr = self.generate_prologue();

        self.envs.push_function_frame(identifier);

        self.generate_expression(stmts, exprs, expr)?;

        self.generate_epilogue();
        self.patch_prologue(prologue_addr);

        self.envs.pop_function_frame();

        Ok(prologue_addr as Word)
    }

    fn queue_function_generation(
        &mut self,
        identifier: &'syn str,
        stmts: &Pool<Spanned<Stmt<'syn>>>,
        stmt: Handle<Spanned<Stmt<'syn>>>,
        expr: Handle<Spanned<Expr<'syn>>>,
    ) -> Result<(), Error<'syn>> {
        // FIXME: Encode the fact that this function is not yet completed somehow,
        //        then generate `Call`s to it in a special way, maybe a `ToBePatchedCall` opcode or
        //        something, then iterate the active function frame when this function is
        //        eventually generated and patch those special opcodes with the correct address.
        self.envs
            .declare_symbol(identifier, Symbol::Function(0))
            .map_err(|err| Error(Spanned(err, stmts.get(stmt).span())))?;

        self.function_generation_queue.push(SymbolDecl {
            identifier,
            info: SymbolInfo::Fn(expr),
        });

        Ok(())
    }

    #[inline(always)]
    fn generate_prologue(&mut self) -> Word {
        // This dummy value of `0` will be patched up later.
        self.program.push(Reserve { num_regs: 0 });
        (self.program.len() - 1) as Word
    }

    #[inline(always)]
    fn generate_epilogue(&mut self) {
        self.program.push(Ret);
    }

    #[inline(always)]
    fn patch_prologue(&mut self, prologue_addr: Word) {
        #[cfg(debug_assertions)]
        {
            self.program[prologue_addr as usize] = Reserve {
                num_regs: self.envs.active_function_frame().num_registers,
            };
        }
        #[cfg(not(debug_assertions))]
        {
            // Update the previously added `Reserve` op with the number of allocated registers
            *unsafe { self.program.get_unchecked_mut(prologue_addr as usize) } = Reserve {
                num_regs: self.envs.active_function_frame().num_registers,
            };
        }
    }

    #[inline(always)]
    fn allocate_reg(&mut self) -> u8 {
        if let Some(reg) = self.override_next_allocated_register.take() {
            return reg;
        }

        let count = &mut self.envs.active_function_frame().num_registers;
        mem::replace(count, *count + 1)
    }
}

impl<'syn> Default for Generator<'syn> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum Symbol {
    Variable(u8),
    Function(Word),
}

#[derive(Debug)]
struct EnvManager<'syn> {
    envs: Vec<Env<'syn>>,
}

impl<'syn> EnvManager<'syn> {
    fn with_global_env() -> Self {
        Self {
            envs: vec![Env::new()],
        }
    }

    #[inline(always)]
    fn push(&mut self) {
        self.envs.push(Env::new());
    }

    #[inline(always)]
    fn pop(&mut self) {
        #[cfg(debug_assertions)]
        self.envs
            .pop()
            .expect("There should aways be at least one env to pop (the global one).");

        #[cfg(not(debug_assertions))]
        unsafe {
            self.envs.pop().unwrap_unchecked()
        };
    }

    fn active_function_frame(&mut self) -> &mut FunctionFrame {
        #[cfg(debug_assertions)]
        {
            self.try_active_function_frame().unwrap()
        }

        #[cfg(not(debug_assertions))]
        {
            unsafe { self.try_active_function_frame().unwrap_unchecked() }
        }
    }

    fn try_active_function_frame(&mut self) -> Result<&mut FunctionFrame, ErrorKind<'syn>> {
        for env in self.envs.iter_mut().rev() {
            if let Some(value) = env.function_frames.last_mut() {
                // NOTE: This silly transmute can be removed once the borrow checker sees that the
                // lifetimes are fine.
                return Ok(unsafe {
                    std::mem::transmute::<&mut FunctionFrame, &mut FunctionFrame>(value)
                });
            }
        }

        Err(ErrorKind::NotInsideFunction)
    }

    fn find_symbol(&mut self, identifier: &'syn str) -> Result<&mut Symbol, ErrorKind<'syn>> {
        for env in self.envs.iter_mut().rev() {
            if let Some(value) = env.symbols.get_mut(identifier) {
                // NOTE: This silly transmute can be removed once the borrow checker sees that the
                // lifetimes are fine.
                return Ok(unsafe { std::mem::transmute::<&mut Symbol, &mut Symbol>(value) });
            }
        }

        Err(ErrorKind::UndeclaredSymbol)
    }

    fn declare_symbol(
        &mut self,
        identifier: &'syn str,
        symbol: Symbol,
    ) -> Result<(), ErrorKind<'syn>> {
        // Disallow declaring symbols with the same name as the current function, if we are inside
        // one.
        if let Ok(frame) = self.try_active_function_frame() {
            if identifier == frame.function_name {
                // NOTE: This silly transmute can be removed once the borrow checker sees that the
                // lifetimes are fine.
                return Err(unsafe {
                    std::mem::transmute::<ErrorKind, ErrorKind>(ErrorKind::InvalidShadowing(
                        identifier,
                    ))
                });
            }
        }

        match self.active().symbols.entry(identifier) {
            Entry::Occupied(mut entry) => match (entry.get_mut(), symbol) {
                // Allow variables to shadow other variables.
                (Symbol::Variable(reg), Symbol::Variable(new_reg)) => *reg = new_reg,
                // Disallow any other type of shadowing.
                _ => return Err(ErrorKind::InvalidShadowing(identifier)),
            },
            Entry::Vacant(entry) => {
                entry.insert(symbol);
            }
        }

        Ok(())
    }

    #[inline(always)]
    fn push_function_frame(&mut self, function_name: &'syn str) {
        self.active()
            .function_frames
            .push(FunctionFrame::new(function_name))
    }

    #[inline(always)]
    fn pop_function_frame(&mut self) {
        #[cfg(debug_assertions)]
        self.active()
            .function_frames
            .pop()
            .expect("A function frame should've just been pushed.");

        #[cfg(not(debug_assertions))]
        unsafe {
            self.active().function_frames.pop().unwrap_unchecked()
        };
    }

    #[inline(always)]
    fn active(&mut self) -> &mut Env<'syn> {
        #[cfg(debug_assertions)]
        {
            self.envs
                .last_mut()
                .expect("There should always be at least one env.")
        }
        #[cfg(not(debug_assertions))]
        {
            unsafe { self.envs.last_mut().unwrap_unchecked() }
        }
    }
}

#[derive(Debug)]
struct Env<'syn> {
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
struct FunctionFrame<'syn> {
    num_registers: u8,
    function_name: &'syn str,
}

impl<'syn> FunctionFrame<'syn> {
    fn new(function_name: &'syn str) -> Self {
        Self {
            num_registers: 0,
            function_name,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ErrorKind<'syn> {
    InvalidMainDeclaration,
    InvalidShadowing(&'syn str),
    NotInsideFunction,
    UndeclaredSymbol,
    SymbolIsNotVariable,
}

#[derive(Debug, Copy, Clone)]
pub struct Error<'syn>(Spanned<ErrorKind<'syn>>);

impl<'syn> Error<'syn> {
    pub fn render(&self, source_code: &str) -> String {
        let Spanned(kind, span) = &self.0;
        match kind {
            ErrorKind::InvalidMainDeclaration => {
                format!(
                    "The 'main' symbol in the outer scope must be a function instead of a {}.",
                    span.render(source_code)
                )
            }
            ErrorKind::InvalidShadowing(identifier) => {
                format!(
                    "Shadowing of symbol '{}' with {} is not allowed at this scope.",
                    identifier,
                    span.render(source_code)
                )
            }
            ErrorKind::NotInsideFunction => {
                format!(
                    "Operation {} is only valid inside of a function.",
                    span.render(source_code)
                )
            }
            ErrorKind::UndeclaredSymbol => {
                format!(
                    "Symbol {} is undeclared or not available at that scope.",
                    span.render(source_code)
                )
            }
            ErrorKind::SymbolIsNotVariable => {
                format!(
                    "Only variables are allowed in the place of {}.",
                    span.render(source_code)
                )
            }
        }
    }
}
