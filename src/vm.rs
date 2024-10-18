use std::{collections::HashMap, mem};

use crate::{
    pool::{Handle, Pool},
    syn::{BinaryOp, Expr, Literal, Stmt, SymbolInfo, VarInfo},
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
                Op::AddRegImm { dst, left, right } => *self.reg(dst) = *self.reg(left) + right,
                Op::AddImmReg { dst, left, right } => *self.reg(dst) = left + *self.reg(right),

                Op::Sub { dst, left, right } => *self.reg(dst) = *self.reg(left) - *self.reg(right),
                Op::SubRegImm { dst, left, right } => *self.reg(dst) = *self.reg(left) - right,
                Op::SubImmReg { dst, left, right } => *self.reg(dst) = left - *self.reg(right),

                Op::Div { dst, left, right } => *self.reg(dst) = *self.reg(left) / *self.reg(right),
                Op::DivRegImm { dst, left, right } => *self.reg(dst) = *self.reg(left) / right,
                Op::DivImmReg { dst, left, right } => *self.reg(dst) = left / *self.reg(right),

                Op::Mul { dst, left, right } => *self.reg(dst) = *self.reg(left) * *self.reg(right),
                Op::MulRegImm { dst, left, right } => *self.reg(dst) = *self.reg(left) * right,
                Op::MulImmReg { dst, left, right } => *self.reg(dst) = left * *self.reg(right),

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
    AddRegImm { dst: u8, left: u8, right: Word },
    AddImmReg { dst: u8, left: Word, right: u8 },

    Sub { dst: u8, left: u8, right: u8 },
    SubRegImm { dst: u8, left: u8, right: Word },
    SubImmReg { dst: u8, left: Word, right: u8 },

    Div { dst: u8, left: u8, right: u8 },
    DivRegImm { dst: u8, left: u8, right: Word },
    DivImmReg { dst: u8, left: Word, right: u8 },

    Mul { dst: u8, left: u8, right: u8 },
    MulRegImm { dst: u8, left: u8, right: Word },
    MulImmReg { dst: u8, left: Word, right: u8 },

    Print { reg: u8 },

    Halt,
}

use Op::*;

pub struct Generator<'syn> {
    program: Vec<Op>,
    frames: Vec<Frame<'syn>>,
    override_next_allocated_register: Option<u8>,
}

impl<'syn> Generator<'syn> {
    pub fn new() -> Self {
        Self {
            program: vec![Call { addr: 2 }, Halt],
            frames: Vec::new(),
            override_next_allocated_register: None,
        }
    }

    // FIXME: Propagate errors here and in the generators.
    pub fn generate(mut self, stmts: Pool<Stmt<'syn>>, exprs: Pool<Expr<'syn>>) -> Vec<Op> {
        for stmt in stmts.handles() {
            if let Stmt::SymbolDecl {
                name: "main",
                info: SymbolInfo::Fn(_),
            } = stmts.get(stmt)
            {
                self.generate_statement(&stmts, &exprs, stmt);
                return self.program;
            }
        }

        panic!("'main' not found");
    }

    fn generate_statement(
        &mut self,
        stmts: &Pool<Stmt<'syn>>,
        exprs: &Pool<Expr<'syn>>,
        stmt: Handle<Stmt<'syn>>,
    ) {
        match stmts.get(stmt) {
            Stmt::Expr(expr) => {
                self.generate_expression(stmts, exprs, *expr);
            }
            Stmt::SymbolDecl { name, info } => match info {
                SymbolInfo::Fn(expr) => {
                    self.push_frame();

                    let prologue_addr = self.generate_prologue();
                    self.generate_expression(stmts, exprs, *expr);

                    self.generate_epilogue();
                    self.patch_prologue(prologue_addr);

                    self.pop_frame();
                }
                SymbolInfo::Var(VarInfo::Value(expr)) => {
                    let reg = self.allocate_reg();
                    self.current_frame().symbols.insert(name, reg);

                    self.override_next_allocated_register = Some(reg);
                    self.generate_expression(stmts, exprs, *expr);
                }

                _ => unimplemented!(),
            },
            Stmt::Print(expr) => {
                let reg = self.generate_expression(stmts, exprs, *expr);
                self.program.push(Print { reg });
            }
        }
    }

    fn generate_expression(
        &mut self,
        stmts: &Pool<Stmt<'syn>>,
        exprs: &Pool<Expr<'syn>>,
        expr: Handle<Expr<'syn>>,
    ) -> u8 {
        match exprs.get(expr) {
            Expr::Binary { left, op, right } => {
                let dst = self.allocate_reg();
                let op = match (exprs.get(*left), op, exprs.get(*right)) {
                    (_, BinaryOp::Add, Expr::Literal(Literal::Number(imm))) => {
                        let left = self.generate_expression(stmts, exprs, *left);
                        AddRegImm {
                            dst,
                            left,
                            right: *imm,
                        }
                    }
                    (Expr::Literal(Literal::Number(imm)), BinaryOp::Add, _) => {
                        let right = self.generate_expression(stmts, exprs, *right);
                        AddImmReg {
                            dst,
                            left: *imm,
                            right,
                        }
                    }
                    (_, BinaryOp::Sub, Expr::Literal(Literal::Number(imm))) => {
                        let left = self.generate_expression(stmts, exprs, *left);
                        SubRegImm {
                            dst,
                            left,
                            right: *imm,
                        }
                    }
                    (Expr::Literal(Literal::Number(imm)), BinaryOp::Sub, _) => {
                        let right = self.generate_expression(stmts, exprs, *right);
                        SubImmReg {
                            dst,
                            left: *imm,
                            right,
                        }
                    }
                    (_, BinaryOp::Div, Expr::Literal(Literal::Number(imm))) => {
                        let left = self.generate_expression(stmts, exprs, *left);
                        DivRegImm {
                            dst,
                            left,
                            right: *imm,
                        }
                    }
                    (Expr::Literal(Literal::Number(imm)), BinaryOp::Div, _) => {
                        let right = self.generate_expression(stmts, exprs, *right);
                        DivImmReg {
                            dst,
                            left: *imm,
                            right,
                        }
                    }
                    (_, BinaryOp::Mul, Expr::Literal(Literal::Number(imm))) => {
                        let left = self.generate_expression(stmts, exprs, *left);
                        MulRegImm {
                            dst,
                            left,
                            right: *imm,
                        }
                    }
                    (Expr::Literal(Literal::Number(imm)), BinaryOp::Mul, _) => {
                        let right = self.generate_expression(stmts, exprs, *right);
                        MulImmReg {
                            dst,
                            left: *imm,
                            right,
                        }
                    }
                    _ => {
                        let left = self.generate_expression(stmts, exprs, *left);
                        let right = self.generate_expression(stmts, exprs, *right);
                        match op {
                            BinaryOp::Add => Add { dst, left, right },
                            BinaryOp::Sub => Sub { dst, left, right },
                            BinaryOp::Div => Div { dst, left, right },
                            BinaryOp::Mul => Mul { dst, left, right },
                        }
                    }
                };
                self.program.push(op);
                dst
            }
            Expr::Literal(literal) => match literal {
                Literal::Number(number) => {
                    let dst = self.allocate_reg();
                    self.program.push(LoadImm { dst, imm: *number });
                    dst
                }
            },
            Expr::Symbol(name) => {
                #[cfg(debug_assertions)]
                {
                    if let Some(reg) = self.current_frame().symbols.get(name) {
                        *reg
                    } else {
                        panic!("Undefined identifier {}", name)
                    }
                }
                #[cfg(not(debug_assertions))]
                {
                    unsafe { *self.current_frame().symbols.get(name).unwrap_unchecked() }
                }
            }
            Expr::Block(block_stmts) => {
                let dst = self.allocate_reg();
                for stmt in block_stmts {
                    self.generate_statement(stmts, exprs, *stmt);
                }
                dst
            }
        }
    }

    #[inline(always)]
    fn push_frame(&mut self) {
        self.frames.push(Frame::new());
    }

    #[inline(always)]
    fn pop_frame(&mut self) {
        #[cfg(debug_assertions)]
        self.frames
            .pop()
            .expect("This can't happen - a frame was just pushed.");

        #[cfg(not(debug_assertions))]
        unsafe {
            self.frames.pop().unwrap_unchecked()
        };
    }

    #[inline(always)]
    fn generate_prologue(&mut self) -> usize {
        // Push a dummy `Reserve`, will be patched up later
        self.program.push(Reserve { num_regs: 0 });
        self.program.len() - 1
    }

    #[inline(always)]
    fn generate_epilogue(&mut self) {
        self.program.push(Ret);
    }

    #[inline(always)]
    fn patch_prologue(&mut self, prologue_addr: usize) {
        #[cfg(debug_assertions)]
        {
            self.program[prologue_addr] = Reserve {
                num_regs: self.current_frame().num_registers,
            };
        }
        #[cfg(not(debug_assertions))]
        {
            // Update the previously added `Reserve` op with the number of allocated registers
            *unsafe { self.program.get_unchecked_mut(prologue_addr) } = Reserve {
                num_regs: self.current_frame().num_registers,
            };
        }
    }

    #[inline(always)]
    fn allocate_reg(&mut self) -> u8 {
        if let Some(reg) = self.override_next_allocated_register.take() {
            return reg;
        }

        let count = &mut self.current_frame().num_registers;
        mem::replace(count, *count + 1)
    }

    #[inline(always)]
    fn current_frame(&mut self) -> &mut Frame<'syn> {
        #[cfg(debug_assertions)]
        {
            self.frames.last_mut().expect("There are no frames")
        }
        #[cfg(not(debug_assertions))]
        {
            unsafe { self.frames.last_mut().unwrap_unchecked() }
        }
    }
}

impl<'syn> Default for Generator<'syn> {
    fn default() -> Self {
        Self::new()
    }
}

struct Frame<'syn> {
    num_registers: u8,
    symbols: HashMap<&'syn str, u8>,
}

impl<'syn> Frame<'syn> {
    fn new() -> Self {
        Self {
            num_registers: 0,
            symbols: HashMap::new(),
        }
    }
}
