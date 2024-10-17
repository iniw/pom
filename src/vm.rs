use std::mem;

use crate::{
    pool::{Handle, Pool},
    syn::{BinaryOp, Expr, Literal, Stmt},
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
            match dbg!(op) {
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
                Op::Load { dst, src } => {
                    *self.reg(dst) = *self.reg(src);
                }
                Op::LoadImm { dst, imm } => {
                    *self.reg(dst) = Reg(imm);
                }
                Op::Add { dst, left, right } => {
                    *self.reg(dst) = *self.reg(left) + *self.reg(right);
                }
                Op::Sub { dst, left, right } => {
                    *self.reg(dst) = *self.reg(left) - *self.reg(right);
                }
                Op::Div { dst, left, right } => {
                    *self.reg(dst) = *self.reg(left) / *self.reg(right);
                }
                Op::Mul { dst, left, right } => {
                    *self.reg(dst) = *self.reg(left) * *self.reg(right);
                }
                Op::Halt => {
                    break;
                }
            }
            dbg!(&self.regs);
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

    Halt,
}

use Op::*;

pub struct Generator {
    program: Vec<Op>,
    stack_frames_reg_count: Vec<u8>,
}

impl Generator {
    pub fn new() -> Self {
        Self {
            program: vec![Call { addr: 2 }, Halt],
            stack_frames_reg_count: Vec::new(),
        }
    }

    pub fn generate(mut self, stmts: Pool<Stmt>, exprs: Pool<Expr>) -> Vec<Op> {
        for stmt in stmts.handles() {
            self.generate_statement(&stmts, &exprs, stmt);
        }

        self.program
    }

    fn generate_statement(&mut self, stmts: &Pool<Stmt>, exprs: &Pool<Expr>, stmt: Handle<Stmt>) {
        match stmts.get(stmt) {
            Stmt::Expr(expr) => {
                self.stack_frames_reg_count.push(0);

                let prologue_addr = self.generate_prologue();
                self.generate_expression(exprs, *expr);
                self.generate_epilogue();
                self.patch_prologue(prologue_addr);

                unsafe { self.stack_frames_reg_count.pop().unwrap_unchecked() };
            }
            Stmt::Empty => (),
        }
    }

    fn generate_expression(&mut self, exprs: &Pool<Expr>, expr: Handle<Expr>) -> u8 {
        match exprs.get(expr) {
            Expr::Binary { left, op, right } => {
                let left = self.generate_expression(exprs, *left);
                let right = self.generate_expression(exprs, *right);

                let dst = self.allocate_reg();
                let op = match op {
                    BinaryOp::Add => Add { dst, left, right },
                    BinaryOp::Sub => Sub { dst, left, right },
                    BinaryOp::Div => Div { dst, left, right },
                    BinaryOp::Mul => Mul { dst, left, right },
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
        }
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
        // Update the previously added `Reserve` op with the number of allocated registers
        *unsafe { self.program.get_unchecked_mut(prologue_addr) } = Reserve {
            num_regs: *unsafe { self.stack_frames_reg_count.last_mut().unwrap_unchecked() },
        };
    }

    #[inline(always)]
    fn allocate_reg(&mut self) -> u8 {
        let count = unsafe { self.stack_frames_reg_count.last_mut().unwrap_unchecked() };
        mem::replace(count, *count + 1)
    }
}

impl Default for Generator {
    fn default() -> Self {
        Self::new()
    }
}
