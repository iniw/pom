use std::mem;

use super::{Op, Reg, Word};

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
                        .resize(self.regs.len() + num_regs as usize, Reg(0));
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
