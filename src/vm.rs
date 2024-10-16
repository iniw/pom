use std::mem;

pub struct Processor<'gen> {
    program: &'gen [Op],
    pc: Word,
    bp: Word,
    regs: Vec<Reg>,
}

impl<'gen> Processor<'gen> {
    pub fn new(program: &'gen [Op]) -> Self {
        Self {
            regs: Vec::new(),
            pc: 0,
            bp: 0,
            program,
        }
    }

    pub fn run(mut self) {
        loop {
            if self.pc as usize >= self.program.len() {
                println!("Program ended, bye!");
                break;
            }

            let pc = {
                let temp = self.pc;
                mem::replace(&mut self.pc, temp + 1)
            };
            match self.program[pc as usize] {
                Op::Call { addr } => {
                    let ret_addr = mem::replace(&mut self.pc, addr);
                    self.regs.push(Reg(ret_addr));
                }
                Op::Reserve { regs } => {
                    self.regs.push(Reg(self.bp));
                    self.bp = self.regs.len() as Word;

                    self.regs
                        .resize_with(self.regs.len() + regs as usize, Default::default);
                }
                Op::Ret => {
                    self.regs.truncate(self.bp as usize);

                    self.bp = self.regs.pop().expect("Calling convention violated!").0;
                    self.pc = self.regs.pop().expect("Calling convention violated!").0;
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
                Op::Halt => {
                    println!("Halting, bye!");
                    break;
                }
            }
            dbg!(&self.regs);
        }
    }

    #[inline(always)]
    fn reg(&mut self, idx: u8) -> &mut Reg {
        &mut self.regs[self.bp as usize + idx as usize]
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

#[derive(Debug, Copy, Clone)]
pub enum Op {
    Call { addr: Word },
    Reserve { regs: u8 },
    Ret,

    Load { dst: u8, src: u8 },
    LoadImm { dst: u8, imm: Word },

    Add { dst: u8, left: u8, right: u8 },

    Halt,
}

#[expect(dead_code)]
struct Generator {}
