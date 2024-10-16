use std::mem;

pub struct Processor<'gen> {
    program: &'gen [u8],
    pc: Word,
    bp: Word,
    regs: Vec<Reg>,
}

impl<'gen> Processor<'gen> {
    pub fn new(program: &'gen [u8]) -> Self {
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

            match dbg!(self.fetch::<OpCode>()) {
                OpCode::Call => {
                    let addr = self.fetch::<Word>();

                    self.regs.push(Reg(self.pc));
                    self.pc = addr;
                }
                OpCode::Setup => {
                    self.regs.push(Reg(self.bp));
                    self.bp = self.regs.len() as Word;

                    let regs = self.fetch::<Reg>();
                    for _ in 0..regs {
                        self.regs.push(Reg(0));
                    }
                }
                OpCode::Ret => {
                    let regs = self.regs.len() - self.bp as usize;
                    for _ in 0..regs {
                        self.regs.pop();
                    }

                    self.bp = self.regs.pop().expect("Calling convention violated!").0;
                    self.pc = self.regs.pop().expect("Calling convention violated!").0;
                }
                OpCode::Load => {
                    let reg1 = self.fetch::<Reg>();
                    let reg2 = self.fetch::<Reg>();
                    *self.reg(reg1) = *self.reg(reg2);
                }
                OpCode::LoadImm => {
                    let reg = self.fetch::<Reg>();
                    let imm = self.fetch::<Word>();
                    *self.reg(reg) = Reg(imm);
                }
                OpCode::Add => {
                    let reg1 = self.fetch::<Reg>();
                    let reg2 = self.fetch::<Reg>();
                    let reg3 = self.fetch::<Reg>();
                    *self.reg(reg1) = *self.reg(reg2) + *self.reg(reg3);
                }
                OpCode::AddImm => {
                    let reg1 = self.fetch::<Reg>();
                    let reg2 = self.fetch::<Reg>();
                    let imm = self.fetch::<Word>();
                    *self.reg(reg1) = *self.reg(reg2) + imm;
                }
                OpCode::Halt => {
                    println!("Halting, bye!");
                    break;
                }
                OpCode::Illegal => {
                    eprintln!("Illegal instruction, something went wrong.");
                    break;
                }
            }
            dbg!(&self.regs);
        }
    }

    #[inline(always)]
    fn reg(&mut self, idx: <Reg as Fetchable>::Output) -> &mut Reg {
        &mut self.regs[self.bp as usize + idx as usize]
    }

    #[inline(always)]
    fn fetch<T: Fetchable>(&mut self) -> T::Output {
        let size = mem::size_of::<T::Output>() as Word;
        let data = T::from_bytes(&self.program[self.pc as usize..(self.pc + size) as usize]);
        self.pc += size;
        data
    }
}

type Word = u32;

#[derive(Debug, Copy, Clone)]
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
enum OpCode {
    Call,
    Setup,
    Ret,

    Load,
    LoadImm,

    Add,
    AddImm,

    Halt,
    Illegal,
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => OpCode::Call,
            1 => OpCode::Setup,
            2 => OpCode::Ret,
            3 => OpCode::Load,
            4 => OpCode::LoadImm,
            5 => OpCode::Add,
            6 => OpCode::AddImm,
            7 => OpCode::Halt,
            _ => OpCode::Illegal,
        }
    }
}

trait Fetchable: Sized {
    type Output;

    fn from_bytes(bytes: &[u8]) -> Self::Output;
}

impl Fetchable for u8 {
    type Output = u8;

    #[inline(always)]
    fn from_bytes(bytes: &[u8]) -> Self::Output {
        bytes[0]
    }
}

impl Fetchable for u16 {
    type Output = u16;

    #[inline(always)]
    fn from_bytes(bytes: &[u8]) -> Self::Output {
        (bytes[0] as u16) | (bytes[1] as u16) << 8
    }
}

impl Fetchable for u32 {
    type Output = u32;

    #[inline(always)]
    fn from_bytes(bytes: &[u8]) -> Self::Output {
        (bytes[0] as u32)
            | (bytes[1] as u32) << 8
            | (bytes[2] as u32) << 16
            | (bytes[3] as u32) << 24
    }
}

impl Fetchable for u64 {
    type Output = u64;

    #[inline(always)]
    fn from_bytes(bytes: &[u8]) -> Self::Output {
        (bytes[0] as u64)
            | (bytes[1] as u64) << 8
            | (bytes[2] as u64) << 16
            | (bytes[3] as u64) << 24
            | (bytes[4] as u64) << 32
            | (bytes[5] as u64) << 40
            | (bytes[6] as u64) << 48
            | (bytes[7] as u64) << 56
    }
}

impl Fetchable for Reg {
    type Output = u8;

    #[inline(always)]
    fn from_bytes(bytes: &[u8]) -> Self::Output {
        <u8 as Fetchable>::from_bytes(bytes)
    }
}

impl Fetchable for OpCode {
    type Output = OpCode;

    #[inline(always)]
    fn from_bytes(bytes: &[u8]) -> Self::Output {
        OpCode::from(<u8 as Fetchable>::from_bytes(bytes))
    }
}

#[expect(dead_code)]
struct Generator {}
