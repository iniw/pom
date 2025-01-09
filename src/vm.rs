use std::ops;

pub mod cpu;
pub mod error;
pub mod generator;

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

type Word = u32;

#[derive(Debug, Copy, Clone, Default)]
#[repr(transparent)]
pub struct Reg(Word);

impl ops::Add for Reg {
    type Output = Reg;

    #[inline(always)]
    fn add(self, rhs: Self) -> Self::Output {
        Reg(self.0 + rhs.0)
    }
}

impl ops::Add<Word> for Reg {
    type Output = Reg;

    #[inline(always)]
    fn add(self, rhs: Word) -> Self::Output {
        Reg(self.0 + rhs)
    }
}

impl ops::Add<Reg> for Word {
    type Output = Reg;

    #[inline(always)]
    fn add(self, rhs: Reg) -> Self::Output {
        Reg(self + rhs.0)
    }
}

impl ops::Sub for Reg {
    type Output = Reg;

    #[inline(always)]
    fn sub(self, rhs: Self) -> Self::Output {
        Reg(self.0 - rhs.0)
    }
}

impl ops::Sub<Word> for Reg {
    type Output = Reg;

    #[inline(always)]
    fn sub(self, rhs: Word) -> Self::Output {
        Reg(self.0 - rhs)
    }
}

impl ops::Sub<Reg> for Word {
    type Output = Reg;

    #[inline(always)]
    fn sub(self, rhs: Reg) -> Self::Output {
        Reg(self - rhs.0)
    }
}

impl ops::Div for Reg {
    type Output = Reg;

    #[inline(always)]
    fn div(self, rhs: Self) -> Self::Output {
        Reg(self.0 / rhs.0)
    }
}

impl ops::Div<Word> for Reg {
    type Output = Reg;

    #[inline(always)]
    fn div(self, rhs: Word) -> Self::Output {
        Reg(self.0 / rhs)
    }
}

impl ops::Div<Reg> for Word {
    type Output = Reg;

    #[inline(always)]
    fn div(self, rhs: Reg) -> Self::Output {
        Reg(self / rhs.0)
    }
}

impl ops::Mul for Reg {
    type Output = Reg;

    #[inline(always)]
    fn mul(self, rhs: Self) -> Self::Output {
        Reg(self.0 * rhs.0)
    }
}

impl ops::Mul<Word> for Reg {
    type Output = Reg;

    #[inline(always)]
    fn mul(self, rhs: Word) -> Self::Output {
        Reg(self.0 * rhs)
    }
}

impl ops::Mul<Reg> for Word {
    type Output = Reg;

    #[inline(always)]
    fn mul(self, rhs: Reg) -> Self::Output {
        Reg(self * rhs.0)
    }
}
