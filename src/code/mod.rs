use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
    vec::IntoIter,
};

use byteorder::{BigEndian, ByteOrder, ReadBytesExt, WriteBytesExt};

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Instructions(pub Vec<u8>);

impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Deref for Instructions {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl IntoIterator for Instructions {
    type Item = u8;

    type IntoIter = IntoIter<u8>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ret = String::new();
        let mut i = 0;

        while i < self.0.len() {
            let op: u8 = *self.0.get(i).unwrap();
            let op = unsafe { std::mem::transmute(op) };

            let (operands, read) = read_operands(&op, &self.0[i + 1..]);

            ret.push_str(&format!(
                "{:04} {}\n",
                i,
                format_instruction(&op, &operands)
            ));
            i = i + 1 + read; // op + operands
        }

        f.write_str(ret.as_str())
    }
}

#[inline(always)]
fn format_instruction(op: &Op, operands: &Vec<usize>) -> String {
    match op.operand_widths().len() {
        2 => format!("{} {} {}", op, operands[0], operands[1]),
        1 => format!("{} {}", op, operands[0]),
        0 => format!("{}", op),
        _ => panic!("unsuported operand width"),
    }
}

pub fn read_operands(op: &Op, instructions: &[u8]) -> (Vec<usize>, usize) {
    let mut operands = Vec::with_capacity(op.operand_widths().len());
    let mut offset = 0;

    for width in op.operand_widths() {
        match width {
            2 => {
                operands.push(read_u16(&instructions[offset..offset + 2]));
                offset += 2;
            }
            1 => {
                operands.push(instructions[offset] as usize);
                offset += 1;
            }
            _ => panic!(),
        }
    }

    (operands, offset)
}

pub(crate) fn read_u16(slice: &[u8]) -> usize {
    BigEndian::read_u16(slice) as usize
}

#[repr(u8)]
#[derive(Debug)]
pub enum Op {
    Const,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    True,
    False,
    Pop,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Op::Const => "OpConstant",
            Op::Add => "OpAdd",
            Op::Sub => "OpSub",
            Op::Mul => "OpMul",
            Op::Div => "OpDiv",
            Op::Mod => "OpMod",
            Op::Pop => "OpPop",
            Op::True => "OpTrue",
            Op::False => "OpFalse",
        };
        f.write_str(s)
    }
}

impl Op {
    pub fn operand_widths(&self) -> Vec<usize> {
        match self {
            Op::Const => vec![2],
            Op::Add => vec![],
            Op::Sub => vec![],
            Op::Mul => vec![],
            Op::Div => vec![],
            Op::Mod => vec![],
            Op::True => vec![],
            Op::False => vec![],
            Op::Pop => vec![],
        }
    }
}

pub fn make(op: Op, operands: &Vec<usize>) -> Instructions {
    let widths = op.operand_widths();
    let mut instruction = Vec::new();

    instruction.push(op as u8);

    operands
        .iter()
        .zip(widths)
        .for_each(|(operand, width)| match width {
            2 => instruction.write_u16::<BigEndian>(*operand as u16).unwrap(),
            1 => instruction.write_u8(*operand as u8).unwrap(),
            _ => unreachable!(),
        });

    Instructions(instruction)
}

pub(crate) fn concat_instructions(ins: Vec<Instructions>) -> Instructions {
    Instructions(ins.into_iter().flatten().collect::<Vec<u8>>())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn make_should_work() {
        let tests = [
            (
                vec![make(Op::Const, &vec![65534])],
                vec![Op::Const as u8, 255, 254],
                "0000 OpConstant 65534\n",
            ),
            (
                vec![make(Op::Add, &vec![])],
                vec![Op::Add as u8],
                "0000 OpAdd\n",
            ),
            (
                vec![
                    make(Op::Add, &vec![]),
                    make(Op::Const, &vec![0]),
                    make(Op::Const, &vec![1]),
                ],
                vec![Op::Add as u8, Op::Const as u8, 0, 0, Op::Const as u8, 0, 1],
                "0000 OpAdd\n0001 OpConstant 0\n0004 OpConstant 1\n",
            ),
        ];

        tests.into_iter().for_each(|test| {
            let instructions = concat_instructions(test.0);
            assert_eq!(test.1, instructions.0);
            assert_eq!(&test.2, &format!("{}", instructions));
        })
    }
}