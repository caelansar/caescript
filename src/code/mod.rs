use std::{fmt::Display, io::Cursor, ops::Deref};

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Instructions(Vec<u8>);

impl Deref for Instructions {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
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
            i = i + 1 + read;
        }

        f.write_str(ret.as_str())
    }
}

#[inline(always)]
fn format_instruction(op: &Op, operands: &Vec<usize>) -> String {
    match op.operand_widths().len() {
        2 => format!("{} {} {}", op.name(), operands[0], operands[1]),
        1 => format!("{} {}", op.name(), operands[0]),
        0 => format!("{}", op.name()),
        _ => panic!("unsuported operand width"),
    }
}

pub fn read_operands(op: &Op, instructions: &[u8]) -> (Vec<usize>, usize) {
    let mut operands = Vec::with_capacity(op.operand_widths().len());
    let mut offset = 0;

    for width in op.operand_widths() {
        match width {
            2 => {
                operands.push(
                    Cursor::new(&instructions[offset..offset + 2])
                        .read_u16::<BigEndian>()
                        .unwrap() as usize,
                );
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

#[repr(u8)]
#[derive(Debug)]
pub enum Op {
    Const,
}

impl Op {
    pub fn name(&self) -> &str {
        match self {
            Op::Const => "OpConstant",
        }
    }

    pub fn operand_widths(&self) -> Vec<usize> {
        match self {
            Op::Const => vec![2],
        }
    }
}

pub fn make(op: Op, operands: &Vec<usize>) -> Instructions {
    let _ = op.operand_widths();
    let mut instruction = Vec::new();

    instruction.push(op as u8);
    instruction
        .write_u16::<BigEndian>(operands[0] as u16)
        .unwrap();

    Instructions(instruction)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn make_should_work() {
        let tests = [(
            Op::Const,
            vec![65534],
            vec![Op::Const as u8, 255, 254],
            "0000 OpConstant 65534\n",
        )];

        tests.into_iter().for_each(|test| {
            let instrucations = make(test.0, &test.1);
            assert_eq!(test.2, *instrucations);
            assert_eq!(&test.3, &format!("{}", instrucations));
        })
    }
}
