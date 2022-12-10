use std::str;

use lib::error::Fail;
use sscanf::scanf;

#[derive(Debug, Eq, PartialEq)]
enum Instruction {
    Noop,
    Addx(i64),
}

impl TryFrom<&str> for Instruction {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Instruction, Fail> {
        if s == "noop" {
            Ok(Instruction::Noop)
        } else if let Ok(n) = scanf!(s, "addx {i64:r10}") {
            Ok(Instruction::Addx(n))
        } else {
            Err(Fail(format!("unrecognised instruction {s}")))
        }
    }
}

impl Instruction {
    fn cycles(&self) -> i64 {
        match self {
            Instruction::Noop => 1,
            Instruction::Addx(_) => 2,
        }
    }
}

#[test]
fn test_instruction_try_from() {
    assert_eq!(Instruction::try_from("noop").unwrap(), Instruction::Noop);
    assert_eq!(
        Instruction::try_from("addx -4").unwrap(),
        Instruction::Addx(-4)
    );
    assert!(Instruction::try_from("jmp 2").is_err());
}

struct Cpu {
    x: i64,
}

fn sample_cycle(before: i64, after: i64) -> Option<i64> {
    const BOUNDARIES: [i64; 6] = [20, 60, 100, 140, 180, 220];
    BOUNDARIES
        .into_iter()
        .find(|&boundary| before < boundary && after >= boundary)
}

impl Cpu {
    fn new() -> Cpu {
        Cpu { x: 1 }
    }

    /// Simulate an instruction, returning the new clock value.
    // If execution spans a cycle which is a multiple of 20, return
    // the cycle number and x register value occuring on that cycle.
    fn simulate_instruction(&mut self, oldclock: i64, insn: &Instruction) -> (i64, Option<i64>) {
        let duration = insn.cycles();
        let newx = match insn {
            Instruction::Noop => self.x,
            Instruction::Addx(increment) => self.x + increment,
        };
        let newclock = oldclock + duration;
        //dbg!(&insn);
        //dbg!(oldclock);
        //dbg!(newclock);
        //dbg!(self.x);
        let sample = match sample_cycle(oldclock, newclock) {
            Some(sample_cycle) => {
                //dbg!(sample_cycle);
                Some(
                    sample_cycle
                        * if sample_cycle == newclock {
                            newx
                        } else {
                            self.x
                        },
                )
            }
            None => None,
        };
        self.x = newx;
        (newclock, sample)
    }
}

#[test]
fn test_simulate() {
    let mut cpu = Cpu::new();
    assert_eq!(cpu.simulate_instruction(0, &Instruction::Noop), (1, None));
    assert_eq!(cpu.x, 1);
    assert_eq!(
        cpu.simulate_instruction(1, &Instruction::Addx(3)),
        (3, None)
    );
    assert_eq!(cpu.x, 4);
    assert_eq!(
        cpu.simulate_instruction(3, &Instruction::Addx(-5)),
        (5, None)
    );
    assert_eq!(cpu.x, -1);
}

#[test]
fn test_simulate_samples() {
    let mut cpu = Cpu::new();

    // This instruction hits a sample point on its final cycle.
    assert_eq!(
        cpu.simulate_instruction(18, &Instruction::Addx(5)),
        (20, Some(20 * 6))
    );

    // This instruction hits a sample point before its final cycle.
    assert_eq!(
        cpu.simulate_instruction(19, &Instruction::Addx(5)),
        (21, Some(20 * 6))
    );
}

fn part1(program: &[Instruction]) -> i64 {
    let mut tot: i64 = 0;
    let mut cpu = Cpu::new();
    let mut clock: i64 = 1;
    for insn in program.iter() {
        let (newclock, sample) = cpu.simulate_instruction(clock, insn);
        if let Some(v) = sample {
            //println!("sample value: {v}");
            tot += v;
        }
        clock = newclock;
    }
    tot
}

#[cfg(test)]
const EXAMPLE: &str = concat!(
    "addx 15\n",
    "addx -11\n",
    "addx 6\n",
    "addx -3\n",
    "addx 5\n",
    "addx -1\n",
    "addx -8\n",
    "addx 13\n",
    "addx 4\n",
    "noop\n",
    "addx -1\n",
    "addx 5\n",
    "addx -1\n",
    "addx 5\n",
    "addx -1\n",
    "addx 5\n",
    "addx -1\n",
    "addx 5\n",
    "addx -1\n",
    "addx -35\n",
    "addx 1\n",
    "addx 24\n",
    "addx -19\n",
    "addx 1\n",
    "addx 16\n",
    "addx -11\n",
    "noop\n",
    "noop\n",
    "addx 21\n",
    "addx -15\n",
    "noop\n",
    "noop\n",
    "addx -3\n",
    "addx 9\n",
    "addx 1\n",
    "addx -3\n",
    "addx 8\n",
    "addx 1\n",
    "addx 5\n",
    "noop\n",
    "noop\n",
    "noop\n",
    "noop\n",
    "noop\n",
    "addx -36\n",
    "noop\n",
    "addx 1\n",
    "addx 7\n",
    "noop\n",
    "noop\n",
    "noop\n",
    "addx 2\n",
    "addx 6\n",
    "noop\n",
    "noop\n",
    "noop\n",
    "noop\n",
    "noop\n",
    "addx 1\n",
    "noop\n",
    "noop\n",
    "addx 7\n",
    "addx 1\n",
    "noop\n",
    "addx -13\n",
    "addx 13\n",
    "addx 7\n",
    "noop\n",
    "addx 1\n",
    "addx -33\n",
    "noop\n",
    "noop\n",
    "noop\n",
    "addx 2\n",
    "noop\n",
    "noop\n",
    "noop\n",
    "addx 8\n",
    "noop\n",
    "addx -1\n",
    "addx 2\n",
    "addx 1\n",
    "noop\n",
    "addx 17\n",
    "addx -9\n",
    "addx 1\n",
    "addx 1\n",
    "addx -3\n",
    "addx 11\n",
    "noop\n",
    "noop\n",
    "addx 1\n",
    "noop\n",
    "addx 1\n",
    "noop\n",
    "noop\n",
    "addx -13\n",
    "addx -19\n",
    "addx 1\n",
    "addx 3\n",
    "addx 26\n",
    "addx -30\n",
    "addx 12\n",
    "addx -1\n",
    "addx 3\n",
    "addx 1\n",
    "noop\n",
    "noop\n",
    "noop\n",
    "addx -9\n",
    "addx 18\n",
    "addx 1\n",
    "addx 2\n",
    "noop\n",
    "noop\n",
    "addx 9\n",
    "noop\n",
    "noop\n",
    "noop\n",
    "addx -1\n",
    "addx 2\n",
    "addx -37\n",
    "addx 1\n",
    "addx 3\n",
    "noop\n",
    "addx 15\n",
    "addx -21\n",
    "addx 22\n",
    "addx -6\n",
    "addx 1\n",
    "noop\n",
    "addx 2\n",
    "addx 1\n",
    "noop\n",
    "addx -10\n",
    "noop\n",
    "noop\n",
    "addx 20\n",
    "addx 1\n",
    "addx 2\n",
    "addx 2\n",
    "addx -6\n",
    "addx -11\n",
    "noop\n",
    "noop\n",
    "noop\n",
);

#[test]
fn test_part1() {
    let program: Vec<Instruction> = EXAMPLE
        .split_terminator('\n')
        .map(Instruction::try_from)
        .collect::<Result<Vec<Instruction>, Fail>>()
        .expect("example should be a valid program");
    assert_eq!(part1(&program), 13140);
}

fn main() {
    let text = str::from_utf8(include_bytes!("input.txt")).expect("valid input file");
    let program: Vec<Instruction> = text
        .split_terminator('\n')
        .map(Instruction::try_from)
        .collect::<Result<Vec<Instruction>, Fail>>()
        .expect("puzzle input should be a valid program");
    println!("Day 10 part 1: {}", part1(&program));
}
