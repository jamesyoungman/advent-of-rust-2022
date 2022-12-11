use std::fmt;
use std::fmt::Write;
use std::str;

use lib::error::Fail;
use sscanf::scanf;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
struct ClockValue(i64);

const PIXEL_SET: char = '\u{2588}';
const PIXEL_UNSET: char = '\u{2592}';

impl ClockValue {
    fn succ(&self) -> ClockValue {
        ClockValue(self.0 + 1)
    }

    fn to_pixel_position(self) -> usize {
        (self.0 as usize - 1) % 40
    }

    fn to_display_line_offset(self) -> usize {
        40 * ((self.0 as usize - 1) / 40)
    }
}

impl fmt::Display for ClockValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
struct RegisterValue(i64);

impl fmt::Display for RegisterValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

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

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Noop => f.write_str("noop"),
            Instruction::Addx(n) => write!(f, "addx {}", n),
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
    x: RegisterValue,
}

fn is_sample_cycle(tickval: ClockValue) -> bool {
    const BOUNDARIES: [i64; 6] = [20, 60, 100, 140, 180, 220];
    BOUNDARIES.into_iter().any(|boundary| boundary == tickval.0)
}

fn cpu_is_drawing(xregister: RegisterValue, pixpos: usize) -> bool {
    let sprite_left: i64 = xregister.0 - 1;
    let sprite_right: i64 = xregister.0 + 1;
    let p = pixpos as i64;
    p >= sprite_left && p <= sprite_right
}

#[test]
fn test_cpu_is_drawing() {
    assert!(cpu_is_drawing(RegisterValue(-1), 0));
    assert!(!cpu_is_drawing(RegisterValue(-1), 1));
    assert!(!cpu_is_drawing(RegisterValue(-1), 2));

    assert!(cpu_is_drawing(RegisterValue(0), 0));
    assert!(cpu_is_drawing(RegisterValue(0), 1));
    assert!(!cpu_is_drawing(RegisterValue(0), 2));

    assert!(cpu_is_drawing(RegisterValue(1), 0));
    assert!(cpu_is_drawing(RegisterValue(1), 1));
    assert!(cpu_is_drawing(RegisterValue(1), 2));
    assert!(!cpu_is_drawing(RegisterValue(1), 3));
}

impl Cpu {
    fn new() -> Cpu {
        Cpu {
            x: RegisterValue(1),
        }
    }

    fn simulate_instruction<F>(
        &mut self,
        oldclock: ClockValue,
        insn: &Instruction,
        mut ticker: F,
    ) -> ClockValue
    where
        F: FnMut(ClockValue, RegisterValue),
    {
        match insn {
            Instruction::Noop => {
                ticker(oldclock, self.x);
                oldclock.succ()
            }
            Instruction::Addx(increment) => {
                // 2 cycles
                // Complete the first cycle and start the second
                ticker(oldclock, self.x);
                // The X register is updated _after_ the second cycle.
                ticker(oldclock.succ(), self.x);
                self.x.0 += increment;
                oldclock.succ().succ()
            }
        }
    }
}

fn one_part1_instruction(
    cpu: &mut Cpu,
    clock: ClockValue,
    inst: &Instruction,
) -> (ClockValue, Option<i64>) {
    let mut sample: Option<i64> = None;
    let ticker = |clock: ClockValue, x: RegisterValue| {
        if is_sample_cycle(clock) {
            let result = clock.0 * x.0;
            sample = Some(result);
        }
    };
    let newclock = cpu.simulate_instruction(clock, inst, ticker);
    (newclock, sample)
}

#[test]
fn test_simulate() {
    let mut cpu = Cpu::new();
    assert_eq!(
        one_part1_instruction(&mut cpu, ClockValue(0), &Instruction::Noop),
        (ClockValue(1), None)
    );
    assert_eq!(cpu.x, RegisterValue(1));
    assert_eq!(
        one_part1_instruction(&mut cpu, ClockValue(1), &Instruction::Addx(3)),
        (ClockValue(3), None)
    );
    assert_eq!(cpu.x, RegisterValue(4));
    assert_eq!(
        one_part1_instruction(&mut cpu, ClockValue(3), &Instruction::Addx(-5)),
        (ClockValue(5), None)
    );
    assert_eq!(cpu.x, RegisterValue(-1));
}

#[test]
fn test_simulate_samples() {
    let mut cpu = Cpu::new();

    // This instruction does not yet hit a sample point.
    assert_eq!(
        one_part1_instruction(&mut cpu, ClockValue(18), &Instruction::Addx(5)),
        (ClockValue(20), None)
    );
    assert_eq!(cpu.x, RegisterValue(6));
    // This instruction hits a sample point
    assert_eq!(
        one_part1_instruction(&mut cpu, ClockValue(20), &Instruction::Noop),
        (ClockValue(21), Some(20 * 6))
    );
    assert_eq!(cpu.x, RegisterValue(6));

    // This instruction hits a sample point before its final cycle
    // (i.e. before the add has taken effect).
    assert_eq!(
        one_part1_instruction(&mut cpu, ClockValue(19), &Instruction::Addx(5)),
        (ClockValue(21), Some(20 * 6))
    );
    assert_eq!(cpu.x, RegisterValue(11));
}

fn part1(program: &[Instruction]) -> i64 {
    let mut tot: i64 = 0;
    let mut cpu = Cpu::new();
    let mut clock: ClockValue = ClockValue(1);
    for insn in program.iter() {
        let (newclock, sample) = one_part1_instruction(&mut cpu, clock, insn);
        if let Some(v) = sample {
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
fn test_small_example() {
    let program: Vec<Instruction> = concat!("noop\n", "addx 3\n", "addx -5\n")
        .split_terminator('\n')
        .map(Instruction::try_from)
        .collect::<Result<Vec<Instruction>, Fail>>()
        .expect("small example should be a valid program");
    let mut cpu = Cpu::new();
    //let do_nothing_ticker = |_c: ClockValue, _r: RegisterValue| ();
    assert_eq!(
        one_part1_instruction(&mut cpu, ClockValue(1), &program[0]),
        (ClockValue(2), None)
    );
    assert_eq!(
        one_part1_instruction(&mut cpu, ClockValue(2), &program[1]),
        (ClockValue(4), None)
    );
    assert_eq!(
        one_part1_instruction(&mut cpu, ClockValue(1), &program[0]),
        (ClockValue(2), None)
    );
}

#[test]
fn test_part1() {
    let program: Vec<Instruction> = EXAMPLE
        .split_terminator('\n')
        .map(Instruction::try_from)
        .collect::<Result<Vec<Instruction>, Fail>>()
        .expect("example should be a valid program");
    assert_eq!(part1(&program), 13140);
}

fn on_tick(clock: ClockValue, x: RegisterValue, pixels: &mut [char]) {
    let pixpos = clock.to_pixel_position();
    if cpu_is_drawing(x, pixpos) {
        pixels[clock.to_display_line_offset() + pixpos] = PIXEL_SET
    }
}

fn part2(program: &[Instruction]) -> String {
    const SCREEN_LEN: usize = 240;
    let mut pixels: Vec<char> = Vec::with_capacity(SCREEN_LEN);
    pixels.resize(SCREEN_LEN, PIXEL_UNSET);

    let mut cpu = Cpu::new();
    let mut clock: ClockValue = ClockValue(1);

    let mut ticker = |clock: ClockValue, x: RegisterValue| {
        on_tick(clock, x, &mut pixels);
    };
    for insn in program.iter() {
        clock = cpu.simulate_instruction(clock, insn, &mut ticker);
    }
    let mut result: String = String::new();
    for (i, chunk) in pixels.chunks(40).enumerate() {
        let chunk_str: String = chunk.iter().collect();
        writeln!(
            result,
            "Cycle {:>3} -> {} <- Cycle {:>3}",
            (i * 40) + 1,
            chunk_str,
            (i * 40) + 40
        )
        .expect("output write error");
    }
    result
}

#[test]
fn test_part2() {
    let program: Vec<Instruction> = EXAMPLE
        .split_terminator('\n')
        .map(Instruction::try_from)
        .collect::<Result<Vec<Instruction>, Fail>>()
        .expect("example should be a valid program");
    let output = part2(&program);
    assert_eq!(
        output,
        concat!(
            "Cycle   1 -> ██▒▒██▒▒██▒▒██▒▒██▒▒██▒▒██▒▒██▒▒██▒▒██▒▒ <- Cycle  40\n",
            "Cycle  41 -> ███▒▒▒███▒▒▒███▒▒▒███▒▒▒███▒▒▒███▒▒▒███▒ <- Cycle  80\n",
            "Cycle  81 -> ████▒▒▒▒████▒▒▒▒████▒▒▒▒████▒▒▒▒████▒▒▒▒ <- Cycle 120\n",
            "Cycle 121 -> █████▒▒▒▒▒█████▒▒▒▒▒█████▒▒▒▒▒█████▒▒▒▒▒ <- Cycle 160\n",
            "Cycle 161 -> ██████▒▒▒▒▒▒██████▒▒▒▒▒▒██████▒▒▒▒▒▒████ <- Cycle 200\n",
            "Cycle 201 -> ███████▒▒▒▒▒▒▒███████▒▒▒▒▒▒▒███████▒▒▒▒▒ <- Cycle 240\n",
        )
    );
}

fn main() {
    let text = str::from_utf8(include_bytes!("input.txt")).expect("valid input file");
    let program: Vec<Instruction> = text
        .split_terminator('\n')
        .map(Instruction::try_from)
        .collect::<Result<Vec<Instruction>, Fail>>()
        .expect("puzzle input should be a valid program");
    println!("Day 10 part 1: {}", part1(&program));
    println!("Day 10 part 2:\n{}", part2(&program));
}
