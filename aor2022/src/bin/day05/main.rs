use std::fmt;
use std::str;

use regex::Regex;
use sscanf::scanf;

use lib::error::Fail;

#[derive(Debug, Clone, Copy)]
enum Crane {
    Model9000,
    Model9001,
}

#[derive(Debug, Eq, PartialEq)]
struct Move {
    count: usize,
    from: usize,
    to: usize,
}

fn parse_move(instruction: &str) -> Result<Move, Fail> {
    if let Some((count, from, to)) =
        scanf!(instruction, "move {} from {} to {}", usize, usize, usize)
    {
        Ok(Move {
            count,
            from: from - 1,
            to: to - 1,
        })
    } else {
        Err(Fail(format!(
            "invalid move {instruction}: not enough fields matched"
        )))
    }
}

#[test]
fn test_parse() {
    assert_eq!(
        parse_move("move 1 from 2 to 3").expect("valid move"),
        Move {
            count: 1,
            from: 1,
            to: 2
        }
    );
}

#[derive(Debug, Clone)]
struct State {
    stacks: Vec<Vec<char>>,
}

impl State {
    fn top_crates(&self) -> String {
        self.stacks
            .iter()
            .map(|st| st.last().copied().unwrap_or(' '))
            .collect()
    }

    fn apply_move_sequence(&mut self, moves: &[Move], crane_model: Crane) {
        for m in moves.iter() {
            let unmoved = self.stacks[m.from].len() - m.count;
            let mut items = self.stacks[m.from].split_off(unmoved);
            match crane_model {
                Crane::Model9000 => {
                    items.reverse();
                }
                Crane::Model9001 => (),
            }
            self.stacks[m.to].extend(items);
        }
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (ix, stack) in self.stacks.iter().enumerate() {
            write!(f, "{} ", ix + 1)?;
            for ch in stack.iter() {
                write!(f, "{}", ch)?;
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

fn parse_initial_state(state: &str) -> Result<State, Fail> {
    let end_rx = Regex::new(r"^( *\d)* *$").map_err(|e| Fail(e.to_string()))?;

    let mut stacks: Vec<Vec<char>> = Vec::new();
    for line in state.split_terminator('\n') {
        if end_rx.is_match(line) {
            break;
        }
        let chars: Vec<char> = line.chars().collect();
        for (column, chunk) in chars.chunks(4).enumerate() {
            if stacks.len() <= column {
                stacks.resize_with(column + 1, Vec::new);
            }
            match chunk {
                &[' ', ' ', ' ', ' '] | &[' ', ' ', ' '] => (),
                &['[', letter, ']', ..] => {
                    stacks[column].push(letter);
                }
                other => {
                    return Err(Fail(format!(
                        "initial-state diagram has unexpected line format: {line}; chunk looks like [{other:?}]"
                    )));
                }
            }
        }
    }
    for st in stacks.iter_mut() {
        st.reverse();
    }
    Ok(State { stacks })
}

#[test]
fn test_parse_initial_state() {
    let initial = concat!(
        "    [D]    \n",
        "[N] [C]    \n",
        "[Z] [M] [P]\n",
        " 1   2   3 \n"
    );
    let state = parse_initial_state(initial).expect("valid test data");
    assert_eq!(state.stacks[0].as_slice(), &['Z', 'N']);
    assert_eq!(state.stacks[1].as_slice(), &['M', 'C', 'D']);
    assert_eq!(state.stacks[2].as_slice(), &['P']);
    assert_eq!(state.stacks.len(), 3);
}

fn parse_input(input: &str) -> Result<(State, Vec<Move>), Fail> {
    match input.split_once("\n\n") {
        Some((state, moves)) => Ok((
            parse_initial_state(state)?,
            moves
                .split_terminator('\n')
                .map(parse_move)
                .collect::<Result<Vec<_>, _>>()?,
        )),
        None => Err(Fail("there should be a blank line".to_string())),
    }
}

fn main() {
    let (mut part1_state, moves) =
        parse_input(str::from_utf8(include_bytes!("input.txt")).expect("valid encoding"))
            .expect("valid input file");
    let mut part2_state = part1_state.clone();
    part1_state.apply_move_sequence(&moves, Crane::Model9000);
    println!("Day 05 part 1: {}", part1_state.top_crates());
    part2_state.apply_move_sequence(&moves, Crane::Model9001);
    println!("Day 05 part 2: {}", part2_state.top_crates());
}
