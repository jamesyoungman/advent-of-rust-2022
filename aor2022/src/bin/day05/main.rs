use std::fmt;
use std::str;

use regex::Regex;

use lib::error::Fail;

struct MoveParser {
    rx: Regex,
}

const MOVE_REGEX: &str = r"^move (\d+) from (\d+) to (\d+)$";

impl MoveParser {
    fn new() -> Result<MoveParser, Fail> {
        let rx = Regex::new(MOVE_REGEX).map_err(|e| Fail(e.to_string()))?;
        Ok(MoveParser { rx })
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Move {
    count: usize,
    from: usize,
    to: usize,
}

impl MoveParser {
    fn parse(&self, instruction: &str) -> Result<Move, Fail> {
        let invalid = |why: &str| Fail(format!("invalid move {instruction}: {why}"));
        match self.rx.captures(instruction) {
            None => Err(invalid("does not match regex")),
            Some(captures) => match (captures.get(1), captures.get(2), captures.get(3)) {
                (Some(count), Some(from), Some(to)) => {
                    match (
                        count.as_str().parse(),
                        from.as_str().parse(),
                        to.as_str().parse(),
                    ) {
                        (Ok(count), Ok(from), Ok(to)) => Ok(Move { count, from, to }),
                        (Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) => {
                            Err(Fail(e.to_string()))
                        }
                    }
                }
                _ => Err(invalid("not enough fields matched")),
            },
        }
    }
}

#[test]
fn test_parse() {
    let p = MoveParser::new().unwrap();
    assert_eq!(
        p.parse("move 1 from 2 to 3").expect("valid move"),
        Move {
            id: 1,
            from: 2,
            to: 3
        }
    );
}

fn parse_moves(text: &str) -> Result<Vec<Move>, Fail> {
    let parser = MoveParser::new()?;
    text.split_terminator('\n')
        .map(|line| parser.parse(line))
        .collect()
}

//                 [B] [L]     [J]
//             [B] [Q] [R]     [D] [T]
//             [G] [H] [H] [M] [N] [F]
//         [J] [N] [D] [F] [J] [H] [B]
//     [Q] [F] [W] [S] [V] [N] [F] [N]
// [W] [N] [H] [M] [L] [B] [R] [T] [Q]
// [L] [T] [C] [R] [R] [J] [W] [Z] [L]
// [S] [J] [S] [T] [T] [M] [D] [B] [H]
//  1   2   3   4   5   6   7   8   9

#[derive(Debug)]
struct State {
    stacks: Vec<Vec<char>>,
}

fn take_several_items(v: &mut Vec<char>, n: usize) -> Result<Vec<char>, Fail> {
    let mut result: Vec<char> = Vec::with_capacity(n);
    while result.len() < n {
        match v.pop() {
            Some(ch) => {
                result.push(ch);
            }
            None => {
                return Err(Fail("source stack is too short".to_string()));
            }
        }
    }
    Ok(result)
}

impl State {
    fn top_crates(&self) -> String {
        self.stacks
            .iter()
            .map(|st| st.last().copied().unwrap_or(' '))
            .collect()
    }

    fn apply_move_sequence(&mut self, moves: &[Move]) -> Result<(), Fail> {
        for this_move in moves.iter() {
            self.apply_move(this_move)?;
        }
        Ok(())
    }

    fn apply_move(&mut self, m: &Move) -> Result<(), Fail> {
        let items = match self.stacks.get_mut(m.from - 1) {
            Some(mut source) => take_several_items(&mut source, m.count)?,
            None => {
                return Err(Fail(format!(
                    "source stack {} does not exist in {:?}",
                    m.from, &self
                )));
            }
        };
        match self.stacks.get_mut(m.to - 1) {
            Some(dest) => {
                dest.extend(items);
                Ok(())
            }
            None => {
                return Err(Fail(format!(
                    "dest stack {} does not exist in {:?}",
                    m.to, &self
                )));
            }
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
                stacks.resize_with(column + 1, || vec![]);
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
        Some((state, moves)) => {
            let state = parse_initial_state(state)?;
            let moves = parse_moves(moves)?;
            Ok((state, moves))
        }
        None => Err(Fail("there should be a blank line".to_string())),
    }
}

fn main() {
    let (mut state, moves) =
        parse_input(str::from_utf8(include_bytes!("input.txt")).expect("valid encoding"))
            .expect("valid input file");
    eprintln!("{}", &state);
    state.apply_move_sequence(&moves).expect("valid moves");
    println!("Day 05 part 1: {}", state.top_crates());
}
