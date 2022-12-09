use std::collections::HashSet;
use std::num::ParseIntError;
use std::str;

use lib::error::Fail;
use lib::grid::{CompassDirection, Position};

const MAX_H_GAP: i64 = 1;
const MAX_V_GAP: i64 = 1;

#[derive(Debug, Clone)]
struct Rope {
    knots: Vec<Position>,
}

fn close_enough(head_pos: &Position, tail_pos: &Position) -> bool {
    let hgap = head_pos.x - tail_pos.x;
    let vgap = head_pos.y - tail_pos.y;
    if hgap.abs() <= MAX_H_GAP {
        if vgap.abs() <= MAX_V_GAP {
            true
        } else {
            false
        }
    } else {
        false
    }
}

fn maybe_move_tail(head_pos: Position, tail_pos: Position) -> Position {
    if close_enough(&head_pos, &tail_pos) {
        tail_pos
    } else {
        let result = Position {
            x: tail_pos.x + (head_pos.x - tail_pos.x).signum(),
            y: tail_pos.y + (head_pos.y - tail_pos.y).signum(),
        };
        // we want to re-establish this invariant
        assert!(close_enough(&head_pos, &result));
        result
    }
}

fn update_knot_position(head: &Position, tail: &mut Position) {
    *tail = maybe_move_tail(*head, *tail);
}

fn visit_pairs<T>(v: &mut [T], mut f: impl FnMut(&T, &mut T)) {
    let mut prev: Option<&T> = None;
    for item in v.iter_mut() {
        if let Some(previous) = prev {
            f(previous, item);
        }
        prev = Some(&*item);
    }
}

impl Rope {
    fn new(len: usize) -> Rope {
        Rope {
            knots: vec![Position { x: 0, y: 0 }; len],
        }
    }

    fn all_close_enough(&self) -> bool {
        self.knots
            .iter()
            .fold((true, None), |(good, prev), knot| match prev {
                Some(previous_knot) => (good && close_enough(previous_knot, knot), Some(knot)),
                None => (good, Some(knot)),
            })
            .0
    }

    /// Move the head of the rope and, if necessary, the tail.
    fn perform_move(&mut self, head_direction: CompassDirection) {
        assert!(self.all_close_enough()); // invariant
        self.knots[0] = self.knots[0].move_direction(&head_direction);
        visit_pairs(&mut self.knots, update_knot_position);
        assert!(self.all_close_enough()); // invariant
    }
}

#[derive(Debug, Clone)]
struct Move {
    direction: CompassDirection,
    count: usize,
}

impl TryFrom<&str> for Move {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Move, Fail> {
        fn convert_count(s: &str) -> Result<usize, Fail> {
            s.parse().map_err(|e: ParseIntError| Fail(e.to_string()))
        }

        match s.split_at(2) {
            ("U ", count) => Ok(Move {
                direction: CompassDirection::North,
                count: convert_count(count)?,
            }),
            ("D ", count) => Ok(Move {
                direction: CompassDirection::South,
                count: convert_count(count)?,
            }),
            ("L ", count) => Ok(Move {
                direction: CompassDirection::West,
                count: convert_count(count)?,
            }),

            ("R ", count) => Ok(Move {
                direction: CompassDirection::East,
                count: convert_count(count)?,
            }),
            (other, _) => Err(Fail(format!("invalid direction {other}"))),
        }
    }
}

fn perform_sequence<I: IntoIterator<Item = Move>>(start: Rope, moves: I) -> Vec<Rope> {
    struct State {
        current: Rope,
        history: Vec<Rope>,
    }
    fn update(now: State, thismove: Move) -> State {
        match now {
            State {
                mut current,
                mut history,
            } => {
                for _ in 0..thismove.count {
                    current.perform_move(thismove.direction);
                    history.push(current.clone());
                }
                State { current, history }
            }
        }
    }
    moves
        .into_iter()
        .fold(
            State {
                current: start.clone(),
                history: vec![start],
            },
            update,
        )
        .history
}

fn parse_moves(s: &str) -> Result<Vec<Move>, Fail> {
    s.split_terminator('\n').map(Move::try_from).collect()
}

fn count_unique_tail_positions(moves: Vec<Move>, len: usize) -> usize {
    let history = perform_sequence(Rope::new(len), moves);
    let positions: HashSet<Position> = history
        .into_iter()
        .map(|mut rope| rope.knots.pop())
        .flatten()
        .collect();
    positions.len()
}

fn solve_part1(s: &str) -> Result<usize, Fail> {
    Ok(count_unique_tail_positions(parse_moves(s)?, 2))
}

fn solve_part2(s: &str) -> Result<usize, Fail> {
    Ok(count_unique_tail_positions(parse_moves(s)?, 10))
}

#[test]
fn test_count_unique_tail_positions() {
    const EXAMPLE: &str =
        concat!("R 4\n", "U 4\n", "L 3\n", "D 1\n", "R 4\n", "D 1\n", "L 5\n", "R 2\n",);
    let moves = parse_moves(EXAMPLE).expect("example should be valid");
    assert_eq!(count_unique_tail_positions(moves, 2), 13);
}

fn main() {
    let text = str::from_utf8(include_bytes!("input.txt")).expect("valid input file");
    println!(
        "Day 09 part 1: {}",
        solve_part1(text).expect("should be able to solve part 1")
    );
    println!(
        "Day 09 part 1: {}",
        solve_part2(text).expect("should be able to solve part 2")
    );
}
