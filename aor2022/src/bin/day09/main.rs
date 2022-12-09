use std::collections::HashSet;
use std::num::ParseIntError;
use std::str;

use lib::error::Fail;
use lib::grid::{CompassDirection, Position};

#[derive(Debug, Clone)]
struct Rope {
    knots: Vec<Position>,
}

fn close_enough(head_pos: &Position, tail_pos: &Position) -> bool {
    let hgap = head_pos.x - tail_pos.x;
    let vgap = head_pos.y - tail_pos.y;
    hgap.abs() <= 1 && vgap.abs() <= 1
}

fn maybe_move_tail(head_pos: &Position, tail_pos: Position) -> Position {
    if close_enough(head_pos, &tail_pos) {
        tail_pos
    } else {
        let result = Position {
            x: tail_pos.x + (head_pos.x - tail_pos.x).signum(),
            y: tail_pos.y + (head_pos.y - tail_pos.y).signum(),
        };
        // we want to re-establish this invariant
        assert!(close_enough(head_pos, &result));
        result
    }
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

    fn tail_position(&self) -> Option<Position> {
        self.knots.iter().rev().next().copied()
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
        // Visit each pair of knots (a,b), starting at the head end (a
        // is the knot nearest the head).  Update the position of b if
        // it is too far away from a.
        visit_pairs(&mut self.knots, |a, b| {
            *b = maybe_move_tail(a, *b);
        });
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

/// Perform a sequence of moves of the head end of the rope, and
/// return a vector containing the successive positions occupied by
/// the tail of the rope.
fn perform_sequence(start: Rope, moves: &[Move]) -> Vec<Position> {
    struct State {
        current: Rope,
        tail_history: Vec<Position>,
    }
    fn update(mut accumulator: State, thismove: &Move) -> State {
        for _ in 0..thismove.count {
            accumulator.current.perform_move(thismove.direction);
            accumulator
                .tail_history
                .extend(accumulator.current.tail_position());
        }
        accumulator
    }
    // The initial tail history is a Vec of size 0 (if there are zero
    // knots) or 1 (if there is at least one knot in the rope).
    let tail_history: Vec<Position> = start.tail_position().into_iter().collect();
    moves
        .iter()
        .fold(
            State {
                current: start,
                tail_history,
            },
            update,
        )
        .tail_history
}

fn parse_moves(s: &str) -> Result<Vec<Move>, Fail> {
    s.split_terminator('\n').map(Move::try_from).collect()
}

fn count_unique_tail_positions(moves: &[Move], len: usize) -> usize {
    // Count the unique positions by collecting the items into a hash
    // set and then returning the size of the set.
    perform_sequence(Rope::new(len), moves)
        .into_iter()
        .collect::<HashSet<Position>>()
        .len()
}

fn solve_part1(moves: &[Move]) -> usize {
    count_unique_tail_positions(moves, 2)
}

fn solve_part2(moves: &[Move]) -> usize {
    count_unique_tail_positions(moves, 10)
}

#[test]
fn test_count_unique_tail_positions() {
    const EXAMPLE: &str =
        concat!("R 4\n", "U 4\n", "L 3\n", "D 1\n", "R 4\n", "D 1\n", "L 5\n", "R 2\n",);
    let moves = parse_moves(EXAMPLE).expect("example should be valid");
    assert_eq!(count_unique_tail_positions(&moves, 2), 13);
}

fn main() {
    let text = str::from_utf8(include_bytes!("input.txt")).expect("valid input file");
    let moves: Vec<Move> = parse_moves(text).expect("valid moves");
    println!("Day 09 part 1: {}", solve_part1(&moves));
    println!("Day 09 part 2: {}", solve_part2(&moves));
}
