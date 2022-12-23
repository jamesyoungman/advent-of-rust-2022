use std::{collections::HashMap, collections::HashSet, str};

use lib::error::Fail;
use lib::grid::{bounds, CompassDirection, Position};

struct Grove {
    occupied: HashSet<Position>,
    direction_order: [CompassDirection; 4],
}

fn build_line(y: i64, line: &str) -> impl Iterator<Item = Position> + '_ {
    line.chars().enumerate().filter_map(move |(x, ch)| {
        if ch == '#' {
            Some(Position { x: x as i64, y })
        } else {
            None
        }
    })
}

impl TryFrom<&str> for Grove {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Grove, Fail> {
        let occupied: HashSet<Position> = s
            .split_terminator('\n')
            .enumerate()
            .flat_map(|(y, line)| build_line(y as i64, line))
            .collect();
        Ok(Grove {
            occupied,
            direction_order: [
                CompassDirection::North,
                CompassDirection::South,
                CompassDirection::West,
                CompassDirection::East,
            ],
        })
    }
}

fn direction_qualifying_positions(pos: &Position, direction: &CompassDirection) -> [Position; 3] {
    match direction {
        CompassDirection::North => [
            Position {
                x: pos.x - 1,
                y: pos.y - 1,
            }, // NW
            Position {
                x: pos.x,
                y: pos.y - 1,
            }, // N
            Position {
                x: pos.x + 1,
                y: pos.y - 1,
            },
        ], // NW
        CompassDirection::East => [
            Position {
                x: pos.x + 1,
                y: pos.y - 1,
            }, // NE
            Position {
                x: pos.x + 1,
                y: pos.y,
            }, // E
            Position {
                x: pos.x + 1,
                y: pos.y + 1,
            },
        ], // SE
        CompassDirection::South => [
            Position {
                x: pos.x - 1,
                y: pos.y + 1,
            }, // SE
            Position {
                x: pos.x,
                y: pos.y + 1,
            }, // S
            Position {
                x: pos.x - 1,
                y: pos.y + 1,
            },
        ], // SW
        CompassDirection::West => [
            Position {
                x: pos.x - 1,
                y: pos.y - 1,
            }, // NW
            Position {
                x: pos.x - 1,
                y: pos.y,
            }, // W
            Position {
                x: pos.x - 1,
                y: pos.y + 1,
            },
        ], // SW
    }
}

fn cardinal_direction_is_crowded(
    occupied: &HashSet<Position>,
    direction: &CompassDirection,
    pos: &Position,
) -> bool {
    for qualifier in direction_qualifying_positions(pos, direction) {
        if occupied.contains(&qualifier) {
            return true;
        }
    }
    return false;
}

fn make_proposal(
    occupied: &HashSet<Position>,
    direction_order: &[CompassDirection],
    pos: &Position,
) -> Option<CompassDirection> {
    direction_order
        .iter()
        .filter(|direction| !cardinal_direction_is_crowded(occupied, direction, pos))
        .copied()
        .next()
}

fn count_proposed_occupancies(
    proposals: &HashMap<Position, Option<CompassDirection>>,
) -> HashMap<Position, usize> {
    let mut result: HashMap<Position, usize> = HashMap::new();
    for (pos, maybe_dir) in proposals.iter() {
        if let Some(dir) = maybe_dir {
            let next = pos.move_direction(dir);
            result
                .entry(next)
                .and_modify(|count| *count += 1)
                .or_insert(1);
        }
    }
    result
}

impl Grove {
    fn make_proposals(&self) -> HashMap<Position, Option<CompassDirection>> {
        self.occupied
            .iter()
            .map(|pos| {
                (
                    *pos,
                    make_proposal(&self.occupied, &self.direction_order, pos),
                )
            })
            .collect()
    }

    fn rotate_direction_order(&mut self) {
        self.direction_order.rotate_left(1);
    }

    fn iterate(&mut self) -> bool {
        let proposals: HashMap<Position, Option<CompassDirection>> = self.make_proposals();
        let proposed_occupancies: HashMap<Position, usize> = count_proposed_occupancies(&proposals);
        let mut anybody_moved = false;
        for (current_pos, direction) in
            proposals
                .into_iter()
                .filter_map(|(pos, maybe_direction)| match maybe_direction {
                    None => None,
                    Some(dir) => Some((pos, dir)),
                })
        {
            let new_pos = current_pos.move_direction(&direction);
            match proposed_occupancies.get(&new_pos) {
                None | Some(0) => {
                    panic!("proposed move {direction} from {current_pos} but proposed occupancy of {new_pos} is zero");
                }
                Some(1) => {
                    assert!(current_pos != new_pos);
                    self.occupied.remove(&current_pos);
                    self.occupied.insert(new_pos);
                    anybody_moved = true;
                }
                _ => (), // two elves block each other
            }
        }
        self.rotate_direction_order();
        anybody_moved
    }

    fn area_occupied_by_elves(&self) -> usize {
        match bounds(self.occupied.iter()) {
            Some((top_left, bot_right)) => {
                let width: usize = (bot_right.x - top_left.x)
                    .try_into()
                    .expect("x bounds should not be reversed");
                let height: usize = (bot_right.y - top_left.y)
                    .try_into()
                    .expect("y bounds should not be reversed");
                width * height
            }
            None => 0,
        }
    }
}

fn solve_part1(s: &str) -> usize {
    let mut grove = Grove::try_from(s).expect("valid input");
    for _ in 0..10 {
        if !grove.iterate() {
            break;
        }
    }
    grove.area_occupied_by_elves()
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    println!("Day 23 part 1: {}", solve_part1(input));
}
