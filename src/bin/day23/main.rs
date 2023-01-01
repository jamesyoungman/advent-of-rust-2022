use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Formatter, Write},
    str,
};

use lib::error::Fail;
use lib::grid::{bounds, CompassDirection, Position};

struct Grove {
    occupied: HashSet<Position>,
    direction_order: [CompassDirection; 4],
}

impl Display for Grove {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match bounds(self.occupied.iter()) {
            None => Ok(()), // empty, nothing to display.
            Some(bounds) => self.render(&bounds, |ch| f.write_char(ch)),
        }
    }
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
    let result = match direction {
        CompassDirection::North => {
            let y = pos.y - 1;
            [
                Position { x: pos.x - 1, y }, // NW
                Position { x: pos.x, y },     // N
                Position { x: pos.x + 1, y },
            ]
        } // NW
        CompassDirection::East => {
            let x = pos.x + 1;
            [
                Position { x, y: pos.y - 1 }, // NE
                Position { x, y: pos.y },     // E
                Position { x, y: pos.y + 1 },
            ]
        } // SE
        CompassDirection::South => {
            let y = pos.y + 1;
            [
                Position { x: pos.x - 1, y }, // SE
                Position { x: pos.x, y },     // S
                Position { x: pos.x + 1, y },
            ]
        } // SW
        CompassDirection::West => {
            let x = pos.x - 1;
            [
                Position { x, y: pos.y - 1 }, // NW
                Position { x, y: pos.y },     // W
                Position { x, y: pos.y + 1 },
            ]
        } // SW
    };
    let dx: Vec<i64> = result.iter().map(|p| p.x - pos.x).collect();
    let dy: Vec<i64> = result.iter().map(|p| p.y - pos.y).collect();
    if dx[0] == dx[1] && dx[0] == dx[2] {
        if dx[0] == -1 {
            assert_eq!(direction, &CompassDirection::West);
        } else {
            assert_eq!(dx[0], 1);
            assert_eq!(direction, &CompassDirection::East);
        }
        assert_eq!(dy[0], -1);
        assert_eq!(dy[1], 0);
        assert_eq!(dy[2], 1);
    } else if dy[0] == dy[1] && dy[0] == dy[2] {
        if dy[0] == -1 {
            assert_eq!(direction, &CompassDirection::North);
        } else {
            assert_eq!(dy[0], 1);
            assert_eq!(direction, &CompassDirection::South);
        }
        assert_eq!(dx[0], -1);
        assert_eq!(dx[1], 0);
        assert_eq!(dx[2], 1);
    } else {
        panic!("direction_qualifying_positions({pos:?}, {direction:?}): wrong result {result:?}");
    }
    result
}

fn cardinal_direction_is_crowded(
    occupied: &HashSet<Position>,
    direction: &CompassDirection,
    pos: &Position,
) -> bool {
    direction_qualifying_positions(pos, direction)
        .iter()
        .any(|qualifier| occupied.contains(qualifier))
}

fn make_proposal(
    occupied: &HashSet<Position>,
    direction_order: &[CompassDirection],
    pos: &Position,
) -> Option<CompassDirection> {
    println!("for the elf at position {pos} the direction order is {direction_order:?}");

    for (i, direction) in direction_order.iter().enumerate() {
        if cardinal_direction_is_crowded(occupied, direction, pos) {
            println!("elf at {pos} cannot move {direction:?} because the space will be occupied");
        } else {
            println!("elf at {pos} proposes to move {direction:?} (preference {i})");
            return Some(*direction);
        }
    }
    println!("elf at {pos} seems blocked in every direction");
    None
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
    #[cfg(test)]
    fn render_as_string(&self, bounds: &(Position, Position)) -> String {
        let (top_left, bottom_right) = bounds;
        let mut result = String::with_capacity(
            usize::try_from(bottom_right.y - top_left.y).unwrap_or(0)
                * usize::try_from(bottom_right.x - top_left.x + 1).unwrap_or(0),
        );
        let emit = |ch| -> Result<(), ()> {
            result.push(ch);
            Ok(())
        };
        self.render(bounds, emit).expect("emit cannot fail");
        result
    }

    fn render<F, E>(&self, bounds: &(Position, Position), mut output: F) -> Result<(), E>
    where
        F: FnMut(char) -> Result<(), E>,
    {
        let (top_left, bottom_right) = bounds;
        for y in (top_left.y)..=(bottom_right.y) {
            for x in (top_left.x)..=(bottom_right.x) {
                output(if self.occupied.contains(&Position { x, y }) {
                    '#'
                } else {
                    '.'
                })?;
            }
            output('\n')?;
        }
        Ok(())
    }
    fn make_proposals(&self) -> HashMap<Position, Option<CompassDirection>> {
        self.occupied
            .iter()
            .filter(|pos| {
                if self.has_neighbour(pos) {
                    println!("elf at {pos} has a neighbour, so needs to move");
                    true
                } else {
                    println!("elf at {pos} has no neighbour, so will stay put");
                    false
                }
            })
            .map(|pos| {
                (
                    *pos,
                    make_proposal(&self.occupied, &self.direction_order, pos),
                )
            })
            .collect()
    }

    fn has_neighbour(&self, here: &Position) -> bool {
        for dy in [-1, 0, 1] {
            for dx in [-1, 0, 1] {
                let there = Position {
                    x: here.x + dx,
                    y: here.y + dy,
                };
                if &there != here && self.occupied.contains(&there) {
                    return true;
                }
            }
        }
        false
    }

    fn rotate_direction_order(&mut self) {
        println!(
            "Before rotation, old direction order is {:?}",
            &self.direction_order,
        );
        self.direction_order.rotate_left(1);
        println!(
            "After rotation, new direction order is {:?}",
            &self.direction_order,
        );
    }

    fn iterate(&mut self) -> bool {
        let proposals: HashMap<Position, Option<CompassDirection>> = self.make_proposals();
        let proposed_occupancies: HashMap<Position, usize> = count_proposed_occupancies(&proposals);
        let mut anybody_moved = false;
        for (current_pos, direction) in proposals
            .into_iter()
            .filter_map(|(pos, maybe_direction)| maybe_direction.map(|dir| (pos, dir)))
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

#[cfg(test)]
fn small_example() -> &'static str {
    concat!(
        ".....\n",
        "..##.\n",
        "..#..\n",
        ".....\n",
        "..##.\n",
        ".....\n",
    )
}

#[cfg(test)]
fn large_example() -> &'static str {
    concat!(
        "..............\n",
        "..............\n",
        ".......#......\n",
        ".....###.#....\n",
        "...#...#.#....\n",
        "....#...##....\n",
        "...#.###......\n",
        "...##.#.##....\n",
        "....#..#......\n",
        "..............\n",
        "..............\n",
        "..............\n",
    )
}

#[cfg(test)]
fn check_iteration_stage_rendering(
    mut grove: Grove,
    expected: &[String],
    bounds: &(Position, Position),
) {
    for (iteration, expected_image) in expected.iter().enumerate() {
        let got = grove.render_as_string(bounds);
        println!(
            "For iteration {iteration} the preferred move order is {:?}",
            &grove.direction_order
        );
        println!("At start of iteration {iteration}:\n{got}");
        if &got != expected_image {
            panic!("check_iteration_stage_rendering: mismatch at iteration {iteration}.  Expected:\n{expected_image}\n\nGot:\n{got}");
        }
        println!("Moving elves for iteration {iteration}...");
        grove.iterate();
        println!(
            "At end of iteration {iteration}:\n{}",
            grove.render_as_string(bounds)
        );
    }
}

#[test]
fn test_small_example() {
    let grove = Grove::try_from(small_example()).expect("valid small example");
    let expected: Vec<String> = vec![
        [
            ".....",
            "..##.",
            "..#..",
            ".....",
            "..##.",
            ".....\n",
        ]
        .join("\n"),
        [
            "..##.",
            ".....",
            "..#..",
            "...#.",
            "..#..",
            ".....\n",
        ]
        .join("\n"),
        [
            ".....",
            "..##.",
            ".#...",
            "....#",
            ".....",
            "..#..\n",
        ]
        .join("\n"),
        [
            "..#..",
            "....#",
            "#....",
            "....#",
            ".....",
            "..#..\n",
        ]
        .join("\n"),
    ];
    let bounds: &(Position, Position) = &(Position { x: 0, y: 0 }, Position { x: 4, y: 5 });
    check_iteration_stage_rendering(grove, &expected, bounds);
}

#[test]
fn test_large_example() {
    let grove = Grove::try_from(large_example()).expect("valid large example");
    let expected: Vec<String> = vec![
        [
            "..............",
            "..............",
            ".......#......",
            ".....###.#....",
            "...#...#.#....",
            "....#...##....",
            "...#.###......",
            "...##.#.##....",
            "....#..#......",
            "..............",
            "..............",
            "..............\n",
        ]
        .join("\n"),
        [
            "..............",
            ".......#......",
            ".....#...#....",
            "...#..#.#.....",
            ".......#..#...",
            "....#.#.##....",
            "..#..#.#......",
            "..#.#.#.##....",
            "..............",
            "....#..#......",
            "..............",
            "..............\n",
        ]
        .join("\n"),
        [
            "..............",
            ".......#......",
            "....#.....#...",
            "...#..#.#.....",
            ".......#...#..",
            "...#..#.#.....",
            ".#...#.#.#....",
            "..............",
            "..#.#.#.##....",
            "....#..#......",
            "..............",
            "..............\n",
        ]
        .join("\n"),
        [
            "..............",
            ".......#......",
            ".....#....#...",
            "..#..#...#....",
            ".......#...#..",
            "...#..#.#.....",
            ".#..#.....#...",
            ".......##.....",
            "..##.#....#...",
            "...#..........",
            ".......#......",
            "..............\n",
        ]
        .join("\n"),
        [
            "..............",
            ".......#......",
            "......#....#..",
            "..#...##......",
            "...#.....#.#..",
            ".........#....",
            ".#...###..#...",
            "..#......#....",
            "....##....#...",
            "....#.........",
            ".......#......",
            "..............\n",
        ]
        .join("\n"),
        [
            // Round 5
            ".......#......",
            "..............",
            "..#..#.....#..",
            ".........#....",
            "......##...#..",
            ".#.#.####.....",
            "...........#..",
            "....##..#.....",
            "..#...........",
            "..........#...",
            "....#..#......",
            "..............\n",
        ]
        .join("\n"),
    ];
    let bounds: &(Position, Position) = &(Position { x: 0, y: 0 }, Position { x: 13, y: 11 });
    check_iteration_stage_rendering(grove, &expected, bounds);
}

fn solve_part1(s: &str) -> (usize, Grove) {
    let mut grove = Grove::try_from(s).expect("valid input");
    for _round in 0..10 {
        if !grove.iterate() {
            println!("stopping early!");
            break;
        }
    }
    (grove.area_occupied_by_elves(), grove)
}

#[test]
fn test_solve_part1() {
    let (area, grove) = solve_part1(large_example());
    let s = grove.to_string();
    assert_eq!(
        s,
        concat!(
            "......#.....\n",
            "..........#.\n",
            ".#.#..#.....\n",
            ".....#......\n",
            "..#.....#..#\n",
            "#......##...\n",
            "....##......\n",
            ".#........#.\n",
            "...#.#..#...\n",
            "............\n",
            "...#..#..#..\n"
        )
    );
    assert_eq!(110, area);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    // 6399 is wrong.
    let (area, _) = solve_part1(input);
    println!("Day 23 part 1: {area}");
}
