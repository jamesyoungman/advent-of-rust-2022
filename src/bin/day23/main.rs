use std::{
    collections::{BTreeSet, HashMap},
    fmt::{Display, Formatter, Write},
    str,
};

use lib::error::Fail;
use lib::grid::{bounds, BoundingBox, CompassDirection, Position};

struct Grove {
    occupied: BTreeSet<Position>,
    direction_order: [CompassDirection; 4],
}

impl Display for Grove {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match bounds(self.occupied.iter()) {
            None => Ok(()), // empty, nothing to display.
            Some(bounds) => self.render(Some(&bounds), |ch| f.write_char(ch)),
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
        let occupied: BTreeSet<Position> = s
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
                Position { x: pos.x + 1, y }, // NE
            ]
        }
        CompassDirection::East => {
            let x = pos.x + 1;
            [
                Position { x, y: pos.y - 1 }, // NE
                Position { x, y: pos.y },     // E
                Position { x, y: pos.y + 1 }, // SE
            ]
        }
        CompassDirection::South => {
            let y = pos.y + 1;
            [
                Position { x: pos.x - 1, y }, // SW
                Position { x: pos.x, y },     // S
                Position { x: pos.x + 1, y }, // SE
            ]
        }
        CompassDirection::West => {
            let x = pos.x - 1;
            [
                Position { x, y: pos.y - 1 }, // NW
                Position { x, y: pos.y },     // W
                Position { x, y: pos.y + 1 }, // SW
            ]
        }
    };
    // Sanity-check `result`.
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
    occupied: &BTreeSet<Position>,
    direction: &CompassDirection,
    pos: &Position,
) -> bool {
    direction_qualifying_positions(pos, direction)
        .iter()
        .any(|qualifier| occupied.contains(qualifier))
}

fn make_proposal(
    occupied: &BTreeSet<Position>,
    direction_order: &[CompassDirection],
    pos: &Position,
) -> Option<CompassDirection> {
    for direction in direction_order.iter() {
        if !cardinal_direction_is_crowded(occupied, direction, pos) {
            return Some(*direction);
        }
    }
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

#[cfg(test)]
fn estimate_rendered_size(maybe_bbox: Option<&BoundingBox>) -> usize {
    match maybe_bbox {
        Some(bbox) => {
            usize::try_from(bbox.bottom_right.y - bbox.top_left.y).unwrap_or(0)
                * usize::try_from(bbox.bottom_right.x - bbox.top_left.x + 1).unwrap_or(0)
        }
        None => 0,
    }
}

impl Grove {
    #[cfg(test)]
    fn render_as_string(&self, bounds: Option<&BoundingBox>) -> String {
        let mut result = String::with_capacity(estimate_rendered_size(bounds));
        let emit = |ch| -> Result<(), ()> {
            result.push(ch);
            Ok(())
        };
        self.render(bounds, emit).expect("emit cannot fail");
        result
    }

    fn render<F, E>(
        &self,
        specified_bounding_box: Option<&BoundingBox>,
        mut output: F,
    ) -> Result<(), E>
    where
        F: FnMut(char) -> Result<(), E>,
    {
        let maybe_computed_bounds = bounds(self.occupied.iter());
        let bbox = match (specified_bounding_box, maybe_computed_bounds.as_ref()) {
            (_, None) => {
                // There are no elves to render at all.
                return Ok(());
            }
            (Some(specified_bounds), Some(computed_bounds)) => {
                if computed_bounds.top_left.x < specified_bounds.top_left.x
                    || computed_bounds.top_left.y < specified_bounds.top_left.y
                    || computed_bounds.bottom_right.x > specified_bounds.bottom_right.x
                    || computed_bounds.bottom_right.y > specified_bounds.bottom_right.y
                {
                    println!("render: warning: actual bounds of elves are greater than the specified bounds (that is, some elves will not be shown; {computed_bounds:?} vs {specified_bounds:?})");
                }
                specified_bounds
            }
            (None, Some(computed_bounds)) => computed_bounds,
        };

        for y in (bbox.top_left.y)..=(bbox.bottom_right.y) {
            for x in (bbox.top_left.x)..=(bbox.bottom_right.x) {
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
            .filter_map(|pos| {
                if self.has_neighbour(pos) {
                    Some((
                        *pos,
                        make_proposal(&self.occupied, &self.direction_order, pos),
                    ))
                } else {
                    None
                }
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
        self.direction_order.rotate_left(1);
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
            Some(bounding_box) => {
                let width: usize = (1_i64 + bounding_box.bottom_right.x - bounding_box.top_left.x)
                    .try_into()
                    .expect("x bounds should not be reversed");
                let height: usize = (1_i64 + bounding_box.bottom_right.y - bounding_box.top_left.y)
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
fn first_differing_position(
    a: &str,
    b: &str,
    bounds: Option<&BoundingBox>,
) -> Option<(Position, char, char)> {
    let mut x: i64 = 0;
    let mut y: i64 = 0;
    let top_left: &Position = match bounds {
        Some(bbox) => &bbox.top_left,
        None => &Position { x: 0, y: 0 },
    };
    for (ach, bch) in a.chars().zip(b.chars()) {
        if ach != bch {
            return Some((
                Position {
                    x: top_left.x + x,
                    y: top_left.y + y,
                },
                ach,
                bch,
            ));
        }
        match ach {
            '\n' => {
                x = 0;
                y += 1;
            }
            _ => {
                x += 1;
            }
        }
    }
    None
}

#[cfg(test)]
fn check_iteration_stage_rendering(
    mut grove: Grove,
    expected: &[String],
    bounds: Option<&BoundingBox>,
) {
    for (iteration, expected_image) in expected.iter().enumerate() {
        let got = grove.render_as_string(bounds);
        if &got != expected_image {
            if let Some((diffpos, got_ch, expected_ch)) =
                first_differing_position(&got, expected_image, bounds)
            {
                panic!("check_iteration_stage_rendering: mismatch at iteration {iteration}.  Expected:\n{expected_image}\n\nGot:\n{got}\nPosition of difference is {diffpos}.  It contains '{got_ch}' while we expected '{expected_ch}'.");
            } else {
                panic!("got and expected string differ, but first_differing_position didn't locate the difference");
            }
        }
        grove.iterate();
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
    let bounds = BoundingBox {
        top_left: Position { x: 0, y: 0 },
        bottom_right: Position { x: 4, y: 5 },
    };
    check_iteration_stage_rendering(grove, &expected, Some(&bounds));
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
    let bounds = BoundingBox {
        top_left: Position { x: 0, y: 0 },
        bottom_right: Position { x: 13, y: 11 },
    };
    check_iteration_stage_rendering(grove, &expected, Some(&bounds));
}

fn solve_part1(s: &str) -> (usize, Grove) {
    let mut grove = Grove::try_from(s).expect("valid input");
    for _round in 0..10 {
        if !grove.iterate() {
            panic!("stopped early!");
        }
    }
    let total_area = grove.area_occupied_by_elves();
    let elf_count = grove.occupied.len();
    let empty_ground_tiles = total_area - elf_count;
    (empty_ground_tiles, grove)
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

fn solve_part2(s: &str) -> usize {
    let mut grove = Grove::try_from(s).expect("valid input");
    for round in 1.. {
        if !grove.iterate() {
            return round;
        }
    }
    unreachable!()
}

#[test]
fn test_sove_part2() {
    assert_eq!(solve_part2(large_example()), 20);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    let (area, _) = solve_part1(input);
    println!("Day 23 part 1: {area}");
    println!("Day 23 part 2: {}", solve_part2(input));
}
