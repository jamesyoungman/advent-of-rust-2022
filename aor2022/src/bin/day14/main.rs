use std::{
    collections::HashSet,
    fmt::{Display, Formatter},
    str,
};

use lib::error::Fail;
use lib::grid::{bounds, update_max, update_min, Position};

#[derive(Debug)]
enum Fall {
    Down,
    DownLeft,
    DownRight,
}

#[derive(Debug)]
struct Cave {
    structures: HashSet<Position>,
    sand: HashSet<Position>,
    source: Position,
}

impl Cave {
    fn sand_move(&self, pos: &Position) -> (Option<Fall>, Position) {
        for possible_move in [Fall::Down, Fall::DownLeft, Fall::DownRight] {
            let next_pos = match possible_move {
                Fall::Down => Position {
                    x: pos.x,
                    y: pos.y + 1,
                },
                Fall::DownLeft => Position {
                    x: pos.x - 1,
                    y: pos.y + 1,
                },
                Fall::DownRight => Position {
                    x: pos.x + 1,
                    y: pos.y + 1,
                },
            };
            if !self.structures.contains(&next_pos) && !self.sand.contains(&next_pos) {
                return (Some(possible_move), next_pos);
            }
        }
        (None, *pos) // at rest
    }

    fn sand_stopped_at(&mut self, pos: Position) {
        self.sand.insert(pos);
    }

    fn display_char(&self, pos: &Position) -> char {
        if *pos == self.source {
            '+'
        } else if self.structures.contains(pos) {
            '#'
        } else if self.sand.contains(pos) {
            'o'
        } else {
            '.'
        }
    }

    fn bounds(&self) -> (Position, Position) {
        fn expand_bounds(
            minpos: &mut Position,
            maxpos: &mut Position,
            candidate_min: Position,
            candidate_max: Position,
        ) -> (Position, Position) {
            update_min(&mut minpos.x, candidate_min.x);
            update_min(&mut minpos.y, candidate_min.y);
            update_max(&mut maxpos.x, candidate_max.x);
            update_max(&mut maxpos.y, candidate_max.y);
            (*minpos, *maxpos)
        }

        let mut top_left = Position {
            x: self.source.x,
            y: self.source.y,
        };
        let mut bot_right = Position {
            x: self.source.x,
            y: self.source.y,
        };
        if let Some((structure_top_left, structure_bottom_right)) = bounds(self.structures.iter()) {
            expand_bounds(
                &mut top_left,
                &mut bot_right,
                structure_top_left,
                structure_bottom_right,
            );
        }
        if let Some((sand_top_left, sand_bottom_right)) = bounds(self.sand.iter()) {
            expand_bounds(
                &mut top_left,
                &mut bot_right,
                sand_top_left,
                sand_bottom_right,
            );
        }
        (top_left, bot_right)
    }
}

impl Display for Cave {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let (top_left, bottom_right) = self.bounds();
        for y in (top_left.y)..=(bottom_right.y) {
            for x in (top_left.x)..=(bottom_right.x) {
                write!(f, "{}", self.display_char(&Position { x, y }))?;
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

fn parse_point(point: &str) -> Result<Position, Fail> {
    match point.split_once(',') {
        Some((left, right)) => match (left.parse(), right.parse()) {
            (Ok(x), Ok(y)) => Ok(Position { x, y }),
            (Err(e), _) => Err(Fail(format!("{left} is not a valid number: {e}"))),
            (_, Err(e)) => Err(Fail(format!("{right} is not a valid number: {e}"))),
        },
        None => Err(Fail(format!(
            "{point} is not a valid position; it contains no comma"
        ))),
    }
}

fn parse_structure(line: &str) -> Result<HashSet<Position>, Fail> {
    let mut result = HashSet::new();
    let points: Vec<Position> = line
        .split(" -> ")
        .map(parse_point)
        .collect::<Result<Vec<Position>, Fail>>()?;
    for w in points.windows(2) {
        let from = w[0];
        let to = w[1];
        let xdirection = (to.x - from.x).signum();
        let ydirection = (to.y - from.y).signum();
        let mut p = from;
        while p != to {
            result.insert(p);
            p.x += xdirection;
            p.y += ydirection;
        }
        result.insert(p);
    }
    Ok(result)
}

impl TryFrom<&str> for Cave {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Cave, Fail> {
        let mut structures = HashSet::new();
        for line in s.split_terminator('\n') {
            let mut structure = parse_structure(line)?;
            structures.extend(structure.drain());
        }
        Ok(Cave {
            structures,
            sand: HashSet::new(),
            source: Position { x: 500, y: 0 },
        })
    }
}

fn solve1(cave: &mut Cave) -> usize {
    let lowest_y = match bounds(cave.structures.iter()) {
        Some((_, bottom_right)) => bottom_right.y,
        None => {
            // There are no structures in the cave.  So nothing to
            // come to rest on.
            return 0;
        }
    };
    for sand_units in 1.. {
        let mut pos = Position { x: 500, y: 0 };
        assert!(
            cave.sand_move(&pos).0.is_some(),
            "entry point should not itself be blocked",
        );
        loop {
            match cave.sand_move(&pos) {
                (None, _) => {
                    println!("Particle {sand_units} came to rest at at {pos}");
                    cave.sand_stopped_at(pos);
                    break;
                }
                (Some(_), newpos) => {
                    if newpos.y > lowest_y {
                        // nothing to stop this sand particle.
                        println!("Particle {sand_units} is in infinite-fall at {newpos}");
                        return sand_units - 1;
                    }
                    pos = newpos;
                }
            }
        }
    }
    unreachable!()
}

#[cfg(test)]
fn example() -> &'static str {
    concat!(
        "498,4 -> 498,6 -> 496,6\n",
        "503,4 -> 502,4 -> 502,9 -> 494,9\n",
    )
}

#[test]
fn test_day14_part1_example() {
    let mut cave = Cave::try_from(example()).expect("example should be valid");
    assert_eq!(solve1(&mut cave), 24);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    let mut cave = Cave::try_from(input).expect("example should be valid");
    let part1 = solve1(&mut cave);
    println!("{cave}\n");
    println!("Day 14 part 1: {part1}");
}
