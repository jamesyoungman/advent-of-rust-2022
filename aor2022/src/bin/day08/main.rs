use std::str;

use lib::error::Fail;
use lib::grid::{CompassDirection, Position, ALL_MOVE_OPTIONS};

struct Trees {
    heights: Vec<Vec<i8>>,
    width: i64,
}

impl TryFrom<&str> for Trees {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Trees, Fail> {
        fn convert_row(s: &str) -> Result<Vec<i8>, Fail> {
            s.chars()
                .map(|ch| match ch.to_digit(10) {
                    Some(n) => {
                        if n <= 9 {
                            Ok(n as i8)
                        } else {
                            Err(Fail(format!("{n} is not a decimal digit")))
                        }
                    }
                    None => Err(Fail(format!("{ch} is not a digit"))),
                })
                .collect()
        }

        let grid: Vec<Vec<i8>> = match s.split_terminator('\n').map(convert_row).collect() {
            Ok(g) => g,
            Err(e) => {
                return Err(e);
            }
        };
        let width: i64 = match grid.get(0) {
            Some(first) => first.len() as i64,
            None => 0,
        };
        if grid.iter().all(|row| row.len() as i64 == width) {
            Ok(Trees {
                heights: grid,
                width,
            })
        } else {
            Err(Fail("some row has a different width".to_string()))
        }
    }
}

impl Trees {
    fn height(&self) -> i64 {
        self.heights.len() as i64
    }

    fn width(&self) -> i64 {
        self.width
    }

    fn get(&self, getpos: &Position) -> i8 {
        //dbg!(&getpos);
        self.heights[getpos.y as usize][getpos.x as usize]
    }

    fn highest(&self, mut pos: Position, direction: &CompassDirection, count: usize) -> i8 {
        let mut highest = -1;
        //dbg!(&pos);
        //dbg!(direction);
        //dbg!(&count);
        for _ in 0..count {
            let this_tree_height = self.get(&pos);
            if this_tree_height > highest {
                highest = this_tree_height;
            }
            pos = pos.move_direction(direction);
        }
        highest
    }

    fn is_visible_from_direction(&self, pos: &Position, fromdir: &CompassDirection) -> bool {
        let start = match fromdir {
            CompassDirection::North => Position { x: pos.x, y: 0 },
            CompassDirection::South => Position {
                x: pos.x,
                y: self.height() - 1,
            },
            CompassDirection::East => Position {
                x: self.width() - 1,
                y: pos.y,
            },
            CompassDirection::West => Position { x: 0, y: pos.y },
        };
        let count: usize = match fromdir {
            CompassDirection::North => pos.y as usize,
            CompassDirection::South => (self.height() - 1 - pos.y) as usize,
            CompassDirection::East => (self.width() - 1 - pos.x) as usize,
            CompassDirection::West => pos.x as usize,
        };
        //dbg!(pos);
        //dbg!(fromdir);
        //dbg!(start);
        self.highest(start, &fromdir.reversed(), count) < self.get(&pos)
    }

    fn is_visible_from_edge(&self, pos: &Position) -> bool {
        ALL_MOVE_OPTIONS
            .iter()
            .any(|direction| self.is_visible_from_direction(pos, &direction))
    }
}

fn count_visible_trees(s: &str) -> usize {
    let trees = Trees::try_from(s).expect("input should be valid");
    let mut count = 0;
    for x in 0..trees.width() {
        for y in 0..trees.height() {
            if trees.is_visible_from_edge(&Position { x, y }) {
                count += 1;
            }
        }
    }
    count
}

#[test]
fn test_count_visible_trees() {
    const EXAMPLE: &str = concat!("30373\n", "25512\n", "65332\n", "33549\n", "35390\n",);
    assert_eq!(count_visible_trees(EXAMPLE), 21);
}

fn main() {
    let text = str::from_utf8(include_bytes!("input.txt")).expect("valid input file");
    println!("Day 08 part 1: {}", count_visible_trees(text));
}
