use std::cmp::{max, min};
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};
use std::str;

use lib::error::Fail;

const TOWER_WIDTH: usize = 7;
const SHAPE_HEIGHT: usize = 4;
const SHAPE_WIDTH: usize = 4;

struct Shape {
    columns: [[bool; SHAPE_HEIGHT]; SHAPE_WIDTH],
}

impl Shape {
    #[cfg(test)]
    fn popcount(&self) -> usize {
        self.columns
            .iter()
            .map(|c| c.iter().filter(|&v| *v).count())
            .sum()
    }

    fn column_occupied(shape: &Shape, col: usize) -> bool {
        let occupation = &shape.columns[col];
        occupation.iter().any(|occupied| *occupied)
    }

    fn occupied(&self, row: usize, col: usize) -> bool {
        self.columns[col][row]
    }

    fn width(&self) -> usize {
        (0..SHAPE_WIDTH)
            .rev()
            .find_map(|column| {
                if Shape::column_occupied(self, column) {
                    Some(column + 1)
                } else {
                    None
                }
            })
            .unwrap_or(0)
    }
}

impl Display for Shape {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        for row in (0..SHAPE_HEIGHT).rev() {
            for col in 0..SHAPE_WIDTH {
                let ch: char = if self.columns[col][row] { '#' } else { '.' };
                write!(f, "{}", ch)?;
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl Debug for Shape {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self)
    }
}

#[derive(Debug)]
struct ShapeGenerator {
    i: usize,
}
const SHAPE_SEQUENCE: [&'static Shape; 5] = [&MINUS, &PLUS, &ANGLE, &PIPE, &SQUARE];

impl ShapeGenerator {
    fn new() -> ShapeGenerator {
        ShapeGenerator { i: 0 }
    }

    fn next_shape(&mut self) -> &'static Shape {
        let current = self.i;
        self.i = (self.i + 1) % 5;
        SHAPE_SEQUENCE[current]
    }
}

const MINUS: Shape = Shape {
    columns: [
        [true, false, false, false],
        [true, false, false, false],
        [true, false, false, false],
        [true, false, false, false],
    ],
};

#[test]
fn test_shape_popcount() {
    for (shape, expected_count) in [(MINUS, 4), (PLUS, 5), (ANGLE, 5), (PIPE, 4), (SQUARE, 4)] {
        let got = shape.popcount();
        assert_eq!(got, expected_count);
    }
}

#[test]
fn test_width_minus() {
    assert_eq!(MINUS.width(), 4);
}

const PLUS: Shape = Shape {
    columns: [
        [false, true, false, false],
        [true, true, true, false],
        [false, true, false, false],
        [false, false, false, false],
    ],
};

#[test]
fn test_width_plus() {
    assert!(Shape::column_occupied(&PLUS, 0));
    assert_eq!(PLUS.width(), 3);
}

const ANGLE: Shape = Shape {
    columns: [
        [true, false, false, false],
        [true, false, false, false],
        [true, true, true, false],
        [false, false, false, false],
    ],
};

#[test]
fn test_width_angle() {
    assert!(Shape::column_occupied(&ANGLE, 0));
    assert_eq!(ANGLE.width(), 3);
}

const PIPE: Shape = Shape {
    columns: [
        [true, true, true, true],
        [false, false, false, false],
        [false, false, false, false],
        [false, false, false, false],
    ],
};

#[test]
fn test_width_pipe() {
    assert!(Shape::column_occupied(&PIPE, 0));
    assert_eq!(PIPE.width(), 1);
}

const SQUARE: Shape = Shape {
    columns: [
        [true, true, false, false],
        [true, true, false, false],
        [false, false, false, false],
        [false, false, false, false],
    ],
};

#[test]
fn test_width_square() {
    assert!(Shape::column_occupied(&SQUARE, 0));
    assert_eq!(SQUARE.width(), 2);
}

#[derive(Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
struct RowCol {
    r: usize,
    c: usize,
}

#[derive(Debug, Default)]
struct Tower {
    height: usize,
    occupied: BTreeMap<usize, u8>,
}

#[derive(Debug, Copy, Clone)]
enum Direction {
    Left,
    Right,
}

impl Display for Direction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(match self {
            Direction::Left => "left",
            Direction::Right => "right",
        })
    }
}

impl Tower {
    fn new() -> Tower {
        Tower::default()
    }

    fn high_point(&self) -> usize {
        let fast = self.height;
        let slow = self
            .occupied
            .iter()
            .rev()
            .filter_map(|(r, cells)| if cells != 0 { Some(r) } else { None })
            .next()
            .unwrap_or(0);
        assert_eq!(fast, slow);
        fast
    }

    //fn rows_are_identical(&self, r1: usize, r2: usize) {
    //	fn range_for_row(r: usize) -> Range<RowCol> {
    //	    (Included(RowCol{r: r, c:0}), Excluded(RowCol{r: r+1, c:0}))
    //	}
    //	let range1 = self.occupied.range(range_for_row(r1));
    //	let range2 = self.occupied.range(range_for_row(r2));
    //	self.occupied.range(range1).zip(self.occupied(range2))
    //	    .all(|(v1, v2) v1 == v2)
    //}

    fn cycle_exists_at(&self, from_row: usize, to_row: usize) -> bool {
        assert!(from_row < to_row);
        todo!()
    }

    fn find_cycle(&self) -> Option<(usize, usize)> {
        todo!()
    }
}

impl Tower {
    fn max_height(&self) -> usize {
        self.height
    }

    fn collision(&self, shape: &Shape, shape_height: usize, shape_pos: usize) -> bool {
        for c in 0..SHAPE_WIDTH {
            for r in 0..SHAPE_HEIGHT {
                if shape.occupied(r, c) {
                    let rc = RowCol {
                        r: r + shape_height,
                        c: c + shape_pos,
                    };
                    if rc.c >= TOWER_WIDTH || rc.r == 0 || self.occupied.contains(&rc) {
                        return true;
                    }
                }
            }
        }
        false
    }

    fn update_shape_pos(
        &self,
        jet: &Direction,
        shape: &Shape,
        shape_pos: usize,
        shape_height: usize,
        shape_width: usize,
    ) -> Option<usize> {
        let newpos = match jet {
            Direction::Left => {
                if shape_pos > 0 {
                    shape_pos - 1
                } else {
                    return None;
                }
            }
            Direction::Right => {
                let rightmost = shape_pos + shape_width;
                if rightmost < TOWER_WIDTH {
                    shape_pos + 1
                } else {
                    return None;
                }
            }
        };
        if self.collision(shape, shape_height, newpos) {
            None
        } else {
            Some(newpos)
        }
    }

    fn update_occupation_for_landing(
        &mut self,
        shape: &Shape,
        shape_pos: usize,
        shape_height: usize,
        shape_width: usize,
    ) {
        for shape_col in 0..shape_width {
            let column = &shape.columns[shape_col];
            for shape_row in 0..SHAPE_HEIGHT {
                if column[shape_row] {
                    let rc = RowCol {
                        r: shape_row + shape_height,
                        c: shape_col + shape_pos,
                    };
                    self.height = max(self.height, rc.r);
                    self.occupied.insert(rc);
                }
            }
        }
    }

    fn tick(
        &mut self,
        verbosity: &Verbosity,
        shape: &Shape,
        shape_width: usize,
        shape_height: &mut usize,
        shape_pos: &mut usize,
        jet: &Direction,
    ) -> bool {
        let caveat: &str =
            match self.update_shape_pos(jet, shape, *shape_pos, *shape_height, shape_width) {
                None => " but nothing happens",
                Some(newpos) => {
                    *shape_pos = newpos;
                    ""
                }
            };
        if verbosity.meets(&Verbosity::High) {
            println!(
                "Jet of gas pushes rock {jet}{caveat}:\n{}",
                draw(&self, Some(&shape), *shape_height, *shape_pos)
            );
        }
        let height_after_fall = *shape_height - 1;
        if self.collision(shape, height_after_fall, *shape_pos) {
            self.update_occupation_for_landing(shape, *shape_pos, *shape_height, shape_width);
            if verbosity.meets(&Verbosity::High) {
                println!(
                    "Rock comes to rest:\n{}",
                    draw(&self, None, *shape_height, *shape_pos)
                );
            }
            true
        } else {
            *shape_height = height_after_fall;
            if verbosity.meets(&Verbosity::High) {
                println!(
                    "Rock falls one unit:\n{}",
                    draw(&self, Some(shape), *shape_height, *shape_pos)
                );
            }
            false
        }
    }
}

#[cfg(test)]
fn build_tower(rcs: &[(usize, usize)]) -> Tower {
    let occupied: BTreeSet<RowCol> = rcs
        .iter()
        .copied()
        .map(|(row, col)| RowCol { r: row, c: col })
        .collect();
    Tower {
        height: occupied.iter().map(|rc| rc.r).max().unwrap_or(0),
        occupied,
    }
}

#[test]
fn test_tower_update_shape_pos() {
    let t = build_tower(&[(0, 2), (0, 5), (1, 2)]);
    assert_eq!(t.update_shape_pos(&Direction::Left, &MINUS, 0, 7, 4), None);
    assert_eq!(
        t.update_shape_pos(&Direction::Left, &MINUS, 1, 7, 4),
        Some(0)
    );
    assert_eq!(
        t.update_shape_pos(&Direction::Left, &MINUS, 2, 7, 4),
        Some(1)
    );

    assert_eq!(
        t.update_shape_pos(&Direction::Right, &MINUS, 0, 7, 4),
        Some(1)
    );
    assert_eq!(
        t.update_shape_pos(&Direction::Right, &MINUS, 1, 7, 4),
        Some(2)
    );
    assert_eq!(
        t.update_shape_pos(&Direction::Right, &MINUS, 2, 7, 4),
        Some(3)
    );
    assert_eq!(t.update_shape_pos(&Direction::Right, &MINUS, 3, 7, 4), None);
}

#[test]
fn test_tower_highpoint() {
    assert_eq!(build_tower(&[(1, 2)]).high_point(), 1);
    assert_eq!(build_tower(&[(2, 1)]).high_point(), 2);
    assert_eq!(build_tower(&[(0, 2), (0, 5), (1, 2),]).high_point(), 1);
    assert_eq!(build_tower(&[(0, 2), (7, 5), (1, 2),]).high_point(), 7);
    assert_eq!(build_tower(&[]).high_point(), 0);
}

fn parse_jet_sequence(s: &str) -> Result<Vec<Direction>, Fail> {
    s.chars()
        .map(|ch| match ch {
            '<' => Ok(Direction::Left),
            '>' => Ok(Direction::Right),
            _ => Err(Fail(format!("unexpected jet direction {ch}"))),
        })
        .collect::<Result<Vec<Direction>, Fail>>()
}

fn draw(tower: &Tower, shape: Option<&Shape>, shape_height: usize, shape_pos: usize) -> String {
    let mut result = String::new();
    let hmax = max(tower.max_height(), shape_height + SHAPE_HEIGHT);
    let hmin = min(shape_height, shape_height.checked_sub(4).unwrap_or(0));

    for row in (hmin..=hmax).rev() {
        if row == 0 {
            result.push('+');
        } else {
            result.push('|');
        }
        for col in 0..TOWER_WIDTH {
            let shape_char: Option<char> = if let Some(shape) = shape {
                if let Some(c) = col.checked_sub(shape_pos) {
                    if let Some(r) = row.checked_sub(shape_height) {
                        if c < SHAPE_WIDTH && r < SHAPE_HEIGHT && shape.columns[c][r] {
                            Some('@')
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            };

            let ch = if row == 0 {
                '-'
            } else if let Some(ch) = shape_char {
                ch
            } else if tower.occupied.contains(&RowCol { r: row, c: col }) {
                '#'
            } else if row < shape_height {
                '.'
            } else if col < shape_pos {
                '.'
            } else if col >= shape_pos + SHAPE_WIDTH {
                '.'
            } else if row >= shape_height + SHAPE_HEIGHT {
                '.'
            } else {
                '.'
            };
            result.push(ch);
        }
        if row == 0 {
            result.push('+');
        } else {
            result.push('|');
        }
        result.push('\n');
    }
    if hmin > 0 {
        result.extend(['?'].into_iter().cycle().take(TOWER_WIDTH + 2));
    }
    result
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum Verbosity {
    None,
    High,
}

impl Verbosity {
    fn meets(&self, atleast: &Verbosity) -> bool {
        self >= atleast
    }
}

fn simulate(verbosity: &Verbosity, directions: &str, hit_limit: usize) -> Result<Tower, Fail> {
    let jets: Vec<Direction> = parse_jet_sequence(directions)?;
    let mut jet_iter = jets.iter().cycle();
    let mut shape_generator = ShapeGenerator::new();
    let mut shape: Option<&Shape> = None;
    let mut shape_pos: usize = 2;
    let mut shape_height: usize = 0;
    let mut shape_width: usize = 0;
    let mut tower = Tower::new();
    let mut hit_count: usize = 0;

    loop {
        if shape.is_none() {
            let newshape: &Shape = shape_generator.next_shape();
            shape_pos = 2;
            shape_height = tower.max_height() + 4;
            shape_width = newshape.width();
            shape = Some(newshape);

            if verbosity.meets(&Verbosity::High) {
                println!(
                    "Rock {} begins falling:\n{}",
                    hit_count + 1,
                    draw(&tower, shape, shape_height, shape_pos)
                );
            }
        }

        let direction: &Direction = jet_iter.next().unwrap();
        if tower.tick(
            verbosity,
            shape.unwrap(),
            shape_width,
            &mut shape_height,
            &mut shape_pos,
            direction,
        ) {
            if verbosity.meets(&Verbosity::High) {
                println!(
                    "hit at height {}; updated tower height is {}",
                    shape_height,
                    tower.max_height()
                );
            }
            shape = None;
            hit_count += 1;
            if verbosity.meets(&Verbosity::High) {
                println!("{hit_count} rocks have landed.");
            }
            if hit_count == hit_limit {
                return Ok(tower);
            }
        }
    }
}

fn solve_part1(verbosity: &Verbosity, directions: &str) -> Result<usize, Fail> {
    let tower = simulate(verbosity, directions, 2022)?;
    Ok(tower.high_point())
}

fn solve_part2(verbosity: &Verbosity, directions: &str) -> Result<usize, Fail> {
    let long_enough = directions.len() * SHAPE_SEQUENCE.len();
    println!("{long_enough} should be long enough");
    let tower = simulate(verbosity, directions, long_enough).expect("simulation should succeed");
    let (cycle_rocks, cycle_height) = tower.find_cycle().expect("a cycle should exist");
    println!("part 2: cycle_rocks={cycle_rocks}, cycle_height={cycle_height}");
    todo!()
}

#[cfg(test)]
fn example() -> &'static str {
    ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
}

#[test]
fn test_part1_example() {
    assert_eq!(solve_part1(&Verbosity::None, example()), Ok(3068));
}

#[test]
fn test_part2_example() {
    assert_eq!(solve_part2(&Verbosity::None, example()), Ok(1514285714288));
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt"))
        .expect("valid input")
        .trim();

    println!(
        "Day 17 part 1: {}",
        solve_part1(&Verbosity::High, input).expect("should be able to solve part 1")
    );
}
