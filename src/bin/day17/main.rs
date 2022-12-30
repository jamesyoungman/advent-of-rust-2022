use core::ops::Range;
use std::cmp::{max, min};
use std::collections::BTreeMap;
use std::fmt::{Debug, Display, Formatter};
use std::str;

use lib::error::Fail;

const SHAPE_HEIGHT: usize = 4;
const TOWER_WIDTH: usize = 7;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum Verbosity {
    None,
    Medium,
    High,
}

impl Verbosity {
    fn meets(&self, atleast: &Verbosity) -> bool {
        self >= atleast
    }
}

#[derive(Clone)]
struct Shape {
    // The shape is represented as bits with the lowest row in
    // rows[0].  Meaning that the patterns of 1 and 0 in the
    // initializer appear to be upside down.
    //
    // We initialise the bits in rows such that the rock is already in
    // its starting position (2 units away from the left wall).
    rows: [u8; SHAPE_HEIGHT],
}

fn shl_if_possible(x: u8) -> Option<u8> {
    if x & (1 << 7) != 0 {
        None
    } else {
        Some(x << 1)
    }
}

fn shr_if_possible(x: u8) -> Option<u8> {
    // Tower is only 7 units wide, so we should never use the 1-valued
    // position.
    if x & 0b11 != 0 {
        None
    } else {
        Some(x >> 1)
    }
}

impl Shape {
    fn shifted_left(&self) -> Option<Shape> {
        Some(Shape {
            rows: [
                shl_if_possible(self.rows[0])?,
                shl_if_possible(self.rows[1])?,
                shl_if_possible(self.rows[2])?,
                shl_if_possible(self.rows[3])?,
            ],
        })
    }

    fn shifted_right(&self) -> Option<Shape> {
        Some(Shape {
            rows: [
                shr_if_possible(self.rows[0])?,
                shr_if_possible(self.rows[1])?,
                shr_if_possible(self.rows[2])?,
                shr_if_possible(self.rows[3])?,
            ],
        })
    }

    fn occupied(&self, row: usize, col: usize) -> bool {
        self.rows[row] & (1 << (7 - col)) != 0
    }

    fn masks_for_tower_rows(&self, shape_height: usize) -> [(usize, u8); SHAPE_HEIGHT] {
        [
            (shape_height, self.rows[0]),
            (shape_height + 1, self.rows[1]),
            (shape_height + 2, self.rows[2]),
            (shape_height + 3, self.rows[3]),
        ]
    }
}

impl Display for Shape {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        fn do_write(f: &mut Formatter<'_>, block: bool) -> Result<(), std::fmt::Error> {
            f.write_str(if block { "#" } else { "." })
        }
        for rowvalue in self.rows.iter().rev() {
            for col in (0..TOWER_WIDTH).rev() {
                do_write(f, rowvalue & (1 << col) != 0)?;
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl Debug for Shape {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{self}")
    }
}

#[derive(Debug)]
struct ShapeGenerator {
    i: usize,
}
const SHAPE_SEQUENCE: [&Shape; 5] = [&MINUS, &PLUS, &ANGLE, &PIPE, &SQUARE];

impl ShapeGenerator {
    fn new() -> ShapeGenerator {
        ShapeGenerator { i: 0 }
    }

    fn next_shape(&mut self) -> (usize, Shape) {
        let current = self.i;
        self.i = (self.i + 1) % 5;
        (current, SHAPE_SEQUENCE[current].clone())
    }
}

#[test]
fn test_bottom_row_of_shapes_is_nonblank() {
    for shape in SHAPE_SEQUENCE.iter() {
        assert!(shape.rows[0] != 0);
    }
}

const MINUS: Shape = Shape {
    rows: [
        0b00111100, // bottom
        0,
        0,
        0, // top
    ],
};

const PLUS: Shape = Shape {
    rows: [
        0b00010000, // bottom
        0b00111000,
        0b00010000,
        0b00000000, // top
    ],
};

const ANGLE: Shape = Shape {
    rows: [
        0b00111000, // bottom
        0b00001000,
        0b00001000,
        0b00000000, // top
    ],
};

const PIPE: Shape = Shape {
    rows: [
        0b00100000, // bottom
        0b00100000,
        0b00100000,
        0b00100000, // top
    ],
};

const SQUARE: Shape = Shape {
    rows: [
        0b00110000, // bottom
        0b00110000,
        0b00000000,
        0b00000000, // top
    ],
};

#[derive(Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
struct RowCol {
    r: usize,
    c: usize,
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct State {
    height_gain: [usize; TOWER_WIDTH],
    rock_index: usize,
    jet_index: usize,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
struct CycleValue {
    highest: usize,
    total_rocks: usize,
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct Cycle {
    rocks_in_cycle: usize,
    height_gain: usize,
}

/// A tower's lowest level is 0, blocks can come to rest there.  In
/// effect, the floor of the tower is at level -1.
#[derive(Debug, Default)]
struct Tower {
    highest_row: Option<usize>,
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

    fn same_blocks(&self, r1: Range<usize>, r2: Range<usize>) -> bool {
        assert_eq!(r1.len(), r2.len());
        assert!(!r1.is_empty());
        assert!(!r2.is_empty());
        r1.zip(r2)
            .all(|(row1, row2)| self.occupied[&row1] == self.occupied[&row2])
    }

    fn cell_is_occupied(&self, rc: &RowCol) -> bool {
        match self.highest_row {
            Some(highest) => {
                if rc.r > highest {
                    false
                } else {
                    self.occupied.get(&rc.r).unwrap_or(&0_u8) & (1 << (7 - rc.c)) != 0
                }
            }
            None => false,
        }
    }

    fn high_point(&self) -> Option<usize> {
        let fast = self.highest_row;
        let slow = self
            .occupied
            .iter()
            .rev()
            .filter_map(|(r, cells)| if *cells != 0 { Some(*r) } else { None })
            .next();
        assert_eq!(fast, slow);
        fast
    }

    fn draw(&self, shape: Option<&Shape>, shape_height: usize) -> String {
        let mut result = String::new();
        let hmax = max(
            self.high_point().unwrap_or(shape_height),
            shape_height + SHAPE_HEIGHT,
        );
        let hmin = min(shape_height, shape_height.saturating_sub(4));

        for row in (hmin..=hmax).rev() {
            result.push('|');
            for col in 0..TOWER_WIDTH {
                let shape_char: Option<char> = if let Some(shape) = shape {
                    if let Some(r) = row.checked_sub(shape_height) {
                        if r < SHAPE_HEIGHT && shape.occupied(r, col) {
                            Some('@')
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };

                result.push(if let Some(ch) = shape_char {
                    ch
                } else if self.cell_is_occupied(&RowCol { r: row, c: col }) {
                    '#'
                } else {
                    '.'
                });
            }
            result.push('|');
            result.push('\n');
        }
        if hmin > 0 {
            result.extend(['?'].into_iter().cycle().take(TOWER_WIDTH + 2));
        } else {
            result.push_str("+-------+\n");
        }
        result
    }

    fn collision(&self, shape: &Shape, shape_height: usize) -> bool {
        let row_collision = |row, shape_mask| -> bool {
            match self.occupied.get(&row) {
                Some(tower_bits) => *tower_bits & shape_mask != 0,
                None => false,
            }
        };
        row_collision(shape_height, shape.rows[0])
            || row_collision(shape_height + 1, shape.rows[1])
            || row_collision(shape_height + 2, shape.rows[2])
            || row_collision(shape_height + 3, shape.rows[3])
    }

    /// Update the position of the shape; return true if it was updated.
    fn update_shape_pos(&self, jet: &Direction, shape: &mut Shape, shape_height: usize) -> bool {
        let updated_shape: Option<Shape> = match jet {
            Direction::Left => shape.shifted_left(),
            Direction::Right => shape.shifted_right(),
        };
        if let Some(updated) = updated_shape {
            if self.collision(&updated, shape_height) {
                false
            } else {
                *shape = updated;
                true
            }
        } else {
            false
        }
    }

    fn update_occupation_for_landing(
        &mut self,
        shape: &Shape,
        shape_height: usize,
        verbosity: &Verbosity,
    ) {
        for (row, mask) in shape.masks_for_tower_rows(shape_height).into_iter() {
            if verbosity.meets(&Verbosity::High) {
                println!("update_occupation_for_landing: row {row}: with bits {mask:08b}");
            }
            if mask != 0 {
                let givemsg = |highest| {
                    if verbosity.meets(&Verbosity::High) {
                        println!("tower now has highest row {highest} ({mask:08b})",);
                    }
                };
                match self.highest_row {
                    Some(highest) => {
                        if row > highest {
                            self.highest_row = Some(row);
                            givemsg(row);
                        }
                    }
                    None => {
                        self.highest_row = Some(row);
                        givemsg(row);
                    }
                }
            }
            self.occupied
                .entry(row)
                .and_modify(|rowbits| *rowbits |= mask)
                .or_insert(mask);
            if mask != 0 {
                assert!(
                    self.highest_row
                        .expect("a shape landed, so there must be a highest row")
                        >= row
                );
            }
        }
    }

    fn tick(
        &mut self,
        verbosity: &Verbosity,
        shape: &mut Shape,
        shape_height: &mut usize,
        rock_count: &mut usize,
        jet: &Direction,
    ) -> bool {
        let caveat: &str = if self.update_shape_pos(jet, shape, *shape_height) {
            ""
        } else {
            " but nothing happens"
        };
        if verbosity.meets(&Verbosity::High) {
            println!(
                "Jet of gas pushes rock {jet}{caveat}:\n{}",
                self.draw(Some(shape), *shape_height)
            );
        }
        let (collision_occurred, updated_height): (bool, usize) = if *shape_height > 0 {
            let height_after_fall: usize = *shape_height - 1;
            if self.collision(shape, height_after_fall) {
                if verbosity.meets(&Verbosity::High) {
                    println!("shape cannot fall from {shape_height} to {height_after_fall} because it would hit something");
                }
                (true, *shape_height)
            } else {
                if verbosity.meets(&Verbosity::High) {
                    println!("shape falls from {shape_height} to {height_after_fall} without hitting anything");
                }
                (false, height_after_fall)
            }
        } else {
            if verbosity.meets(&Verbosity::High) {
                println!("shape cannot fall from {shape_height} because it would hit the bottom");
            }
            (true, *shape_height)
        };
        *shape_height = updated_height;
        if collision_occurred {
            *rock_count += 1;
            self.update_occupation_for_landing(shape, updated_height, verbosity);
            if verbosity.meets(&Verbosity::High) {
                println!("Rock comes to rest:\n{}", self.draw(None, *shape_height));
            }
        } else if verbosity.meets(&Verbosity::High) {
            println!(
                "Rock falls one unit:\n{}",
                self.draw(Some(shape), *shape_height)
            );
        }
        collision_occurred
    }
}

#[cfg(test)]
fn build_tower(rcs: &[(usize, usize)]) -> Tower {
    let occupied: BTreeMap<usize, u8> = rcs.iter().fold(BTreeMap::new(), |mut acc, (row, col)| {
        let mask = 1 << col;
        acc.entry(*row).and_modify(|m| *m |= mask).or_insert(mask);
        acc
    });
    Tower {
        highest_row: match occupied.keys().rev().next() {
            Some(y) => Some(*y),
            None => None,
        },
        occupied,
    }
}

#[test]
fn test_tower_update_shape_pos() {
    let t = build_tower(&[(0, 2), (0, 5), (1, 2)]);
    let mut minus = MINUS.clone();
    // Minus starts out 2 columns from the left.  So we should be able
    // to shift it left twice before collision.
    // initial state: |..####.|
    assert!(t.update_shape_pos(&Direction::Left, &mut minus, 7));
    // current state: |.####..|
    assert!(t.update_shape_pos(&Direction::Left, &mut minus, 7));
    // The third time should fail because we hit the left wall.
    // current state: |####...|
    assert!(!t.update_shape_pos(&Direction::Left, &mut minus, 7));

    // The initial state of minus has columns 2,3,4,5 (bit positions
    // 5,4,3,2) filled.  Starting again from that initial state we
    // should be able to shift right 1 time (since the position with
    // bit value 1 is not inside the tower).
    let mut minus = MINUS.clone();
    // initial state: |..####.|
    assert!(t.update_shape_pos(&Direction::Right, &mut minus, 7));
    // current state: |...####|
    // Then we should hit the wall.
    assert!(!t.update_shape_pos(&Direction::Right, &mut minus, 7));
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

fn simulate(
    verbosity: &Verbosity,
    directions: &str,
    target_hit_count: usize,
) -> Result<(Tower, usize), Fail> {
    let jets: Vec<Direction> = parse_jet_sequence(directions)?;
    let mut jet_iter = jets.iter().enumerate().cycle();
    let mut shape_generator = ShapeGenerator::new();
    let mut shape: Option<Shape> = None;
    let mut shape_height: usize = 0;
    let mut tower = Tower::new();
    let mut hit_count: usize = 0;
    let mut alignment: Option<(usize, usize)> = None;
    let mut virtual_highest: usize = 0;

    loop {
        if shape.is_none() {
            let (_, newshape): (usize, Shape) = shape_generator.next_shape();
            shape_height = match tower.high_point() {
                Some(h) => h + 4,
                None => 3,
            };
            if verbosity.meets(&Verbosity::High) {
                println!(
                    "Rock {} begins falling:\n{}",
                    hit_count + 1,
                    tower.draw(Some(&newshape), shape_height)
                );
            }

            shape = Some(newshape);
        };

        let (direction_number, direction) = jet_iter.next().unwrap();
        let hit = tower.tick(
            verbosity,
            shape.as_mut().unwrap(),
            &mut shape_height,
            &mut hit_count,
            direction,
        );
        if hit {
            if verbosity.meets(&Verbosity::High) {
                println!(
                    "hit at height {}; updated tower high point is {}",
                    shape_height,
                    tower.high_point().unwrap()
                );
            }
            shape = None;
            if verbosity.meets(&Verbosity::Medium) {
                println!("{hit_count} rocks have landed.");
            }
            if hit_count >= target_hit_count {
                let total = virtual_highest + tower.high_point().unwrap() + 1;
                return Ok((tower, total));
            }
        }
        if direction_number == 0 && hit_count > 10000 {
            if verbosity.meets(&Verbosity::Medium) {
                println!("hit count {hit_count}: looking for a cycle...");
            }
            if let Some((first_alignment_height, first_alignment_rocks)) = alignment.as_ref() {
                const MARGIN: usize = 15;
                let h = tower.high_point().unwrap();
                assert!(h > *first_alignment_height);
                let r1 = *first_alignment_height..h - MARGIN;
                let r2 = (2 * first_alignment_height - h)..first_alignment_height - MARGIN;
                if r1.is_empty() || r2.is_empty() {
                    if verbosity.meets(&Verbosity::Medium) {
                        println!("h is too small {h}, deferring comparison");
                    }
                } else {
                    if verbosity.meets(&Verbosity::Medium) {
                        println!("comparing ranges {r1:?} and {r2:?}");
                    }
                    if tower.same_blocks(r1, r2) {
                        if verbosity.meets(&Verbosity::Medium) {
                            println!("found a cycle");
                        }
                        let hits_per_cycle = hit_count - first_alignment_rocks;
                        let cycle_height = h - first_alignment_height;
                        let rocks_left = target_hit_count - hit_count;
                        let cycles_to_go = rocks_left / hits_per_cycle;
                        virtual_highest = cycles_to_go * cycle_height;
                        hit_count += hits_per_cycle * cycles_to_go;
                    } else if verbosity.meets(&Verbosity::Medium) {
                        println!("no cycle");
                    }
                }
            } else {
                alignment = tower.high_point().map(|h| (h, hit_count));
            }
        }
    }
}

fn solve(verbosity: &Verbosity, directions: &str, target: usize) -> Result<usize, Fail> {
    Ok(simulate(verbosity, directions, target)?.1)
}

fn solve1(verbosity: &Verbosity, directions: &str) -> Result<usize, Fail> {
    solve(verbosity, directions, 2022)
}

fn solve2(verbosity: &Verbosity, directions: &str) -> Result<usize, Fail> {
    solve(verbosity, directions, 1000000000000)
}

#[cfg(test)]
fn example() -> &'static str {
    ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
}

#[test]
fn test_part1_example() {
    let height = solve1(&Verbosity::Medium, example()).expect("should find a cycle for part 1");
    assert_eq!(height, 3068);
}

#[test]
fn test_part2_example() {
    let height = solve2(&Verbosity::Medium, example()).expect("should find a cycle for part 2");
    assert_eq!(height, 1514285714288);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt"))
        .expect("valid input")
        .trim();

    let v = Verbosity::None;
    println!(
        "Day 17 part 1: {}",
        solve1(&v, input).expect("should be able to find a solution")
    );
    println!(
        "Day 17 part 2: {}",
        solve2(&v, input).expect("should be able to find a solution")
    );
}
