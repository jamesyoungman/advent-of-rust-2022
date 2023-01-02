use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::str;

use regex::Regex;

use lib::error::Fail;
use lib::grid::{bounds, BoundingBox, CompassDirection, Position};

#[cfg(test)]
fn example() -> &'static str {
    concat!(
        "        ...#    \n",
        "        .#..    \n",
        "        #...    \n",
        "        ....    \n",
        "...#.......#    \n",
        "........#...    \n",
        "..#....#....    \n",
        "..........#.    \n",
        "        ...#....\n",
        "        .....#..\n",
        "        .#......\n",
        "        ......#.\n",
        "\n",
        "10R5L5R10L4R5L5 \n",
    )
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum Cell {
    Open,
    Wall,
}

struct Grid {
    cells: HashMap<Position, Cell>,
    bbox: BoundingBox,
}

impl Grid {
    fn start_position(&self) -> Position {
        ((self.bbox.top_left.x)..=(self.bbox.bottom_right.x))
            .map(|x| Position {
                x,
                y: self.bbox.top_left.y,
            })
            .find(|pos: &Position| self.cells.contains_key(pos))
            .expect("at least one cell on the top row should be occupied")
    }

    fn is_wall(&self, pos: &Position) -> bool {
        match self.cells.get(pos) {
            Some(Cell::Open) => false,
            Some(Cell::Wall) => true,
            None => {
                panic!("Grid::is_wall was called for position {pos} which is not occupied");
            }
        }
    }

    fn wrap(&self, mut pos: Position, heading: &CompassDirection) -> Position {
        while !self.cells.contains_key(&pos) {
            pos = self.bounds_wrap(pos.move_direction(heading));
        }
        pos
    }

    fn bounds_wrap(&self, pos: Position) -> Position {
        // We wrap in each direction separately since diagonal moves
        // are not allowed.
        if pos.x > self.bbox.bottom_right.x {
            Position {
                x: self.bbox.top_left.x,
                ..pos
            }
        } else if pos.y > self.bbox.bottom_right.y {
            Position {
                y: self.bbox.top_left.y,
                ..pos
            }
        } else if pos.x < self.bbox.top_left.x {
            Position {
                x: self.bbox.bottom_right.x,
                ..pos
            }
        } else if pos.y < self.bbox.top_left.y {
            Position {
                y: self.bbox.bottom_right.y,
                ..pos
            }
        } else {
            pos
        }
    }
}

#[test]
fn test_is_wall() {
    let (grid, _) = parse_input(example());
    assert!(grid.is_wall(&Position { x: 11, y: 0 }));
    assert!(!grid.is_wall(&Position { x: 10, y: 0 }));
}

fn build_cell(x: usize, y: usize, ch: char) -> Option<(Position, Cell)> {
    let pos = Position {
        x: x as i64,
        y: y as i64,
    };
    match ch {
        ' ' => None, // cell does not exist
        '#' => Some((pos, Cell::Wall)),
        '.' => Some((pos, Cell::Open)),
        _ => {
            panic!("unexpected character '{ch}' in input");
        }
    }
}

fn build_line(y: usize, line: &str) -> impl Iterator<Item = (Position, Cell)> + '_ {
    line.chars()
        .enumerate()
        .filter_map(move |(x, ch)| build_cell(x, y, ch))
}

impl TryFrom<&str> for Grid {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Grid, Fail> {
        let cells: HashMap<Position, Cell> = s
            .split_terminator('\n')
            .enumerate()
            .flat_map(|(y, line)| build_line(y, line))
            .collect();
        match bounds(cells.keys()) {
            Some(bbox) => Ok(Grid { cells, bbox }),
            None => Err(Fail("grid is empty".to_string())),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum Instruction {
    Right,
    Left,
    Proceed(i64),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Instruction::Right => f.write_str("R"),
            Instruction::Left => f.write_str("L"),
            Instruction::Proceed(distance) => write!(f, "{distance}"),
        }
    }
}

fn parse_directions(s: &str) -> Vec<Instruction> {
    let mut result = Vec::new();
    let re = Regex::new(r"([RL])|(\d+)").expect("valid regex");
    for cap in re.captures_iter(s) {
        match cap.get(1).map(|m| m.as_str()) {
            Some("R") => {
                result.push(Instruction::Right);
            }
            Some("L") => {
                result.push(Instruction::Left);
            }
            Some(_) => {
                panic!("inconsistency between regex and code");
            }
            None => match cap.get(2).map(|m| m.as_str()) {
                Some(s) => {
                    let n: i64 = s.parse().expect("valid number");
                    result.push(Instruction::Proceed(n));
                }
                None => {
                    panic!("unexpected capture: {cap:?}");
                }
            },
        }
    }
    result
}

#[test]
fn test_parse_directions() {
    for input in ["10R5L5R10L4R5L5"] {
        let parsed = parse_directions(input);
        let mut displayed = String::new();
        for inst in parsed.iter() {
            displayed.push_str(&inst.to_string());
        }
        assert_eq!(input, displayed);
    }
}

fn parse_input(s: &str) -> (Grid, Vec<Instruction>) {
    if let Some((grid_str, instr_str)) = s.split_once("\n\n") {
        match (Grid::try_from(grid_str), parse_directions(instr_str)) {
            (Ok(grid), instructions) => (grid, instructions),
            (Err(e), _) => {
                panic!("invalid grid: {e}");
            }
        }
    } else {
        panic!("missing blank line in input {s}");
    }
}

struct State {
    heading: CompassDirection,
    position: Position,
}

impl State {
    fn password(&self) -> i64 {
        let col = self.position.x + 1;
        let row = self.position.y + 1;
        1000 * row
            + 4 * col
            + match self.heading {
                CompassDirection::East => 0,
                CompassDirection::South => 1,
                CompassDirection::West => 2,
                CompassDirection::North => 3,
            }
    }

    fn new(grid: &Grid) -> State {
        State {
            heading: CompassDirection::East,
            position: grid.start_position(),
        }
    }

    fn obey(&mut self, instruction: &Instruction, grid: &Grid) {
        use CompassDirection::*;
        match instruction {
            Instruction::Right => {
                self.heading = match self.heading {
                    North => East,
                    East => South,
                    South => West,
                    West => North,
                }
            }
            Instruction::Left => {
                self.heading = match self.heading {
                    North => West,
                    East => North,
                    South => East,
                    West => South,
                }
            }
            Instruction::Proceed(distance) => {
                for _ in 0..*distance {
                    let nextpos =
                        grid.wrap(self.position.move_direction(&self.heading), &self.heading);
                    if grid.is_wall(&nextpos) {
                        break;
                    } else {
                        self.position = nextpos;
                    }
                }
            }
        }
    }
}

#[test]
fn test_example() {
    let (grid, _) = parse_input(example());
    let mut state = State::new(&grid);
    // Move in 1-step increments along the top row,
    // verify that we stop when blocked by the wall.
    assert_eq!(state.position, Position { x: 8, y: 0 });
    state.obey(&Instruction::Proceed(1), &grid);
    assert_eq!(state.position, Position { x: 9, y: 0 });
    state.obey(&Instruction::Proceed(1), &grid);
    assert_eq!(state.position, Position { x: 10, y: 0 });
    state.obey(&Instruction::Proceed(1), &grid); // blocked by wall.
    assert_eq!(state.position, Position { x: 10, y: 0 }); // unchanged.

    state.position = Position { x: 8, y: 0 }; // back to start point
    state.obey(&Instruction::Proceed(500), &grid); // blocked by same wall
    assert_eq!(state.position, Position { x: 10, y: 0 }); // check we stop in the right place

    // Wrap then hit a wall; we start immediately below a wall.
    let mut state = State {
        heading: CompassDirection::South,
        position: Position { x: 3, y: 5 },
    };
    // We should end up at (3,7) because (3,4) contains a wall.
    state.obey(&Instruction::Proceed(500), &grid);
    assert_eq!(state.position, Position { x: 3, y: 7 });
}

fn make_journey(grid: &Grid, instructions: &[Instruction]) -> State {
    instructions
        .iter()
        .fold(State::new(grid), |mut state, instruction| {
            state.obey(instruction, grid);
            state
        })
}

fn solve_part1(s: &str) -> i64 {
    let (grid, instructions) = parse_input(s);
    let final_state = make_journey(&grid, &instructions);
    final_state.password()
}

#[test]
fn test_solve_part1() {
    assert_eq!(solve_part1(example()), 6032);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    println!("Day 22 part 1: {}", solve_part1(input));
}
