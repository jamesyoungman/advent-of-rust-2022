use std::{
    cmp::max,
    collections::{BTreeSet, HashMap},
    iter::once,
    str,
};

use lib::error::Fail;
use lib::grid::{CompassDirection, Position, ALL_MOVE_OPTIONS};

#[derive(Debug)]
struct Weather {
    blizzard_initial_positions: HashMap<Position, CompassDirection>,
}

fn blizzard_position(
    initial: &Position,
    direction: CompassDirection,
    width: i64,
    height: i64,
    minute: i64,
) -> Position {
    fn pos_at_time(initial: i64, velocity: i64, t: i64, modulus: i64) -> i64 {
        (velocity * t + initial) % modulus
    }
    let w = width - 2;
    let h = height - 2;
    match direction {
        CompassDirection::North => Position {
            x: initial.x,
            y: 1 + (h + pos_at_time(initial.y - 1, -1, minute, h)) % h,
        },
        CompassDirection::South => Position {
            x: initial.x,
            y: 1 + pos_at_time(initial.y - 1, 1, minute, h) % h,
        },
        CompassDirection::East => Position {
            x: 1 + pos_at_time(initial.x - 1, 1, minute, w) % w,
            y: initial.y,
        },
        CompassDirection::West => Position {
            x: 1 + (w + pos_at_time(initial.x - 1, -1, minute, w)) % w,
            y: initial.y,
        },
    }
}

#[test]
fn test_blizzard_position() {
    assert_eq!(
        blizzard_position(
            &Position { y: 6, x: 40 },
            CompassDirection::East,
            200,
            200,
            0
        ),
        Position { x: 40, y: 6 }
    );
    assert_eq!(
        blizzard_position(
            &Position { y: 6, x: 40 },
            CompassDirection::East,
            200,
            200,
            1
        ),
        Position { x: 41, y: 6 }
    );
    assert_eq!(
        blizzard_position(
            &Position { x: 40, y: 6 },
            CompassDirection::South,
            200,
            200,
            1
        ),
        Position { x: 40, y: 7 }
    );
    assert_eq!(
        blizzard_position(
            &Position { x: 40, y: 6 },
            CompassDirection::North,
            200,
            200,
            1
        ),
        Position { x: 40, y: 5 }
    );
}

#[derive(Debug, PartialEq, Eq)]
enum BlizzardsAtPos {
    One(CompassDirection),
    Several(u8),
}

impl BlizzardsAtPos {
    fn len(&self) -> usize {
        match self {
            BlizzardsAtPos::One(_) => 1,
            BlizzardsAtPos::Several(n) => (*n).into(),
        }
    }

    #[cfg(test)]
    fn as_char(&self) -> char {
        match self {
            BlizzardsAtPos::One(CompassDirection::North) => '^',
            BlizzardsAtPos::One(CompassDirection::East) => '>',
            BlizzardsAtPos::One(CompassDirection::South) => 'v',
            BlizzardsAtPos::One(CompassDirection::West) => '<',
            BlizzardsAtPos::Several(n) => match char::from_digit(u32::from(*n), 10) {
                Some(ch) => ch,
                None => unreachable!(),
            },
        }
    }
}

impl Weather {
    fn blizzard_positions(
        &self,
        valley_length: i64,
        valley_width: i64,
        minute: i64,
    ) -> HashMap<Position, BlizzardsAtPos> {
        let mut result: HashMap<Position, BlizzardsAtPos> = HashMap::new();
        for (initial_pos, &direction) in self.blizzard_initial_positions.iter() {
            let pos =
                blizzard_position(initial_pos, direction, valley_width, valley_length, minute);
            result
                .entry(pos)
                .and_modify(|what| match what {
                    BlizzardsAtPos::One(_) => {
                        *what = BlizzardsAtPos::Several(2);
                    }
                    BlizzardsAtPos::Several(n) => {
                        *what = BlizzardsAtPos::Several(*n + 1);
                    }
                })
                .or_insert(BlizzardsAtPos::One(direction));
        }
        result
    }
}

#[test]
fn test_blizzard_positions() {
    use CompassDirection::*;
    let weather = Weather {
        blizzard_initial_positions: [
            (Position { x: 1, y: 1 }, East),
            (Position { x: 3, y: 1 }, West),
        ]
        .into_iter()
        .collect(),
    };
    let pos0 = weather.blizzard_positions(1, 5, 0);
    dbg!(&pos0);
    // #####
    // #>.<#
    // #####
    assert_eq!(
        pos0.get(&Position { x: 1, y: 1 }),
        Some(&BlizzardsAtPos::One(East))
    );
    assert_eq!(
        pos0.get(&Position { x: 3, y: 1 }),
        Some(&BlizzardsAtPos::One(West))
    );
    let pos1 = weather.blizzard_positions(1, 5, 1);
    dbg!(&pos1);
    // #####
    // #.2.#
    // #####
    assert_eq!(pos1.get(&Position { x: 2, y: 1 }).unwrap().len(), 2);
    let pos2 = weather.blizzard_positions(1, 5, 2);
    dbg!(&pos2);
    // #####
    // #<.>#
    // #####
    assert_eq!(
        pos2.get(&Position { x: 1, y: 1 }),
        Some(&BlizzardsAtPos::One(West))
    );
    assert_eq!(
        pos2.get(&Position { x: 3, y: 1 }),
        Some(&BlizzardsAtPos::One(East))
    );
    let pos3 = weather.blizzard_positions(1, 5, 3);
    dbg!(&pos3);
    // #####
    // #>.<#
    // #####
    assert_eq!(
        pos3.get(&Position { x: 1, y: 1 }),
        Some(&BlizzardsAtPos::One(East))
    );
    assert_eq!(
        pos3.get(&Position { x: 3, y: 1 }),
        Some(&BlizzardsAtPos::One(West))
    );
}

#[test]
fn test_example_blizzards_1() {
    let (_, weather) = parse_input(example()).expect("example should be valid");
    let positions = weather.blizzard_positions(6, 8, 1);
    assert_eq!(
        positions.get(&Position { x: 2, y: 1 }),
        Some(&BlizzardsAtPos::One(CompassDirection::East))
    );
    assert_eq!(
        positions.get(&Position { x: 5, y: 1 }),
        Some(&BlizzardsAtPos::One(CompassDirection::West))
    );
    assert_eq!(
        positions.get(&Position { x: 3, y: 1 }),
        Some(&BlizzardsAtPos::Several(3))
    );
}

#[derive(Debug)]
struct Valley {
    width: i64,
    length: i64,
    entrance: i64,
    exit: i64,
}

impl Valley {
    fn is_valid_position(&self, pos: &Position) -> bool {
        if self.is_exit(pos) || self.is_entrance(pos) {
            true
        } else if pos.y < 1 || pos.x < 1 {
            false
        } else if pos.y >= self.length - 1 || pos.x >= self.width - 1 {
            false
        } else {
            true
        }
    }

    fn is_entrance(&self, pos: &Position) -> bool {
        pos.y == 0 && pos.x == self.entrance
    }

    fn is_exit(&self, pos: &Position) -> bool {
        pos.y == self.length - 1 && pos.x == self.exit
    }

    fn neighbours(&self, pos: &Position) -> Vec<Position> {
        ALL_MOVE_OPTIONS
            .iter()
            .map(|dir| pos.move_direction(dir))
            .chain(once(*pos)) // can also stay still.
            .filter(|pos| self.is_valid_position(&pos))
            .collect()
    }

    #[cfg(test)]
    fn to_string(&self, expedition: &Position, weather: &Weather, minute: i64) -> String {
        let mut result: String =
            String::with_capacity((self.width as usize + 3) * (self.length as usize + 1));

        let blank = |pos: &Position| -> char {
            if expedition == pos {
                'E'
            } else {
                '.'
            }
        };
        let entry_exit_row = |row: i64, width: i64, freecol: i64| -> String {
            (0..width)
                .map(|col| {
                    let pos = Position { x: col, y: row };
                    if col == freecol {
                        blank(&pos)
                    } else {
                        '#'
                    }
                })
                .chain(once('\n'))
                .collect()
        };

        let blizzards: HashMap<Position, BlizzardsAtPos> =
            weather.blizzard_positions(self.length, self.width, minute);
        for y in 0..self.length {
            let expcol = if expedition.y == y {
                Some(expedition.x)
            } else {
                None
            };
            if y == 0 {
                result.push_str(&entry_exit_row(y, self.width, self.entrance));
                continue;
            } else if y == self.length - 1 {
                result.push_str(&entry_exit_row(y, self.width, self.exit));
                continue;
            }

            for x in 0..self.width {
                if x == 0 || x == self.width - 1 {
                    result.push('#');
                    continue;
                }
                if let Some(expedition_x) = expcol {
                    if expedition_x == x {
                        result.push('E');
                        continue;
                    }
                }
                // here is the position in the coordinate system of the weather.
                let here = Position { x, y };

                match blizzards.get(&here) {
                    None => {
                        result.push('.');
                    }
                    Some(bp) => {
                        result.push(bp.as_char());
                    }
                }
            }
            result.push('\n');
        }

        result
    }
}

fn find_entrance_or_exit(line: &str) -> Option<i64> {
    line.chars()
        .enumerate()
        .filter_map(|(col, ch)| if ch == '.' { Some(col as i64) } else { None })
        .next()
}

fn char_to_direction(ch: char) -> Option<CompassDirection> {
    use CompassDirection::*;
    match ch {
        '>' => Some(East),
        '<' => Some(West),
        'v' => Some(South),
        '^' => Some(North),
        _ => None,
    }
}

fn parse_input(s: &str) -> Result<(Valley, Weather), Fail> {
    let mut entrance: Option<i64> = None;
    let mut exit: Option<i64> = None;
    let mut blizzard_initial_positions: HashMap<Position, CompassDirection> = HashMap::new();
    let mut last_line = 0;
    let mut maxcol: usize = 0;
    for (line_number, line) in s.split_terminator('\n').enumerate() {
        last_line = line_number;
        if entrance.is_none() {
            entrance = find_entrance_or_exit(line);
        } else {
            exit = find_entrance_or_exit(line);
        }
        for (col, ch) in line.chars().enumerate() {
            maxcol = max(maxcol, col);
            if let Some(dir) = char_to_direction(ch) {
                blizzard_initial_positions.insert(
                    Position {
                        x: col as i64,
                        y: line_number as i64,
                    },
                    dir,
                );
            }
        }
    }
    let valley = Valley {
        width: maxcol as i64 + 1,
        length: last_line as i64 + 1,
        entrance: entrance.expect("entrance"),
        exit: exit.expect("exit"),
    };
    let weather = Weather {
        blizzard_initial_positions,
    };
    Ok((valley, weather))
}

#[cfg(test)]
fn example() -> &'static str {
    concat!(
        "#.######\n",
        "#>>.<^<#\n",
        "#.<..<<#\n",
        "#>v.><>#\n",
        "#<^v^^>#\n",
        "######.#\n",
    )
}

#[test]
fn test_parse_input() {
    let (v, _) =
        parse_input(concat!("#.#\n", "#.#\n", "#.#\n", "#.#\n")).expect("test input is valid");
    assert_eq!(v.width, 3);
    assert_eq!(v.length, 4);
    assert_eq!(v.entrance, 1);
    assert_eq!(v.exit, 1);
}

#[test]
fn test_initial_blizzard_positions() {
    let (_, weather) = parse_input(example()).expect("example should be valid");

    assert_eq!(
        weather
            .blizzard_initial_positions
            .get(&Position { x: 1, y: 1 }),
        Some(&CompassDirection::East)
    );
    assert_eq!(
        weather
            .blizzard_initial_positions
            .get(&Position { x: 2, y: 1 }),
        Some(&CompassDirection::East)
    );
    assert_eq!(
        weather
            .blizzard_initial_positions
            .get(&Position { x: 3, y: 1 }),
        None
    );
    assert_eq!(
        weather
            .blizzard_initial_positions
            .get(&Position { x: 4, y: 1 }),
        Some(&CompassDirection::West)
    );
    assert_eq!(
        weather
            .blizzard_initial_positions
            .get(&Position { x: 5, y: 1 }),
        Some(&CompassDirection::North)
    );
    assert_eq!(
        weather
            .blizzard_initial_positions
            .get(&Position { x: 6, y: 1 }),
        Some(&CompassDirection::West)
    );
}

#[test]
fn test_display_0() {
    let (valley, weather) = parse_input(example()).expect("example should be valid");
    let e = Position { x: 1, y: 0 };
    let output = valley.to_string(&e, &weather, 0);
    assert_eq!(
        output,
        concat!(
            "#E######\n",
            "#>>.<^<#\n",
            "#.<..<<#\n",
            "#>v.><>#\n",
            "#<^v^^>#\n",
            "######.#\n",
        )
    );
}

#[test]
fn test_display_1() {
    let (valley, weather) = parse_input(example()).expect("example should be valid");
    let e = Position { x: 1, y: 1 };
    let output = valley.to_string(&e, &weather, 1);
    assert_eq!(
        output,
        concat!(
            "#.######\n",
            "#E>3.<.#\n",
            "#<..<<.#\n",
            "#>2.22.#\n",
            "#>v..^<#\n",
            "######.#\n",
        )
    );
}

#[test]
fn test_display_2() {
    let (valley, weather) = parse_input(example()).expect("example should be valid");
    let e = Position { x: 1, y: 2 };
    let output = valley.to_string(&e, &weather, 2);
    assert_eq!(
        output,
        concat!(
            "#.######\n",
            "#.2>2..#\n",
            "#E^22^<#\n",
            "#.>2.^>#\n",
            "#.>..<.#\n",
            "######.#\n",
        )
    );
}

#[test]
fn test_display_9() {
    let (valley, weather) = parse_input(example()).expect("example should be valid");
    let e = Position { x: 2, y: 1 };
    let output = valley.to_string(&e, &weather, 9);
    assert_eq!(
        output,
        concat!(
            "#.######\n",
            "#<E2>>.#\n",
            "#.<<.<.#\n",
            "#>2>2^.#\n",
            "#.v><^.#\n",
            "######.#\n",
        )
    );
}

fn bfs<NF>(start: Position, goal: Position, neighbours: NF) -> Option<i64>
where
    NF: Fn(Position, i64) -> Vec<Position>,
{
    let mut frontier: BTreeSet<Position> = BTreeSet::new();
    let mut minute: i64 = 0;

    frontier.insert(start);

    while !frontier.is_empty() {
        println!(
            "minute {minute}: {} positions on the frontier",
            frontier.len()
        );

        if frontier.contains(&goal) {
            return Some(minute);
        }

        minute += 1;
        let next_frontier: BTreeSet<Position> = frontier
            .iter()
            .flat_map(|p| neighbours(*p, minute))
            .collect();
        frontier = next_frontier;
    }
    None
}

fn solve_part1(valley: &Valley, weather: &Weather) -> Option<i64> {
    let neighbours = |pos: Position, t: i64| -> Vec<Position> {
        let blizzards = weather.blizzard_positions(valley.length, valley.width, t);
        let mut neighbours = valley.neighbours(&pos);
        neighbours.retain(|pos| !blizzards.contains_key(pos));
        neighbours
    };
    let start = Position {
        x: valley.entrance,
        y: 0,
    };
    let goal = Position {
        x: valley.exit,
        y: valley.length - 1,
    };
    bfs(start, goal, neighbours)
}

#[test]
fn test_solve_part1() {
    let (valley, weather) = parse_input(example()).expect("example should be valid");
    assert_eq!(solve_part1(&valley, &weather), Some(18));
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    let (valley, weather) = parse_input(input).expect("input should be valid");
    println!(
        "Day 24 part 1: {}",
        solve_part1(&valley, &weather).expect("should find solution")
    );
}
