use std::collections::HashMap;
use std::collections::VecDeque;
use std::str;

use lib::error::Fail;
use lib::grid::{CompassDirection, Position, ALL_MOVE_OPTIONS};

fn elevation(ch: char) -> i64 {
    match ch {
        'E' => elevation('z'),
        'S' => elevation('a'),
        _ => (u32::from(ch)).into(),
    }
}

#[derive(Debug)]
struct Grid {
    squares: HashMap<Position, char>,
    start: Option<Position>,
    goal: Option<Position>,
}

impl TryFrom<&str> for Grid {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Grid, Fail> {
        fn usize_to_i64(n: usize) -> i64 {
            n.try_into().expect("position is out-of-range")
        }
        let mut start: Option<Position> = None;
        let mut goal: Option<Position> = None;
        let mut squares: HashMap<Position, char> = HashMap::new();
        for (line_number, line_text) in s.split_terminator('\n').enumerate() {
            for (column, ch) in line_text.chars().enumerate() {
                let p = Position {
                    x: usize_to_i64(column),
                    y: usize_to_i64(line_number),
                };
                println!("adding '{ch}' at {p}");
                if ch == 'S' {
                    println!("the start is at {p}");
                    start = Some(p);
                } else if ch == 'E' {
                    println!("the goal is at {p}");
                    goal = Some(p);
                }
                squares.insert(p, ch);
            }
        }
        Ok(Grid {
            squares,
            start,
            goal,
        })
    }
}

impl Grid {
    fn permitted_move(fromch: char, toch: char) -> bool {
        let from = elevation(fromch);
        let to = elevation(toch);
        if to <= from {
            true
        } else {
            (from + 1) == to
        }
    }

    fn edge_exists(&self, from: Position, to: Position) -> Result<bool, Fail> {
        fn offgrid(p: Position) -> Fail {
            Fail(format!("{p} is not on the grid"))
        }
        let result = match (from.xbearing(&to), from.ybearing(&to)) {
            (Ok(None), Ok(None)) => Err(Fail(format!(
                "there is no edge allowed from {from} to {to} (which is itself)"
            ))),
            (Ok(_), Ok(_)) => {
                // Check the heights.
                match self.squares.get(&from) {
                    Some(fromch) => match self.squares.get(&to) {
                        Some(toch) => {
                            println!("checking height constaints for {fromch}->{toch}");
                            Ok(Grid::permitted_move(*fromch, *toch))
                        }
                        None => Err(offgrid(to)),
                    },
                    None => Err(offgrid(from)),
                }
            }
            (Err(_), _) | (_, Err(_)) => Err(Fail(format!(
                "{from} and {to} are not close enough together to have an edge"
            ))),
        };
        println!("edge_exists(({from}),({to})): {result:?}");
        result
    }

    fn find_shortest_path(&self) -> Result<Option<Vec<Position>>, Fail> {
        dbg!(&self);
        let start: Position = self
            .start
            .ok_or_else(|| Fail("graph has no start 'S' node".to_string()))?;
        let goal: Position = self
            .goal
            .ok_or_else(|| Fail("graph has no goal 'E' node".to_string()))?;

        let neighbours = |here: Position| -> Vec<Position> {
            let mut result = Vec::new();
            for direction in ALL_MOVE_OPTIONS {
                let next = here.move_direction(&direction);
                println!("considering {here} -> {next}");
                if self.edge_exists(here, next).unwrap_or(false) {
                    println!("edge {here} -> {next} does exist");
                    result.push(next);
                } else {
                    println!("edge {here} -> {next} does not exist");
                }
            }
            result
        };
        Ok(bfs(start, goal, neighbours))
    }
}

fn bfs<NF>(start: Position, goal: Position, neighbours: NF) -> Option<Vec<Position>>
where
    NF: Fn(Position) -> Vec<Position>,
{
    let mut frontier: VecDeque<Position> = VecDeque::new();
    let mut visited: HashMap<Position, Position> = HashMap::new();

    frontier.push_front(start);
    visited.insert(start, start);

    while let Some(p) = frontier.pop_front() {
        println!("considering {p}; {} items in the frontier", frontier.len());
        if p == goal {
            let mut path: Vec<Position> = Vec::new();
            let mut q = goal;
            path.push(q);
            while q != start {
                match visited.get(&q) {
                    Some(predecessor) => {
                        path.push(*predecessor);
                        q = *predecessor;
                    }
                    None => {
                        panic!("what was before {:?}?", q);
                    }
                }
            }
            return Some(path);
        }
        let nv = neighbours(p);
        println!("{p} has {} neighbours", nv.len());
        for n in nv.iter() {
            println!("considering edge from {p} to {n}...");
            if !visited.contains_key(n) {
                println!("we have not previously visited {n}, adding it to the frontier...");
                visited.insert(*n, p);
                frontier.push_back(*n);
            }
        }
    }
    None
}

fn solve_part1(s: &str) -> usize {
    let grid = Grid::try_from(s).expect("valid input");
    match grid.find_shortest_path() {
        Ok(Some(path)) => path.len() - 1,
        Ok(None) => {
            panic!("there is no path from S to E");
        }
        Err(e) => {
            panic!("failed: {}", e);
        }
    }
}

#[test]
fn test_part1_example() {
    let example = concat!(
        "Sabqponm\n",
        "abcryxxl\n",
        "accszExk\n",
        "acctuvwj\n",
        "abdefghi\n",
    );
    assert_eq!(solve_part1(example), 31);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    println!("Day 12 part 1: {}", solve_part1(input));
}
