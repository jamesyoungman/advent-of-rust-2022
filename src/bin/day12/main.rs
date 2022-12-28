use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::str;

use lib::error::Fail;
use lib::grid::{Position, ALL_MOVE_OPTIONS};

//use petgraph::algo::bellman_ford;
//use petgraph::visit::NodeIndexable;
//use petgraph::Graph;

fn elevation(ch: char) -> i64 {
    match ch {
        'E' => elevation('z'),
        'S' => elevation('a'),
        _ => (u32::from(ch)).into(),
    }
}

#[derive(Debug)]
struct Grid {
    squares: BTreeMap<Position, char>,
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
        let mut squares: BTreeMap<Position, char> = BTreeMap::new();
        for (line_number, line_text) in s.split_terminator('\n').enumerate() {
            for (column, ch) in line_text.chars().enumerate() {
                let p = Position {
                    x: usize_to_i64(column),
                    y: usize_to_i64(line_number),
                };
                if ch == 'S' {
                    start = Some(p);
                } else if ch == 'E' {
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
        let result = match (from.neighbour_xbearing(&to), from.neighbour_ybearing(&to)) {
            (Ok(None), Ok(None)) => Err(Fail(format!(
                "there is no edge allowed from {from} to {to} (which is itself)"
            ))),
            (Ok(_), Ok(_)) => {
                // Check the heights.
                match self.squares.get(&from) {
                    Some(fromch) => match self.squares.get(&to) {
                        Some(toch) => Ok(Grid::permitted_move(*fromch, *toch)),
                        None => Err(offgrid(to)),
                    },
                    None => Err(offgrid(from)),
                }
            }
            (Err(_), _) | (_, Err(_)) => Err(Fail(format!(
                "{from} and {to} are not close enough together to have an edge"
            ))),
        };
        result
    }

    fn next_steps(&self, from: Position) -> Vec<Position> {
        let mut result = Vec::new();
        for direction in ALL_MOVE_OPTIONS {
            let next = from.move_direction(&direction);
            if self.edge_exists(from, next).unwrap_or(false) {
                result.push(next);
            }
        }
        result
    }

    //fn prev_steps(&self, to: Position) -> Vec<Position> {
    //    let mut result = Vec::new();
    //    for direction in ALL_MOVE_OPTIONS {
    //        let prev = to.move_direction(&direction);
    //        if self.edge_exists(prev, to).unwrap_or(false) {
    //            result.push(prev);
    //        }
    //    }
    //    result
    //}

    fn find_shortest_path(&self, start: Position) -> Result<Option<Vec<Position>>, Fail> {
        let goal: Position = self
            .goal
            .ok_or_else(|| Fail("graph has no goal 'E' node".to_string()))?;

        let neighbours = |here: Position| -> Vec<Position> { self.next_steps(here) };
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
                        panic!("what was before {q:?}?");
                    }
                }
            }
            return Some(path);
        }
        let nv = neighbours(p);
        for n in nv.iter() {
            if !visited.contains_key(n) {
                visited.insert(*n, p);
                frontier.push_back(*n);
            }
        }
    }
    None
}

fn solve_part1(s: &str) -> usize {
    let grid = Grid::try_from(s).expect("valid input");
    let start: Position = match grid.start {
        Some(g) => g,
        None => {
            panic!("graph has no start 'S' node");
        }
    };
    match grid.find_shortest_path(start) {
        Ok(Some(path)) => path.len() - 1,
        Ok(None) => {
            panic!("there is no path from S to E");
        }
        Err(e) => {
            panic!("failed: {e}");
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

//fn solve_part2_bf(s: &str) -> Option<usize> {
//    const FIXED_COST: f64 = 1.0;
//    let grid = Grid::try_from(s).expect("valid input");
//    let mut graph = Graph::<Position, f64>::new();
//    let mut node_index: HashMap<Position, _> = HashMap::new();
//    let mut goal_ix: Option<_> = None;
//    // We're using the bellman-ford algorithm which gives shortest
//    // paths from a single source to all other vertices.  So as source
//    // we use the 'E' square (which in reality is our destination).
//    // This means that all our edges have to point backwards.
//    for (pos, ch) in grid.squares.iter() {
//        let dest_ix = match node_index.get(pos) {
//            Some(ix) => *ix,
//            None => {
//                let ix = graph.add_node(*pos);
//                node_index.insert(*pos, ix);
//                ix
//            }
//        };
//        if *ch == 'E' {
//            goal_ix = Some(dest_ix);
//        }
//        for prev in grid.prev_steps(*pos) {
//            let prev_ix = match node_index.get(&prev) {
//                Some(ix) => *ix,
//                None => {
//                    let ix = graph.add_node(prev);
//                    node_index.insert(prev, ix);
//                    ix
//                }
//            };
//            // This is a backward edge, see above.
//            graph.update_edge(prev_ix, dest_ix, FIXED_COST);
//        }
//    }
//    match goal_ix {
//        Some(g) => match bellman_ford(&graph, g) {
//            Ok(path) => {
//                dbg!(&path);
//                // path.distances gives the distance from the source
//                // (here, 'E') to each node.
//                let a = elevation('a');
//                let candidates: Vec<(Position, _)> = grid
//                    .squares
//                    .iter()
//                    .filter_map(|(pos, ch)| {
//                        if elevation(*ch) == a {
//                            match node_index.get(pos) {
//                                Some(ix) => Some((*pos, ix)),
//                                None => panic!("index is incomplete"),
//                            }
//                        } else {
//                            None
//                        }
//                    })
//                    .collect();
//                todo!()
//                //candidates
//                //    .iter()
//                //    .map(|(pos, ix)| {
//                //        let i: usize = graph.to_index(*pos);
//                //        path.distances[i]
//                //    })
//                //    .min()
//            }
//            Err(_negative_loop) => {
//                unreachable!("the fixed cost should be positive");
//            }
//        },
//        None => {
//            panic!("the grid has no goal node");
//        }
//    }
//}

fn solve_part2_bruteforce(s: &str) -> Option<usize> {
    let grid = Grid::try_from(s).expect("valid input");
    grid.squares
        .iter()
        .filter_map(|(pos, ch)| {
            if *ch == 'a' || *ch == 'S' {
                Some(pos)
            } else {
                None
            }
        })
        .filter_map(|start| match grid.find_shortest_path(*start) {
            Ok(Some(path)) => Some(path.len() - 1),
            Ok(None) => None,
            Err(e) => {
                panic!("failed: {e}");
            }
        })
        .min()
}

#[test]
fn test_part2_example_bruteforce() {
    let example = concat!(
        "Sabqponm\n",
        "abcryxxl\n",
        "accszExk\n",
        "acctuvwj\n",
        "abdefghi\n",
    );
    assert_eq!(
        solve_part2_bruteforce(example).expect("example should have solution"),
        29
    );
}

//#[test]
//fn test_part2_example_bellman_ford() {
//    let example = concat!(
//        "Sabqponm\n",
//        "abcryxxl\n",
//        "accszExk\n",
//        "acctuvwj\n",
//        "abdefghi\n",
//    );
//    assert_eq!(
//        solve_part2_bf(example).expect("example should have solution"),
//        29
//    );
//}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    println!("Day 12 part 1: {}", solve_part1(input));
    println!(
        "Day 12 part 2: {}",
        solve_part2_bruteforce(input).expect("no solution for part 2")
    );
}
