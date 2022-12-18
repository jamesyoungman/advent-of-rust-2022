use std::collections::BTreeSet;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::str;

use lib::error::Fail;

#[derive(Eq, PartialEq)]
struct Cube {
    n: [u32; 3],
}

fn cube(x: u32, y: u32, z: u32) -> Cube {
    Cube { n: [x, y, z] }
}

impl Debug for Cube {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{},{},{}", self.n[0], self.n[1], self.n[2])
    }
}

impl Cube {
    fn neighbours(&self, other: &Cube) -> bool {
        self.n
            .iter()
            .zip(other.n.iter())
            .map(|(a, b)| (*a as i64 - *b as i64).abs())
            .sum::<i64>()
            == 1
    }
}

#[test]
fn test_neighbours() {
    assert!(Cube { n: [1, 1, 1] }.neighbours(&Cube { n: [2, 1, 1] }));
    assert!(!Cube { n: [0, 0, 0] }.neighbours(&Cube { n: [1, 1, 1] }));
    assert!(!Cube { n: [1, 2, 2] }.neighbours(&Cube { n: [2, 8, 12] }));
    assert!(!Cube { n: [1, 2, 2] }.neighbours(&Cube { n: [2, 2, 1] }));
}

fn exposed_faces_on_axis(first: &Cube, second: &Cube, axis: usize) -> usize {
    //dbg!(axis);
    //dbg!(first);
    //dbg!(second);
    let diffs = (
        second.n[axis] - first.n[axis],
        second.n[(axis + 1) % 3] == first.n[(axis + 1) % 3],
        second.n[(axis + 2) % 3] == first.n[(axis + 2) % 3],
    );
    match diffs {
        (1, true, true) => 0, // the cubes adjoin
        (0, true, true) => {
            panic!("co-incident cubes are not allowed");
        }
        _ => {
            // We only count the forward-facing exposed face, because
            // the first and last cubes will by definition have
            // exposed back and front faces.
            1
        }
    }
}

struct Slice {
    axis: usize,
    at: u32,
    positions: BTreeSet<[u32; 2]>,
}

fn take_slice(cubes: &[Cube], axis: usize, value: u32) -> Slice {
    Slice {
        axis,
        at: value,
        positions: cubes
            .iter()
            .filter(|cube| cube.n[axis] == value)
            .map(|cube| [cube.n[(axis + 1) % 3], cube.n[(axis + 2) % 3]])
            .collect(),
    }
}

fn count_slice_exposed_faces(first: &Slice, second: &Slice) -> usize {
    match second.at - first.at {
        0 => {
            panic!("slices must be at different positions")
        }
        1 => first.positions.len() - first.positions.intersection(&second.positions).count(),
        _ => {
            // All forward-facing faces in `first` are exposed.
            first.positions.len()
        }
    }
}

#[test]
fn test_count_slice_exposed_faces() {
    {
        let no_cubes = vec![];
        assert_eq!(
            count_slice_exposed_faces(&take_slice(&no_cubes, 0, 0), &take_slice(&no_cubes, 0, 1)),
            0
        );
    }
    {
        let not_neighbours = vec![cube(100, 100, 100), cube(100, 101, 101)];
        // If we take slices at Y=100 and Y=101, the cubes are at
        // (X=100,Z=100) and (X=100,Z=101).  So the second cube
        // doesn't cover the +Y-facing face of the first cube.
        assert_eq!(
            count_slice_exposed_faces(
                &take_slice(&not_neighbours, 1, 100),
                &take_slice(&not_neighbours, 1, 101)
            ),
            1
        );
        // If we take slices at Z=100 and Z=101, the cubes are at
        // (X=100,Y=100) and (X=100,Y=101). So the second cube doesn't cover the +Z-facing
        // face of the first cube.
        assert_eq!(
            count_slice_exposed_faces(
                &take_slice(&not_neighbours, 2, 100),
                &take_slice(&not_neighbours, 2, 101)
            ),
            1
        );
        // If we take slices at Z=100 and Z=102, all cubes (i.e. the
        // only cube) in the first slice have an exposed face in the
        // +Z direction, since the slices are not directly
        // adjoining.
        assert_eq!(
            count_slice_exposed_faces(
                &take_slice(&not_neighbours, 2, 100),
                &take_slice(&not_neighbours, 2, 102)
            ),
            1
        );
    }
    {
        let neighbours = vec![cube(1, 1, 1), cube(2, 1, 1)];
        // If we take a slice at X=1 and at X=2, we find that the +X
        // facing face of the first cube is not exposed, because the
        // second cube appears in the second slice.
        assert_eq!(
            count_slice_exposed_faces(
                &take_slice(&neighbours, 0, 1),
                &take_slice(&neighbours, 0, 2)
            ),
            0
        );
        // If we take a slice at X=2 and X=3, we find that the +X
        // facing face of the first cube is exposed (because the second slice
        // is not directly adjoining).
        assert_eq!(
            count_slice_exposed_faces(
                &take_slice(&neighbours, 0, 2),
                &take_slice(&neighbours, 0, 3)
            ),
            1
        );

        // If we take slices at Y=1 and Y=2, we find that both cubes
        // have an exposed face in the +Y direction.
        assert_eq!(
            count_slice_exposed_faces(
                &take_slice(&neighbours, 1, 1),
                &take_slice(&neighbours, 1, 2)
            ),
            2
        );
        // Same for slices at Z=1 and Z=2
        assert_eq!(
            count_slice_exposed_faces(
                &take_slice(&neighbours, 2, 1),
                &take_slice(&neighbours, 2, 2)
            ),
            2
        );
    }
}

//fn count_exposed_faces_on_axis(cubes: &mut Vec<Cube>, axis: usize) -> usize {
//    cubes.sort_by(|a, b| a.n[axis].cmp(&b.n[axis]));
//    2 * ((if cubes.is_empty() { 0 } else { 1 })
//        + cubes
//            .windows(2)
//            .map(|w| exposed_faces_on_axis(&w[0], &w[1], axis))
//            .sum::<usize>())
//}

fn count_exposed_faces_on_axis(cubes: &mut Vec<Cube>, axis: usize) -> usize {
    let slice_locations: BTreeSet<u32> = cubes.iter().map(|cube| cube.n[axis]).collect();
    let slices: Vec<Slice> = slice_locations
        .iter()
        .map(|&pos| take_slice(cubes, axis, pos))
        .collect();
    let faces_exposed_in_plus_direction: usize = slices
        .windows(2)
        .map(|w| count_slice_exposed_faces(&w[0], &w[1]))
        .sum::<usize>()
        + slices
            .last()
            .map(|slice| slice.positions.len())
            .unwrap_or(0);
    2 * faces_exposed_in_plus_direction
}

fn count_exposed_faces(cubes: &mut Vec<Cube>) -> usize {
    count_exposed_faces_on_axis(cubes, 0)
        + count_exposed_faces_on_axis(cubes, 1)
        + count_exposed_faces_on_axis(cubes, 2)
}

#[test]
fn test_count_exposed_faces_on_axis() {
    let mut example = vec![cube(1, 1, 1), cube(2, 1, 1)];
    assert_eq!(count_exposed_faces_on_axis(&mut example, 0), 2);
    assert_eq!(count_exposed_faces_on_axis(&mut example, 1), 4);
    assert_eq!(count_exposed_faces_on_axis(&mut example, 2), 4);
}

#[test]
fn test_count_exposed_colinear_faces() {
    // Test cases with colinear cubes (all on the line y=z=1)
    assert_eq!(
        count_exposed_faces(&mut vec![cube(1, 1, 1), cube(2, 1, 1)]),
        10
    );
    assert_eq!(
        count_exposed_faces(&mut vec![cube(1, 1, 1), cube(2, 1, 1), cube(3, 1, 1)]),
        14
    );
    assert_eq!(
        // These cubes are not neighbours
        count_exposed_faces(&mut vec![cube(1, 1, 1), cube(3, 1, 1)]),
        12
    );
}

#[test]
fn test_count_exposed_inplane_faces() {
    // An L shape
    assert_eq!(
        count_exposed_faces(&mut vec![cube(1, 1, 1), cube(2, 1, 1), cube(2, 2, 1)]),
        14
    );
    // A plus sign in the XY plane at Z=1. This is composed of 5
    // cubes.  The centre cube has 4 hidden faces and each of the
    // cubes that touch it also has the corresponding hidden face.
    // The other 5*6-8=22 faces are exposed.
    let mut plus = vec![
        cube(1, 1, 1), // centre of the +
        cube(0, 1, 1), // left
        cube(2, 1, 1), // right
        cube(1, 2, 1), // top
        cube(1, 0, 1), // bottom
    ];
    assert_eq!(count_exposed_faces_on_axis(&mut plus, 0), 6);
    assert_eq!(count_exposed_faces_on_axis(&mut plus, 1), 6);
    assert_eq!(count_exposed_faces_on_axis(&mut plus, 2), 10);
    assert_eq!(count_exposed_faces(&mut plus), 22);
}

impl TryFrom<&str> for Cube {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Cube, Fail> {
        let mut fields = s.split(',');
        if let Some(Ok(x)) = fields.next().map(|s| s.parse()) {
            if let Some(Ok(y)) = fields.next().map(|s| s.parse()) {
                if let Some(Ok(z)) = fields.next().map(|s| s.parse()) {
                    return Ok(cube(x, y, z));
                }
            }
        }
        Err(Fail(format!("not in x,y,z format: {s}")))
    }
}

#[test]
fn test_parse_cube() {
    assert_eq!(Cube::try_from("84,1,0"), Ok(cube(84, 1, 0)));
}

fn parse_input(s: &str) -> Result<Vec<Cube>, Fail> {
    s.split_terminator('\n')
        .map(Cube::try_from)
        .collect::<Result<Vec<Cube>, Fail>>()
}

#[cfg(test)]
fn example() -> Vec<Cube> {
    let text = concat!(
        "2,2,2\n", "1,2,2\n", "3,2,2\n", "2,1,2\n", "2,3,2\n", "2,2,1\n", "2,2,3\n", "2,2,4\n",
        "2,2,6\n", "1,2,5\n", "3,2,5\n", "2,1,5\n", "2,3,5\n",
    );
    parse_input(text).expect("example should be valid")
}

#[test]
fn example_sanity_check() {
    assert_eq!(example().len(), 13);
}

fn solve_part1(s: &str) -> usize {
    let mut cubes = parse_input(s).expect("input should be valid");
    count_exposed_faces(&mut cubes)
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    println!("Day 18 part 1: {}", solve_part1(input));
}
