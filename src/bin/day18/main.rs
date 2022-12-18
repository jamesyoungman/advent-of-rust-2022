use std::fmt::{Debug, Display, Formatter};
use std::str;

use lib::error::Fail;

#[derive(Eq, PartialEq, Clone, Hash)]
struct Cube {
    n: [i32; 3],
}

fn cube(x: i32, y: i32, z: i32) -> Cube {
    Cube { n: [x, y, z] }
}

impl Debug for Cube {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{},{},{}", self.n[0], self.n[1], self.n[2])
    }
}

impl Display for Cube {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{},{},{}", self.n[0], self.n[1], self.n[2])
    }
}

impl Cube {
    fn all_possible_neighbours(&self) -> Vec<Cube> {
        let x = self.n[0];
        let y = self.n[1];
        let z = self.n[2];
        vec![
            cube(x - 1, y, z),
            cube(x + 1, y, z),
            cube(x, y - 1, z),
            cube(x, y + 1, z),
            cube(x, y, z - 1),
            cube(x, y, z + 1),
        ]
    }
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

mod part1 {
    use std::collections::BTreeSet;

    #[cfg(test)]
    use super::{cube, example};
    use super::{parse_input, Cube};

    pub fn solve_part1(s: &str) -> usize {
        let mut cubes = parse_input(s).expect("input should be valid");
        count_exposed_faces(&mut cubes)
    }

    struct Slice {
        at: i32,
        positions: BTreeSet<[i32; 2]>,
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
                count_slice_exposed_faces(
                    &take_slice(&no_cubes, 0, 0),
                    &take_slice(&no_cubes, 0, 1)
                ),
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

    fn take_slice(cubes: &[Cube], axis: usize, value: i32) -> Slice {
        Slice {
            at: value,
            positions: cubes
                .iter()
                .filter(|cube| cube.n[axis] == value)
                .map(|cube| [cube.n[(axis + 1) % 3], cube.n[(axis + 2) % 3]])
                .collect(),
        }
    }

    fn count_exposed_faces_on_axis(cubes: &mut [Cube], axis: usize) -> usize {
        let slice_locations: BTreeSet<i32> = cubes.iter().map(|cube| cube.n[axis]).collect();
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

    fn count_exposed_faces(cubes: &mut [Cube]) -> usize {
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
    fn test_solve_part1() {
        assert_eq!(count_exposed_faces(&mut example()), 64);
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
}

mod part2 {
    use std::collections::HashSet;

    use core::ops::Range;

    #[cfg(test)]
    use super::{cube, example};
    use super::{parse_input, Cube};

    fn bounds_of_droplet_for_axis(cubes: &[Cube], axis: usize) -> Option<Range<i32>> {
        let extract_value = |cube: &Cube| cube.n[axis];
        let min = cubes.iter().map(extract_value).min();
        let max = cubes.iter().map(extract_value).max();
        match (min, max) {
            (Some(low), Some(high)) => Some(low..(high + 1)),
            (None, None) => None,
            (None, Some(_)) | (Some(_), None) => unreachable!(),
        }
    }

    fn bounds_of_droplet(cubes: &[Cube]) -> Option<[Range<i32>; 3]> {
        let bounds0 = bounds_of_droplet_for_axis(cubes, 0);
        let bounds1 = bounds_of_droplet_for_axis(cubes, 1);
        let bounds2 = bounds_of_droplet_for_axis(cubes, 2);
        match (bounds0, bounds1, bounds2) {
            (Some(b0), Some(b1), Some(b2)) => Some([b0, b1, b2]),
            (None, None, None) => None,
            _ => unreachable!(),
        }
    }

    fn retain_greatest<'a>(
        accumulator: Option<&'a Cube>,
        cube: &'a Cube,
        axis: usize,
    ) -> Option<&'a Cube> {
        if let Some(greatest) = accumulator {
            if greatest.n[axis] < cube.n[axis] {
                Some(cube)
            } else {
                Some(greatest)
            }
        } else {
            Some(cube)
        }
    }

    fn grow_by(bounds: [Range<i32>; 3], n: i32) -> [Range<i32>; 3] {
        [
            (bounds[0].start - n)..(bounds[0].end + n),
            (bounds[1].start - n)..(bounds[1].end + n),
            (bounds[2].start - n)..(bounds[2].end + n),
        ]
    }

    fn flood_fill_impl<F, V, I>(
        todo: &mut Vec<Cube>,
        neighbours: F,
        mut visitor: V,
        seen: &mut HashSet<Cube>,
    ) where
        F: Fn(&Cube) -> I,
        V: FnMut(&Cube),
        I: Iterator<Item = Cube>,
    {
        println!("flood_fill_impl: {} items to work on: {todo:?}", todo.len());
        while let Some(cube) = todo.pop() {
            //println!("considering cube {cube}");
            //println!("calling visitor callback for {cube}");
            visitor(&cube);

            for neighbour in neighbours(&cube) {
                if seen.contains(&neighbour) {
                    //println!("we saw neighcour {neighbour} already");
                } else {
                    //println!("remembering to visit neighbour {neighbour} later");
                    todo.push(neighbour);
                }
            }
            seen.insert(cube);
        }
    }

    fn flood_fill<F, V, I>(here: &Cube, neighbours: F, visitor: V, seen: &mut HashSet<Cube>)
    where
        F: Fn(&Cube) -> I,
        V: FnMut(&Cube),
        I: Iterator<Item = Cube>,
    {
        flood_fill_impl(&mut vec![here.clone()], neighbours, visitor, seen);
    }

    fn externally_accessible_area(cubes: &[Cube]) -> usize {
        let droplet_bounds: [Range<i32>; 3] = match bounds_of_droplet(cubes) {
            None => {
                return 0;
            }
            Some(bounds) => bounds,
        };
        let bounding_box = grow_by(droplet_bounds, 1);
        // start is a location which is not inside the droplet, but
        // adjoins it.  Note that it must adjoin the droplet itself, not
        // the bounding box.  We choose an outlier in the +X direction
        let start = match cubes
            .iter()
            .fold(None, |acc, cube| retain_greatest(acc, cube, 0))
        {
            None => {
                return 0;
            }
            Some(cube) => {
                let result = Cube {
                    n: [cube.n[0] + 1, cube.n[1], cube.n[2]],
                };
                println!("cube {cube} is inside the droplet, so using {result} as the start point");
                result
            }
        };
        let mut visited: HashSet<Cube> = HashSet::new();
        let inbounds = |c: &Cube| -> bool {
            c.n.iter()
                .enumerate()
                .all(|(axis, pos)| bounding_box[axis].contains(pos))
        };
        let droplet: HashSet<Cube> = cubes.iter().cloned().collect();
        let neighbours = |cube: &Cube| {
            assert!(!droplet.contains(cube));
            cube.all_possible_neighbours()
                .into_iter()
                .filter(|c: &Cube| inbounds(c))
                .filter(|c: &Cube| !droplet.contains(c))
        };
        let mut area: HashSet<(Cube, char)> = HashSet::new();
        let visit = |c: &Cube| {
            //println!("visiting {c} (which is outside the droplet)");
            assert!(!droplet.contains(c));
            for n in c.all_possible_neighbours() {
                if droplet.contains(&n) {
                    let arrow: Vec<char> = c.n.iter().zip(n.n.iter()).enumerate()
                    .filter_map(|(axis, (cpos, npos))| {
			if cpos == npos {
			    None
			} else {
			    let delta = (cpos - npos).signum();
			    let ch = match (axis, delta) {
				(0, 1) => 'X',
				(0, -1) => 'x',
				(1, 1) => 'Y',
				(1, -1) => 'y',
				(2, 1) => 'Z',
				(2, -1) => 'z',
				_ => {panic!("arrow computation failed: axis={axis} delta={delta} cpos={cpos} npos={npos}");}
			    };
			    Some(ch)
			}
		    })
                    .collect();
                    println!("the neighbour {n} of {c} is inside the droplet so contributes one face ({arrow:?}) to the area");
                    match arrow.as_slice() {
                        [only] => {
                            area.insert((c.clone(), *only));
                        }
                        [] => {
                            panic!("cannot determine direction of face!");
                        }
                        _ => {
                            panic!("ambiguous direction of face!");
                        }
                    }
                } else {
                    //println!("the neighbour {n} of {c} is not inside the droplet, so does not contribute to the area");
                }
            }
        };
        flood_fill(&start, neighbours, visit, &mut visited);
        area.len()
    }

    #[test]
    fn test_externally_accessible_area_simple_cases() {
        assert_eq!(externally_accessible_area(&[]), 0);
        assert_eq!(externally_accessible_area(&[cube(1, 1, 1)]), 6);
        assert_eq!(
            externally_accessible_area(&[cube(1, 1, 1), cube(2, 1, 1),]),
            10
        );
        assert_eq!(
            externally_accessible_area(&[cube(1, 1, 1), cube(2, 1, 1), cube(3, 1, 1)]),
            14
        );
        assert_eq!(
            externally_accessible_area(&[cube(1, 1, 1), cube(2, 1, 1), cube(3, 1, 1)]),
            14
        );
    }

    #[cfg(test)]
    fn make_solid_cube() -> Vec<Cube> {
        let mut the_cube: Vec<Cube> = Vec::with_capacity(27);
        for x in -1..=1 {
            for y in -1..=1 {
                for z in -1..=1 {
                    the_cube.push(cube(x, y, z));
                }
            }
        }
        the_cube
    }

    #[cfg(test)]
    fn make_hollow_cube() -> Vec<Cube> {
        let mut result = make_solid_cube();
        result.retain(|c| c != &cube(0, 0, 0));
        result
    }

    #[test]
    fn test_externally_accessible_area_hollow_cube() {
        let hollow_cube = make_hollow_cube();
        assert_eq!(externally_accessible_area(&hollow_cube), 6 * 9);

        let mut solid_cube = make_solid_cube();
        assert_eq!(externally_accessible_area(&solid_cube), 6 * 9); // unchanged.

        // snip a corner off.
        solid_cube.retain(|c| c != &cube(1, 1, 1));
        assert_eq!(externally_accessible_area(&solid_cube), 6 * 9); // still unchanged.

        // Slice off the far Z-axis end.
        let mut cube = make_solid_cube();
        cube.retain(|c| c.n[2] != 1);
        assert_eq!(externally_accessible_area(&cube), 2 * 9 + 4 * 6);
    }

    pub fn solve_part2(s: &str) -> usize {
        let cubes = parse_input(s).expect("input should be valid");
        externally_accessible_area(&cubes)
    }

    #[test]
    fn test_externally_accessible_area_part2_example_smaller() {
        let mut example_cubes = example();
        example_cubes.retain(|c| c.n[0] > 2);
        let area = externally_accessible_area(&example_cubes);
        assert!(area % 2 == 0, "area is {area}");
    }

    #[test]
    fn test_externally_accessible_area_part2_example() {
        let example_cubes = example();
        assert_eq!(externally_accessible_area(&example_cubes), 58);
    }
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    println!("Day 18 part 1: {}", part1::solve_part1(input));
    println!("Day 18 part 2: {}", part2::solve_part2(input));
}
