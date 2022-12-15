use sscanf::scanf;
use std::ops::Range;
use std::str;

use gcollections::ops::cardinality::Cardinality;
use gcollections::ops::{set::Difference, Bounded};
use interval::interval::Interval;
use interval::interval_set::*;

use lib::error::Fail;
use lib::grid::{manhattan, Position};

#[derive(Debug, PartialEq, Eq)]
struct Sensor {
    pos: Position,
    closest_beacon: Position,
}

impl TryFrom<&str> for Sensor {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Sensor, Fail> {
        scanf!(
            s,
            "Sensor at x={i64}, y={i64}: closest beacon is at x={i64}, y={i64}"
        )
        .map(|(sx, sy, bx, by)| Sensor {
            pos: Position { x: sx, y: sy },
            closest_beacon: Position { x: bx, y: by },
        })
        .map_err(|_| Fail("incorect line format".to_string()))
    }
}

#[test]
fn test_parse_input_line() {
    assert_eq!(
        Sensor::try_from("Sensor at x=17, y=20: closest beacon is at x=21, y=22"),
        Ok(Sensor {
            pos: Position { x: 17, y: 20 },
            closest_beacon: Position { x: 21, y: 22 }
        })
    );
}

fn parse_input(s: &str) -> Result<Vec<Sensor>, Fail> {
    s.split_terminator('\n')
        .map(Sensor::try_from)
        .collect::<Result<Vec<Sensor>, Fail>>()
}

impl Sensor {
    fn scan_y(&self, y: i64) -> Option<Range<i64>> {
        let m = manhattan(&self.pos, &self.closest_beacon);
        let d = (self.pos.y - y).abs();
        if d > m {
            None
        } else {
            let delta = m - d;
            Some((self.pos.x - delta)..(self.pos.x + delta + 1))
        }
    }
}

#[test]
fn test_scan() {
    let s1 = Sensor {
        pos: Position { x: 8, y: 7 },
        closest_beacon: Position { x: 2, y: 10 },
    };
    assert_eq!(s1.scan_y(16), Some(8..9));
    assert_eq!(s1.scan_y(15), Some(7..10));
    assert_eq!(s1.scan_y(14), Some(6..11));
    assert_eq!(s1.scan_y(7), Some(-1..18));
}

fn scan_all_sensors(y: i64, sensors: &[Sensor]) -> IntervalSet<i64> {
    let mut endpoints: Vec<(i64, i64)> = sensors
        .iter()
        .filter_map(|sensor| sensor.scan_y(y).map(|range| (range.start, range.end - 1)))
        .collect();
    endpoints.sort();
    endpoints.to_interval_set()
}

fn exclude_beacons(segments: &mut IntervalSet<i64>, beacons: &[i64]) {
    beacons.iter().for_each(|x| {
        let goner: IntervalSet<i64> = vec![(*x, *x)].to_interval_set();
        *segments = segments.difference(&goner);
    })
}

fn solve_part1(s: &str, y: i64) -> Result<usize, Fail> {
    let keep_if_on_scanline = |pos: &Position| pos.y == y;
    let sensors: Vec<Sensor> = parse_input(s)?;
    let mut segments: IntervalSet<i64> = scan_all_sensors(y, sensors.as_slice());
    let beacons: Vec<i64> = sensors
        .iter()
        .map(|s| s.closest_beacon)
        .filter(keep_if_on_scanline)
        .map(|pos| pos.x)
        .collect();
    exclude_beacons(&mut segments, &beacons);
    Ok(segments
        .iter()
        .map(|interval: &Interval<i64>| {
            let n: usize = interval.size() as usize;
            n
        })
        .sum())
}

fn part2_locate_missing_beacon(s: &str, limit: i64) -> Result<Position, Fail> {
    let full_width: IntervalSet<i64> = vec![(0, limit)].to_interval_set();
    let sensors: Vec<Sensor> = parse_input(s)?;
    let mut candidates: Vec<Position> = Vec::with_capacity(1);
    for y in 0..=limit {
        let segments: IntervalSet<i64> = scan_all_sensors(y, sensors.as_slice());
        let gap = full_width.difference(&segments);
        candidates.extend(gap.iter().filter_map(|g| {
            let (l, u) = (g.lower(), g.upper());
            if l == u {
                Some(Position { x: u, y })
            } else {
                None
            }
        }))
    }
    match candidates.as_slice() {
        [] => Err(Fail("found no gap".to_string())),
        [only] => Ok(*only),
        _ => Err(Fail(format!(
            "found multiple solutions for part 2: {candidates:?}"
        ))),
    }
}

fn solve_part2(s: &str, limit: i64) -> Result<i64, Fail> {
    const FOUR_MILLION: i64 = 4_000_000;
    part2_locate_missing_beacon(s, limit).map(|pos| pos.x * FOUR_MILLION + pos.y)
}

#[cfg(test)]
fn example() -> &'static str {
    concat!(
        "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n",
        "Sensor at x=9, y=16: closest beacon is at x=10, y=16\n",
        "Sensor at x=13, y=2: closest beacon is at x=15, y=3\n",
        "Sensor at x=12, y=14: closest beacon is at x=10, y=16\n",
        "Sensor at x=10, y=20: closest beacon is at x=10, y=16\n",
        "Sensor at x=14, y=17: closest beacon is at x=10, y=16\n",
        "Sensor at x=8, y=7: closest beacon is at x=2, y=10\n",
        "Sensor at x=2, y=0: closest beacon is at x=2, y=10\n",
        "Sensor at x=0, y=11: closest beacon is at x=2, y=10\n",
        "Sensor at x=20, y=14: closest beacon is at x=25, y=17\n",
        "Sensor at x=17, y=20: closest beacon is at x=21, y=22\n",
        "Sensor at x=16, y=7: closest beacon is at x=15, y=3\n",
        "Sensor at x=14, y=3: closest beacon is at x=15, y=3\n",
        "Sensor at x=20, y=1: closest beacon is at x=15, y=3\n",
    )
}

#[test]
fn test_example_part1() {
    assert_eq!(solve_part1(example(), 9), Ok(25));
    assert_eq!(solve_part1(example(), 10), Ok(26));
    assert_eq!(solve_part1(example(), 11), Ok(28));
}

#[test]
fn test_example_part2() {
    assert_eq!(solve_part2(example(), 20), Ok(56000011));
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    println!(
        "Day 15 part 1: {}",
        solve_part1(input, 2_000_000).expect("solve part 1")
    );
    println!(
        "Day 15 part 2: {}",
        solve_part2(input, 4_000_000).expect("solve part 2")
    );
}
