use lib::error::Fail;

use std::ops::RangeInclusive;
use std::str;

/// Assignment is a range of u32 which is never empty.
#[derive(Debug, Eq, PartialEq)]
struct Assignment(RangeInclusive<u32>);

impl Assignment {
    fn fully_contains(&self, other: &Assignment) -> bool {
        self.0.contains(other.0.start()) && self.0.contains(other.0.end())
    }

    fn partly_contains(&self, other: &Assignment) -> bool {
        self.0.contains(other.0.start()) || self.0.contains(other.0.end())
    }

    fn new(start: u32, end: u32) -> Result<Assignment, Fail> {
        let result = start..=end;
        if result.is_empty() {
            Err(Fail(format!(
                "left and right parts of assignment are out of order: {start},{end}"
            )))
        } else {
            Ok(Assignment(result))
        }
    }
}

#[test]
fn test_assignment_fully_contains() {
    assert!(Assignment::new(1, 6)
        .unwrap()
        .fully_contains(&Assignment::new(3, 4).unwrap()));
    assert!(Assignment::new(1, 6)
        .unwrap()
        .fully_contains(&Assignment::new(3, 6).unwrap()));
    assert!(!Assignment::new(1, 6)
        .unwrap()
        .fully_contains(&Assignment::new(3, 7).unwrap()));
}

#[test]
fn test_assignment_partly_contains() {
    // if fully contains, then also partly contains
    assert!(Assignment::new(1, 6)
        .unwrap()
        .partly_contains(&Assignment::new(3, 4).unwrap()));
    assert!(Assignment::new(1, 6)
        .unwrap()
        .partly_contains(&Assignment::new(3, 6).unwrap()));
    assert!(Assignment::new(1, 6)
        .unwrap()
        .partly_contains(&Assignment::new(6, 6).unwrap()));

    // disjoint
    assert!(!Assignment::new(1, 6)
        .unwrap()
        .partly_contains(&Assignment::new(8, 12).unwrap()));

    // partial cases
    assert!(Assignment::new(1, 6)
        .unwrap()
        .partly_contains(&Assignment::new(3, 7).unwrap()));
    assert!(Assignment::new(3, 6)
        .unwrap()
        .partly_contains(&Assignment::new(1, 4).unwrap()));
}

impl TryFrom<&str> for Assignment {
    type Error = Fail;

    fn try_from(s: &str) -> Result<Assignment, Fail> {
        match s.split_once('-') {
            Some((left, right)) => match (left.parse(), right.parse()) {
                (Err(e), _) | (_, Err(e)) => Err(Fail(format!(
                    "left and right parts of assignment should be numbers: {e}"
                ))),
                (Ok(first), Ok(last)) => Assignment::new(first, last),
            },
            None => Err(Fail("assignment contains no '-'".to_string())),
        }
    }
}

#[test]
fn test_assignment_try_from() {
    let result = Assignment::try_from("3-8").expect("should convert successfully");
    assert_eq!(result, Assignment::new(3, 8).unwrap());

    assert!(Assignment::try_from("3-Q").is_err());
    assert!(Assignment::try_from("3-8x").is_err());
    assert!(Assignment::try_from("3-8-").is_err());
    assert!(Assignment::try_from("3--8").is_err());
    assert!(Assignment::try_from("8-").is_err());
    assert!(Assignment::try_from("-8").is_err());
    assert!(Assignment::try_from("i-3").is_err());
    assert!(Assignment::try_from("39").is_err());
    assert!(Assignment::try_from("9-3").is_err());
}

#[derive(Debug, PartialEq, Eq)]
struct PairAssignment {
    first: Assignment,
    second: Assignment,
}

impl PairAssignment {
    fn is_fully_overlapping(&self) -> bool {
        self.first.fully_contains(&self.second) || self.second.fully_contains(&self.first)
    }
    fn is_partly_overlapping(&self) -> bool {
        self.first.partly_contains(&self.second) || self.second.partly_contains(&self.first)
    }
}

impl TryFrom<&str> for PairAssignment {
    type Error = Fail;

    fn try_from(s: &str) -> Result<PairAssignment, Fail> {
        match s.split_once(',') {
            Some((first, second)) => {
                match (Assignment::try_from(first), Assignment::try_from(second)) {
                    (Ok(first), Ok(second)) => Ok(PairAssignment { first, second }),
                    (Err(e), _) | (_, Err(e)) => Err(e),
                }
            }
            None => Err(Fail(format!("pair assignment should contain a comma: {s}"))),
        }
    }
}

#[test]
fn test_pair_assignment_try_from() {
    let result = PairAssignment::try_from("3-8,9-12").expect("should convert successfully");
    assert_eq!(
        result,
        PairAssignment {
            first: Assignment::new(3, 8).unwrap(),
            second: Assignment::new(9, 12).unwrap(),
        }
    );
    assert!(PairAssignment::try_from("3-8").is_err());
}

fn get_pairs(text: &str) -> Vec<PairAssignment> {
    text.split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| PairAssignment::try_from(line).expect("line should be valid"))
        .filter(|pair| pair.is_partly_overlapping())
        .collect()
}

fn main() {
    let pairs: Vec<PairAssignment> =
        get_pairs(str::from_utf8(include_bytes!("input.txt")).expect("input should be valid"));
    println!(
        "Day 04 part 1: {}",
        pairs
            .iter()
            .filter(|pair| pair.is_fully_overlapping())
            .count()
    );
    println!("Day 04 part 2: {}", pairs.len());
}
