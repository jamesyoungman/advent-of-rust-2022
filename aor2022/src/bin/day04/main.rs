use lib::error::Fail;
use std::str;

#[derive(Debug)]
struct Assignment {
    first: u32,
    last: u32,
}

impl Assignment {
    fn fully_contains(&self, other: &Assignment) -> bool {
        self.first <= other.first && self.last >= other.last
    }
}

#[test]
fn test_assignment_fully_contains() {
    assert_eq!(
        true,
        Assignment { first: 1, last: 6 }.fully_contains(&Assignment { first: 3, last: 4 })
    );
    assert_eq!(
        true,
        Assignment { first: 1, last: 6 }.fully_contains(&Assignment { first: 3, last: 6 })
    );
    assert_eq!(
        false,
        Assignment { first: 1, last: 6 }.fully_contains(&Assignment { first: 3, last: 7 })
    );
}

impl TryFrom<&str> for Assignment {
    type Error = Fail;

    fn try_from(s: &str) -> Result<Assignment, Fail> {
        match s.split_once('-') {
            Some((left, right)) => match (left.parse(), right.parse()) {
                (Err(e), _) | (_, Err(e)) => Err(Fail(format!(
                    "left and right parts of assignment should be numbers: {e}"
                ))),
                (Ok(first), Ok(last)) => {
                    if first <= last {
                        Ok(Assignment { first, last })
                    } else {
                        Err(Fail(format!(
                            "left and right parts of assignment are out of order: {s}"
                        )))
                    }
                }
            },
            None => Err(Fail("assignment contains no '-'".to_string())),
        }
    }
}

#[test]
fn test_assignment_try_from() {
    match Assignment::try_from("3-8").expect("should convert successfully") {
        Assignment { first: 3, last: 8 } => (),
        other => {
            panic!("expected 3-8, got {other:?}");
        }
    }
    assert!(Assignment::try_from("3-Q").is_err());
    assert!(Assignment::try_from("i-3").is_err());
    assert!(Assignment::try_from("39").is_err());
    assert!(Assignment::try_from("9-3").is_err());
}

#[derive(Debug)]
struct PairAssignment {
    first: Assignment,
    second: Assignment,
}

impl PairAssignment {
    fn is_fully_overlapping(&self) -> bool {
        self.first.fully_contains(&self.second) || self.second.fully_contains(&self.first)
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
    match PairAssignment::try_from("3-8,9-12").expect("should convert successfully") {
        PairAssignment {
            first: Assignment { first: 3, last: 8 },
            second: Assignment { first: 9, last: 12 },
        } => (),
        other => {
            panic!("expected 3-8,9-12, got {other:?}");
        }
    }
}

fn solve_part1(text: &str) -> usize {
    text.split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| PairAssignment::try_from(line).expect("line should be valid"))
        .filter(|pair| pair.is_fully_overlapping())
        .count()
}

fn main() {
    let text = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("Day 04 part 1: {}", solve_part1(text));
}
