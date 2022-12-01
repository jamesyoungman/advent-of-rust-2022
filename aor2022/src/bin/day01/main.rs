use lib::error::Fail;
use std::str;

fn ordered_calories(input: &[Option<u64>]) -> Vec<u64> {
    let mut result: Vec<u64> = Vec::new();
    let mut current: u64 = 0;

    let mut endgroup = |curr: &mut u64| {
        result.push(*curr);
        *curr = 0;
    };
    for line in input {
        match line {
            None => endgroup(&mut current),
            Some(value) => {
                current += value;
            }
        }
    }
    endgroup(&mut current);
    result.sort();
    result.into_iter().rev().collect()
}

fn most_calories(reverse_ordered: &[u64]) -> Option<u64> {
    reverse_ordered.iter().copied().next()
}

fn top3_calories(reverse_ordered: &[u64]) -> u64 {
    reverse_ordered.iter().take(3).sum()
}

#[test]
fn test_example() {
    let text = str::from_utf8(include_bytes!("example.txt")).unwrap();
    let input: Vec<Option<u64>> = parse_input(text).expect("input should be valid");
    let ordered = ordered_calories(&input);
    assert_eq!(most_calories(&ordered), Some(24000));
    assert_eq!(top3_calories(&ordered), 45000);
}

fn parse_input(input: &str) -> Result<Vec<Option<u64>>, Fail> {
    let mut result = Vec::new();
    for line in input.split('\n') {
        match line {
            "" => {
                result.push(None);
            }
            nonempty => match nonempty.parse() {
                Ok(n) => {
                    result.push(Some(n));
                }
                Err(e) => {
                    return Err(Fail(format!("non-number {nonempty} in input: {e}")));
                }
            },
        }
    }
    Ok(result)
}

fn main() {
    let text = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let input: Vec<Option<u64>> = parse_input(text).expect("input should be valid");
    let ordered = ordered_calories(&input);
    println!("Day 01 part 1: {}", most_calories(&ordered).unwrap());
    println!("Day 01 part 2: {}", top3_calories(&ordered));
}
