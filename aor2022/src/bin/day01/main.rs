use lib::error::Fail;
use lib::iterplus::sum_result;
use std::str;

fn ordered_calories(text: &str) -> Result<Vec<u64>, Fail> {
    let mut unsorted: Vec<u64> = text
        .split("\n\n")
        .map(|chunk: &str| {
            chunk
                .split('\n')
                .filter(|s| !s.is_empty())
                .map(|s: &str| -> Result<u64, Fail> {
                    s.parse::<u64>().map_err(|e| Fail(e.to_string()))
                })
                .fold(Ok(0), sum_result)
        })
        .collect::<Result<Vec<u64>, Fail>>()?;
    unsorted.sort_by(|a: &u64, b: &u64| b.cmp(a));
    Ok(unsorted)
}

#[test]
fn test_ordered_calories() {
    let result = ordered_calories("1\n").expect("should parse");
    assert!(matches!(&result[..], &[1]));

    let result = ordered_calories("1\n2\n").expect("should parse");
    assert!(matches!(&result[..], &[3]));

    let result = ordered_calories("1\n\n2\n").expect("should parse");
    assert!(matches!(&result[..], &[2, 1]));

    let result = ordered_calories("1\n3\n\n2\n").expect("should parse");
    assert!(matches!(&result[..], &[4, 2]));
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
    let ordered = ordered_calories(&text).unwrap();
    assert_eq!(most_calories(&ordered), Some(24000));
    assert_eq!(top3_calories(&ordered), 45000);
}

fn main() {
    let ordered = ordered_calories(str::from_utf8(include_bytes!("input.txt")).unwrap()).unwrap();
    println!("Day 01 part 1: {}", most_calories(&ordered).unwrap());
    println!("Day 01 part 2: {}", top3_calories(&ordered));
}
