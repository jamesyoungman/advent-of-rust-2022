use std::str;

fn most_calories(input: &str) -> Option<u64> {
    let mut most: Option<u64> = None;
    let mut current: u64 = 0;

    let mut endgroup = |curr: &mut u64| {
        println!("endgroup: curr={curr}, most={most:?}");
        match most {
            Some(n) if n > *curr => (),
            _ => {
                most = Some(*curr);
            }
        }
        *curr = 0;
    };
    for line in input.split('\n') {
        match line {
            "" => endgroup(&mut current),
            nonempty => {
                let value: u64 = match nonempty.parse() {
                    Ok(n) => n,
                    Err(e) => {
                        panic!("non-number {nonempty} in input: {e}");
                    }
                };
                current += value;
            }
        }
    }
    endgroup(&mut current);
    most
}

#[test]
fn test_example() {
    let example = include_bytes!("example.txt");
    assert_eq!(
        most_calories(&str::from_utf8(example).unwrap()),
        Some(24000)
    )
}

fn main() {
    let input = include_bytes!("input.txt");
    println!(
        "Day 01 part 1: {}",
        most_calories(str::from_utf8(input).unwrap()).unwrap()
    );
}
