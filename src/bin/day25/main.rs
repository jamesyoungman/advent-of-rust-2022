use std::str;

use lib::error::Fail;

const SNAFU_BASE: i64 = 5;

fn snafu_digit_value(ch: char) -> Result<i64, Fail> {
    match ch {
        '2' => Ok(2),
        '1' => Ok(1),
        '0' => Ok(0),
        '-' => Ok(-1),
        '=' => Ok(-2),
        _ => Err(Fail(format!("{ch} is not a valid SNAFU digit"))),
    }
}

fn snafu_digit(n: i64) -> Result<char, Fail> {
    match n {
        2 => Ok('2'),
        1 => Ok('1'),
        0 => Ok('0'),
        -1 => Ok('-'),
        -2 => Ok('='),
        _ => Err(Fail(format!(
            "the number {n} does not correspond to a SNAFU digit"
        ))),
    }
}

fn try_from_snafu(s: &str) -> Result<i64, Fail> {
    s.chars()
        .try_fold(0, |acc: i64, ch: char| -> Result<i64, Fail> {
            Ok((acc * SNAFU_BASE) + snafu_digit_value(ch)?)
        })
}

fn i64_to_snafu(mut n: i64) -> Result<String, Fail> {
    let mut reversed_result: String = String::with_capacity(10);
    while n != 0 {
        let digitvalue = match n % SNAFU_BASE {
            4 => -1,
            3 => -2,
            2 => 2,
            1 => 1,
            0 => 0,
            other => {
                return Err(Fail(format!("no SNAFU digit for {other}")));
            }
        };
        reversed_result.push(snafu_digit(digitvalue)?);
        n -= digitvalue;
        n /= SNAFU_BASE;
    }
    if reversed_result.is_empty() {
        Ok("0".to_string())
    } else {
        Ok(reversed_result.chars().rev().collect())
    }
}

#[test]
fn test_try_from_snafu_brochure() {
    for (decimal, snafu) in [
        (1, "1"),
        (2, "2"),
        (3, "1="),
        (4, "1-"),
        (5, "10"),
        (6, "11"),
        (7, "12"),
        (8, "2="),
        (9, "2-"),
        (10, "20"),
        (15, "1=0"),
        (20, "1-0"),
        (2022, "1=11-2"),
        (12345, "1-0---0"),
        (314159265, "1121-1110-1=0"),
    ] {
        assert_eq!(
            try_from_snafu(snafu),
            Ok(decimal),
            "{snafu} should convert to decimal {decimal}"
        );
        assert_eq!(
            i64_to_snafu(decimal).expect("should convert OK").as_str(),
            snafu,
            "{decimal} should convert to SNAFU {snafu}"
        );
    }
}

#[test]
fn test_try_from_snafu_example() {
    for (snafu, decimal) in [
        ("1=-0-2", 1747),
        ("12111", 906),
        ("2=0=", 198),
        ("21", 11),
        ("2=01", 201),
        ("111", 31),
        ("20012", 1257),
        ("112", 32),
        ("1=-1=", 353),
        ("1-12", 107),
        ("12", 7),
        ("1=", 3),
        ("122", 37),
    ] {
        assert_eq!(try_from_snafu(snafu), Ok(decimal));
    }
}

#[cfg(test)]
fn example() -> &'static str {
    concat!(
        "1=-0-2\n", "12111\n", "2=0=\n", "21\n", "2=01\n", "111\n", "20012\n", "112\n", "1=-1=\n",
        "1-12\n", "12\n", "1=\n", "122\n"
    )
}

fn add_snafu_numbers(s: &str) -> Result<i64, Fail> {
    s.split_terminator('\n')
        .try_fold(0, |total, snafu| Ok(total + try_from_snafu(snafu)?))
}

#[test]
fn test_total_example() {
    assert_eq!(add_snafu_numbers(example()), Ok(4890));
}

fn solve_part1(s: &str) -> Result<String, Fail> {
    add_snafu_numbers(s).and_then(i64_to_snafu)
}

#[test]
fn test_solve_part1() {
    assert_eq!(solve_part1(example()), Ok("2=-1=0".to_string()));
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    println!(
        "Day 25 part 1: {}",
        solve_part1(input).expect("should be able to solve part 1")
    );
}
