use std::{
    cmp::Ordering,
    fmt::{Display, Formatter},
    str,
};

use lib::error::Fail;

#[derive(Debug)]
enum Value {
    Integer(u32),
    List(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Integer(n) => n.fmt(f),
            Value::List(items) => {
                f.write_str("[")?;
                for (i, item) in items.iter().enumerate() {
                    if i != 0 {
                        f.write_str(",")?;
                    }
                    item.fmt(f)?;
                }
                f.write_str("]")
            }
        }
    }
}

#[test]
fn test_display() {
    assert_eq!(format!("{}", Value::Integer(2)), "2");
    assert_eq!(format!("{}", Value::List(vec![])), "[]");
    assert_eq!(format!("{}", Value::List(vec![Value::Integer(3)])), "[3]");
    assert_eq!(
        format!(
            "{}",
            Value::List(vec![Value::Integer(3), Value::Integer(12),])
        ),
        "[3,12]"
    );
    assert_eq!(
        format!(
            "{}",
            Value::List(vec![
                Value::Integer(3),
                Value::List(vec![Value::Integer(21), Value::Integer(6),]),
            ])
        ),
        "[3,[21,6]]"
    );
}

fn parse_integer(s: &str) -> Result<(u32, &str), Fail> {
    let mut value: Option<u32> = None;
    let consume_decimal_digit = |ch: char| -> bool {
        match ch.to_digit(10) {
            Some(d) => {
                value = Some(value.unwrap_or(0) * 10 + d);
                true
            }
            None => false,
        }
    };
    let tail = s.trim_start_matches(consume_decimal_digit);
    match value {
        Some(n) => Ok((n, tail)),
        None => Err(Fail(format!("'{s}' is not an integer"))),
    }
}

#[test]
fn test_parse_integer() {
    assert_eq!(parse_integer("0"), Ok((0, "")));
    assert_eq!(parse_integer("1"), Ok((1, "")));
    assert_eq!(parse_integer("2"), Ok((2, "")));
    assert_eq!(parse_integer("3"), Ok((3, "")));
    assert_eq!(parse_integer("4"), Ok((4, "")));
    assert_eq!(parse_integer("5"), Ok((5, "")));
    assert_eq!(parse_integer("6"), Ok((6, "")));
    assert_eq!(parse_integer("7"), Ok((7, "")));
    assert_eq!(parse_integer("8"), Ok((8, "")));
    assert_eq!(parse_integer("9"), Ok((9, "")));
    assert_eq!(parse_integer("100"), Ok((100, "")));
    assert_eq!(parse_integer("101"), Ok((101, "")));
    assert_eq!(parse_integer("102"), Ok((102, "")));
    assert_eq!(parse_integer("103"), Ok((103, "")));
    assert_eq!(parse_integer("104"), Ok((104, "")));
    assert_eq!(parse_integer("105"), Ok((105, "")));
    assert_eq!(parse_integer("106"), Ok((106, "")));
    assert_eq!(parse_integer("107"), Ok((107, "")));
    assert_eq!(parse_integer("108"), Ok((108, "")));
    assert_eq!(parse_integer("109"), Ok((109, "")));
    assert_eq!(parse_integer("00"), Ok((0, "")));
    assert_eq!(parse_integer("07"), Ok((7, "")));
    assert_eq!(parse_integer("08"), Ok((8, "")));
    assert!(parse_integer("").is_err());
    assert!(parse_integer("[]").is_err());
    assert!(parse_integer("]").is_err());
    assert!(parse_integer("[").is_err());
    assert!(parse_integer("x").is_err());
}

fn parse_list_body(mut s: &str) -> Result<(Vec<Value>, &str), Fail> {
    // parse zero or more values separated by commas.
    // example: 1,2,3
    // example: [],[],[]
    let mut result = vec![];
    loop {
        match s.strip_prefix(']') {
            Some(_) => {
                return Ok((result, s));
            }
            None => {
                let (val, rest) = parse_value(s)?;
                result.push(val);
                s = rest;
                if let Some(tail) = s.strip_prefix(',') {
                    s = tail; // loop again
                }
            }
        }
    }
}

#[test]
fn test_parse_list_body() {
    assert_eq!(parse_list_body("]"), Ok((vec![], "]")));
    assert_eq!(parse_list_body("2]"), Ok((vec![Value::Integer(2)], "]")));
    assert_eq!(parse_list_body("[]]"), Ok((vec![Value::List(vec![])], "]")));
    assert_eq!(
        parse_list_body("[[]]]"),
        Ok((vec![Value::List(vec![Value::List(vec![])])], "]"))
    );
}

/// Parse a value from s, and return the parsed value and the
/// unconsumed input.
fn parse_value(s: &str) -> Result<(Value, &str), Fail> {
    match s.strip_prefix('[') {
        None => {
            // The value is an integer.
            match parse_integer(s) {
                Ok((n, tail)) => Ok((Value::Integer(n), tail)),
                Err(_) => Err(Fail(format!("not an integer or a list: '{s}'"))),
            }
        }
        Some(tail) => {
            match parse_list_body(tail) {
                Ok((items, t)) => {
                    // check for ]
                    match t.strip_prefix(']') {
                        Some(tt) => Ok((Value::List(items), tt)),
                        None => Err(Fail(format!("unbalanced brackets ({tail}): {s}"))),
                    }
                }
                Err(e) => Err(e),
            }
        }
    }
}

#[test]
fn test_parse_value() {
    assert!(parse_value("2").is_ok());
    assert_eq!(parse_value("[]"), Ok((Value::List(vec![]), "")));
    assert_eq!(
        parse_value("[2]"),
        Ok((Value::List(vec![Value::Integer(2)]), ""))
    );
}

#[test]
fn test_round_trip() {
    for original in [
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "80",
        "81",
        "82",
        "83",
        "84",
        "85",
        "86",
        "87",
        "88",
        "89",
        "[]",
        "[2]",
        "[1,3]",
        "[6,[12]]",
        "[[],[],[]]",
        "[12,[6]]",
        "[6,[1,8,[]]]",
    ] {
        match Value::try_from(original) {
            Ok(result) => {
                let formatted = format!("{}", result);
                assert_eq!(original, &formatted);
            }
            Err(e) => {
                panic!("failed to parse {original}: {e}");
            }
        }
    }
}

impl TryFrom<&str> for Value {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Value, Fail> {
        match parse_value(s) {
            Ok((result, tail)) if tail.is_empty() => Ok(result),
            Ok((_, tail)) => Err(Fail(format!("unconsumed tail: {tail}"))),
            Err(e) => Err(e),
        }
    }
}

fn listify(n: u32) -> Value {
    Value::List(vec![Value::Integer(n)])
}

fn compare_items(left: &[Value], right: &[Value]) -> Ordering {
    match (left, right) {
        ([l, ls @ ..], [r, rs @ ..]) => match compare_values(l, r) {
            Ordering::Equal => compare_items(ls, rs),
            other => other,
        },
        ([], []) => Ordering::Equal,
        ([], _) => Ordering::Less,
        ([_nonempty, ..], []) => Ordering::Greater,
    }
}

fn compare_values(left: &Value, right: &Value) -> Ordering {
    match (left, right) {
        (Value::Integer(a), Value::Integer(b)) => a.cmp(b),
        (Value::List(left_items), Value::List(right_items)) => {
            compare_items(left_items, right_items)
        }
        (Value::List(_), Value::Integer(n)) => compare_values(left, &listify(*n)),
        (Value::Integer(n), Value::List(_)) => compare_values(&listify(*n), right),
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(compare_values(self, other))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        matches!(compare_values(self, other), Ordering::Equal)
    }
}

impl Eq for Value {}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        compare_values(self, other)
    }
}

#[test]
fn test_values_in_order() {
    for (left, right, expect_ordering) in [
        ("0", "0", Ordering::Equal),
        ("1", "1", Ordering::Equal),
        ("7", "8", Ordering::Less),
        ("[]", "[]", Ordering::Equal),
        ("[1]", "[1]", Ordering::Equal),
        ("[4]", "[5]", Ordering::Less),
        ("[2,3,4]", "[4]", Ordering::Less),
        ("[2,3,4]", "4", Ordering::Less),
    ] {
        let lv = Value::try_from(left).expect("left item should be valid");
        let rv = Value::try_from(right).expect("right item should be valid");
        let got: Ordering = lv.cmp(&rv);
        assert_eq!(
            got, expect_ordering,
            "values_in_order(\"{left}\", \"{right}\") returned {got:?}, expected {expect_ordering:?}"
        );
    }
}

#[cfg(test)]
fn example() -> &'static str {
    concat!(
        "[1,1,3,1,1]\n",
        "[1,1,5,1,1]\n",
        "\n",
        "[[1],[2,3,4]]\n",
        "[[1],4]\n",
        "\n",
        "[9]\n",
        "[[8,7,6]]\n",
        "\n",
        "[[4,4],4,4]\n",
        "[[4,4],4,4,4]\n",
        "\n",
        "[7,7,7,7]\n",
        "[7,7,7]\n",
        "\n",
        "[]\n",
        "[3]\n",
        "\n",
        "[[[]]]\n",
        "[[]]\n",
        "\n",
        "[1,[2,[3,[4,[5,6,7]]]],8,9]\n",
        "[1,[2,[3,[4,[5,6,0]]]],8,9]\n",
        "\n",
    )
}

fn parse_input_for_part1(s: &str) -> Result<Vec<(Value, Value)>, Fail> {
    let mut result = Vec::new();
    for chunk in s.split("\n\n").filter(|chunk| !chunk.is_empty()) {
        let items: Vec<&str> = chunk.split('\n').filter(|item| !item.is_empty()).collect();
        match items.as_slice() {
            [first, second] => {
                let a: Value = Value::try_from(*first)?;
                let b: Value = Value::try_from(*second)?;
                result.push((a, b));
            }
            x => {
                return Err(Fail(format!(
                    "chunk '{chunk}' does not contain two items, instead {}",
                    x.len()
                )));
            }
        }
    }
    Ok(result)
}

fn parse_input_for_part2(s: &str) -> Result<Vec<Value>, Fail> {
    s.split('\n')
        .filter(|chunk| !chunk.is_empty())
        .map(Value::try_from)
        .collect()
}

fn solve_part1(s: &str) -> Result<usize, Fail> {
    Ok(parse_input_for_part1(s)?
        .iter()
        .enumerate()
        .filter_map(|(i, (a, b))| if a < b { Some(i + 1) } else { None })
        .sum())
}

#[test]
fn test_solve_part1() {
    assert_eq!(solve_part1(example()), Ok(13));
}

#[test]
fn test_parse_input_for_part1() {
    let items = parse_input_for_part1(example()).expect("example should be valid");
    assert_eq!(8, items.len());
}

#[test]
fn test_parse_input_for_part2() {
    let items = parse_input_for_part2(example()).expect("example should be valid");
    assert_eq!(16, items.len());
}

fn solve_part2(s: &str) -> Result<usize, Fail> {
    let input_with_dividers = format!("{}\n[[2]]\n[[6]]\n", s);
    let mut items = parse_input_for_part2(&input_with_dividers).expect("example should be valid");
    items.sort();
    let start = Value::try_from("[[2]]")?;
    let end = Value::try_from("[[6]]")?;
    Ok(items
        .iter()
        .enumerate()
        .filter_map(|(i, item)| {
            if item == &start || item == &end {
                Some(i + 1)
            } else {
                None
            }
        })
        .product())
}

#[test]
fn test_solve_part2() {
    assert_eq!(solve_part2(example()), Ok(140));
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    println!(
        "Day 13 part 1: {}",
        solve_part1(input).expect("failed to solve part 1")
    );
    println!(
        "Day 13 part 2: {}",
        solve_part2(input).expect("failed to solve part 2")
    );
}
