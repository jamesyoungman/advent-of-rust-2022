use std::collections::HashMap;
use std::collections::HashSet;
use std::str;

use lib::error::Fail;

const LOWERCASE: &str = "abcdefghijklmnopqrstuvwxyz";
const UPPERCASE: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

struct PriMap {
    mapping: HashMap<char, usize>,
}

impl PriMap {
    fn new() -> PriMap {
        fn build_primap(base: usize, letters: &str, mapping: &mut HashMap<char, usize>) {
            for (pos, ch) in letters.chars().enumerate() {
                mapping.insert(ch, pos + base);
            }
        }
        let mut result = HashMap::new();
        build_primap(1, LOWERCASE, &mut result);
        build_primap(27, UPPERCASE, &mut result);
        PriMap { mapping: result }
    }

    fn lookup(&self, ch: &char) -> Option<usize> {
        match self.mapping.get(ch) {
            Some(n) => Some(*n),
            None => None,
        }
    }
}

fn in_both(s: &str) -> Option<char> {
    let halflen = s.len() / 2;
    let mut in_first_half: HashSet<char> = HashSet::new();
    for (pos, ch) in s.chars().enumerate() {
        if (pos + 1) <= halflen {
            // item is in the first half
            in_first_half.insert(ch);
        } else {
            if in_first_half.contains(&ch) {
                return Some(ch);
            }
        }
    }
    None
}

#[test]
fn test_in_both() {
    assert_eq!(in_both(""), None);
    assert_eq!(in_both("ab"), None);
    assert_eq!(in_both("aa"), Some('a'));
    assert_eq!(in_both("vJrwpWtwJgWrhcsFMMfFFhFp"), Some('p'));
}

fn solve_part1(s: &str) -> usize {
    let pm = PriMap::new();
    s.split('\n')
        .filter(|line| !line.is_empty())
        .map(|line| match in_both(line) {
            Some(ch) => ch,
            None => {
                panic!("a char must appear in both sides: {line}");
            }
        })
        .map(|ch| pm.lookup(&ch).expect("not a letter"))
        .sum()
}

#[test]
fn test_solve_part1() {
    let text = str::from_utf8(include_bytes!("example.txt")).unwrap();
    assert_eq!(solve_part1(text), 157);
}

fn main() {
    let text = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("Day 03 part 1: {}", solve_part1(text));
}
