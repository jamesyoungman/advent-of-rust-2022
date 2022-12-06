use std::collections::HashSet;
use std::str;

fn all_differ(chars: &[char]) -> bool {
    let mut hs = HashSet::new();
    for ch in chars.iter() {
        if !hs.insert(ch) {
            return false; // duplicate
        }
    }
    dbg!(&chars);
    true
}

#[test]
fn test_all_differ() {
    assert!(all_differ(&['a', 'b', 'c', 'd']));
    assert!(!all_differ(&['a', 'a', 'c', 'd']));
    assert!(!all_differ(&['a', 'b', 'a', 'd']));
    assert!(!all_differ(&['a', 'b', 'c', 'a']));
    assert!(!all_differ(&['a', 'b', 'c', 'c']));
}

fn detect_sop(s: &str) -> Option<usize> {
    let v: Vec<char> = s.chars().collect();
    v.windows(4)
        .enumerate()
        .find(|(_, chars)| all_differ(chars))
        .map(|(pos, chars)| pos + chars.len())
}

#[test]
fn test_detect_sop() {
    assert_eq!(detect_sop("bvwbjplbgvbhsrlpgdmjqwftvncz").unwrap(), 5);
    assert_eq!(detect_sop("nppdvjthqldpwncqszvftbrmjlhg").unwrap(), 6);
    assert_eq!(detect_sop("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg").unwrap(), 10);
    assert_eq!(detect_sop("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw").unwrap(), 11);
}

fn main() {
    let text = str::from_utf8(include_bytes!("input.txt")).expect("valid input file");
    println!(
        "Day 06 part 1: {}",
        detect_sop(text).expect("puzzle input has no SOP sequence")
    );
}
