use lib::error::Fail;
use std::str;

#[derive(Copy, Clone, PartialEq, Eq)]
enum Play {
    Rock,
    Paper,
    Scissors,
}
use Play::*;

impl Play {
    fn score(&self) -> usize {
        match self {
            Rock => 1,
            Paper => 2,
            Scissors => 3,
        }
    }
}

impl TryFrom<char> for Play {
    type Error = Fail;
    fn try_from(ch: char) -> Result<Play, Fail> {
        match ch {
            'A' | 'X' => Ok(Play::Rock),
            'B' | 'Y' => Ok(Play::Paper),
            'C' | 'Z' => Ok(Play::Scissors),
            _ => Err(Fail(format!("wanted ABCXYX, got {ch}"))),
        }
    }
}

fn score_play(them: char, me: char) -> Result<usize, Fail> {
    let me = me.try_into()?;
    let them = them.try_into()?;
    let outcome_score = match (me, them) {
        (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => 3, // draw
        (Rock, Scissors) | (Scissors, Paper) | (Paper, Rock) => 6, // I win
        (Scissors, Rock) | (Paper, Scissors) | (Rock, Paper) => 0, // I lose
    };
    Ok(outcome_score + me.score())
}

fn score_line(s: &str) -> Result<usize, Fail> {
    let mut it = s.chars();
    let first = it.next();
    let second = it.next();
    let third = it.next();
    let fourth = it.next();
    match (first, second, third, fourth) {
        (_, _, _, Some(_)) => Err(Fail(format!("line {s} too long"))),
        (Some(them), Some(' '), Some(me), None) => score_play(
            them.try_into().expect("wanted valid play"),
            me.try_into().expect("wanted valid play"),
        ),
        (_, None, _, _) | (None, _, _, _) => Err(Fail(format!("line {s} too short"))),
        (_, Some(x), _, _) if x != ' ' => Err(Fail(format!("expected space in {s}"))),
        other => Err(Fail(format!("invalid: {other:?}"))),
    }
}

#[test]
fn test_score_line() {
    assert_eq!(score_line("A Y").expect("valid"), 8);
    assert_eq!(score_line("B X").expect("valid"), 1);
    assert_eq!(score_line("C Z").expect("valid"), 6);
}

fn score_game(s: &str) -> Result<usize, Fail> {
    s.split('\n')
        .filter(|line| *line != "")
        .fold(Ok(0), |acc, line| match (acc, score_line(line)) {
            (Err(e), _) | (_, Err(e)) => Err(e),
            (Ok(tot), Ok(n)) => Ok(tot + n),
        })
}

#[test]
fn test_score_game() {
    assert_eq!(score_game("A Y\nB X\nC Z\n").expect("valid game"), 15);
}

fn main() {
    let game = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let score = score_game(game).expect("invalid test input");
    println!("Day 02 part 1: {score}");
}
