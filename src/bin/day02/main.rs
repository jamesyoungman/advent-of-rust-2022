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

    fn beats(&self) -> Play {
        match self {
            Rock => Scissors,
            Paper => Rock,
            Scissors => Paper,
        }
    }

    fn draws(&self) -> Play {
        *self
    }

    fn loses_to(&self) -> Play {
        match self {
            Scissors => Rock,
            Rock => Paper,
            Paper => Scissors,
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

type PlayScorer = fn(char, char) -> Result<usize, Fail>;

fn score_play_part1(them: char, me: char) -> Result<usize, Fail> {
    let me = me.try_into()?;
    let them = them.try_into()?;
    let outcome_score = match (me, them) {
        (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => 3, // draw
        (Rock, Scissors) | (Scissors, Paper) | (Paper, Rock) => 6, // I win
        (Scissors, Rock) | (Paper, Scissors) | (Rock, Paper) => 0, // I lose
    };
    Ok(outcome_score + me.score())
}

fn score_play_part2(them: char, outcome: char) -> Result<usize, Fail> {
    let them: Play = them.try_into()?;
    let (outcome_score, my_play) = match outcome {
        'X' => (0, them.beats()),    // lose
        'Y' => (3, them.draws()),    // draw
        'Z' => (6, them.loses_to()), // win
        invalid => {
            return Err(Fail(format!("expected X/Y/Z, got {invalid}")));
        }
    };
    Ok(outcome_score + my_play.score())
}

fn score_line(s: &str, scorer: PlayScorer) -> Result<usize, Fail> {
    let mut it = s.chars();
    let first = it.next();
    let second = it.next();
    let third = it.next();
    let fourth = it.next();
    match (first, second, third, fourth) {
        (_, _, _, Some(_)) => Err(Fail(format!("line {s} too long"))),
        (Some(them), Some(' '), Some(me), None) => scorer(them, me),
        (_, None, _, _) | (None, _, _, _) => Err(Fail(format!("line {s} too short"))),
        (_, Some(x), _, _) if x != ' ' => Err(Fail(format!("expected space in {s}"))),
        other => Err(Fail(format!("invalid: {other:?}"))),
    }
}

#[test]
fn test_score_line_part1() {
    assert_eq!(score_line("A Y", score_play_part1).expect("valid"), 8);
    assert_eq!(score_line("B X", score_play_part1).expect("valid"), 1);
    assert_eq!(score_line("C Z", score_play_part1).expect("valid"), 6);
}

#[test]
fn test_score_line_part2() {
    assert_eq!(score_line("A Y", score_play_part2).expect("valid"), 4);
    assert_eq!(score_line("B X", score_play_part2).expect("valid"), 1);
    assert_eq!(score_line("C Z", score_play_part2).expect("valid"), 7);
}

fn score_game(s: &str, scorer: PlayScorer) -> Result<usize, Fail> {
    s.split('\n')
        .filter(|line| !line.is_empty())
        .fold(Ok(0), |acc, line| match (acc, score_line(line, scorer)) {
            (Err(e), _) | (_, Err(e)) => Err(e),
            (Ok(tot), Ok(n)) => Ok(tot + n),
        })
}

#[test]
fn test_score_game() {
    assert_eq!(
        score_game("A Y\nB X\nC Z\n", score_play_part1).expect("valid game"),
        15
    );
    assert_eq!(
        score_game("A Y\nB X\nC Z\n", score_play_part2).expect("valid game"),
        12
    );
}

fn main() {
    let game = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let score1 = score_game(game, score_play_part1).expect("invalid test input");
    println!("Day 02 part 1: {score1}");
    let score2 = score_game(game, score_play_part2).expect("invalid test input");
    println!("Day 02 part 2: {score2}");
}
