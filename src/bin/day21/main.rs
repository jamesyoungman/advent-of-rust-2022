use std::str;

mod part1;
mod part2;

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    let syms1 = part1::Symtab::try_from(input).expect("valid input");
    println!(
        "Day 21 part 1: {}",
        part1::solve_part1(&syms1).expect("should be able to solve part 1"),
    );

    println!(
        "Day 21 part 2: {}",
        part2::solve(input).expect("should be able to solve part 2"),
    );
}
