// Day 11
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

use std::str;

use lib::error::Fail;
use sscanf::scanf;

#[derive(Debug, Clone)]
struct Item {
    worry_level: u32,
}

impl Item {
    fn reduce_worry_level(&mut self) {
        self.worry_level /= 3;
    }

    fn perform_operation(self, op: &Operation) -> Item {
        Item {
            worry_level: op.perform(self.worry_level),
        }
    }

    fn perform_test(&self, test_divisor: u32) -> bool {
        self.worry_level % test_divisor == 0
    }
}

impl TryFrom<&str> for Item {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Self, <Self as TryFrom<&str>>::Error> {
        let worry_level = s
            .parse()
            .map_err(|e| Fail(format!("{} is not an item id: {}", s, e)))?;
        Ok(Item { worry_level })
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.worry_level)
    }
}

#[derive(Debug)]
enum Operand {
    Itself,
    Number(u32),
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Operand::Itself => f.write_str("old"),
            Operand::Number(n) => write!(f, "{n}"),
        }
    }
}

impl TryFrom<&str> for Operand {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Self, <Self as TryFrom<&str>>::Error> {
        if s == "old" {
            Ok(Operand::Itself)
        } else {
            match s.parse() {
                Ok(n) => Ok(Operand::Number(n)),
                Err(_) => Err(Fail(format!("expected number, got {s}"))),
            }
        }
    }
}

#[derive(Debug)]
enum Operation {
    Add(Operand),
    Multiply(Operand),
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Operation::Add(n) => write!(f, "new = old + {n}"),
            Operation::Multiply(n) => write!(f, "new = old * {n}"),
        }
    }
}

impl TryFrom<&str> for Operation {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Self, <Self as TryFrom<&str>>::Error> {
        match scanf!(s, "new = old {char} {str}") {
            Ok(('*', rhs)) => Ok(Operation::Multiply(Operand::try_from(rhs)?)),
            Ok(('+', rhs)) => Ok(Operation::Add(Operand::try_from(rhs)?)),
            Ok((op, _)) => Err(Fail(format!("unknown operation {}", op))),
            Err(_) => Err(Fail(format!("incorect operation format [{s}]"))),
        }
    }
}

impl Operation {
    fn perform(&self, worry_level: u32) -> u32 {
        match self {
            Operation::Add(Operand::Number(n)) => worry_level + n,
            Operation::Add(Operand::Itself) => worry_level + worry_level,
            Operation::Multiply(Operand::Number(n)) => worry_level * n,
            Operation::Multiply(Operand::Itself) => worry_level * worry_level,
        }
    }
}

#[derive(Debug)]
struct Monkey {
    id: u32,
    items: Vec<Item>,
    operation: Operation,
    test_divisor: u32,
    if_true: u32,
    if_false: u32,
    inspection_count: usize,
}

impl Monkey {
    fn inspect_items(&mut self) -> Vec<(Item, u32)> {
        let mut result = Vec::with_capacity(self.items.len());
        for item in self.items.drain(..) {
            println!(
                "Monkey inspects an item with a worry level of {}",
                item.worry_level
            );
            let mut item = item.perform_operation(&self.operation);
            println!("  Worry level is updated to {}", item.worry_level);
            item.reduce_worry_level();
            println!(
                "  Monkey gets bored with item.  Worry level is divided by 3 to {}.",
                item.worry_level
            );
            let next_monkey: u32 = if item.perform_test(self.test_divisor) {
                self.if_true
            } else {
                self.if_false
            };
            println!(
                "  Item with worry level {} is thorwn to monkey {}.",
                item.worry_level, next_monkey
            );
            result.push((item, next_monkey));
            self.inspection_count += 1;
        }
        result
    }

    fn catch_items(&mut self, items: Vec<Item>) {
        self.items.extend(items);
    }
}

fn show_round_result(round_number: usize, monkeys: &mut BTreeMap<u32, Monkey>) {
    println!("After round {round_number}, the monkeys are holding items with these worry levels:");
    for (id, monkey) in monkeys.iter() {
        print!("Monkey {id}: ");
        for (i, item) in monkey.items.iter().enumerate() {
            if i > 0 {
                print!(", ");
            }
            print!("{}", item);
        }
        println!();
    }
}

fn inspect_items_round(round_number: usize, monkeys: &mut BTreeMap<u32, Monkey>) {
    // items_in_the_air holds the items to be caught by each monkey,
    // in the order they must be caught.
    let mut items_in_the_air: BTreeMap<u32, Vec<Item>> = BTreeMap::new();
    for (id, monkey) in monkeys.iter_mut() {
        println!("Monkey {id}:");
        assert_eq!(*id, monkey.id);
        if let Some(items) = items_in_the_air.remove(id) {
            monkey.catch_items(items);
        }
        for (item, to_monkey) in monkey.inspect_items() {
            items_in_the_air
                .entry(to_monkey)
                .and_modify(|v| v.push(item.clone()))
                .or_insert_with(|| vec![item.clone()]);
        }
    }
    for (to_monkey, items) in items_in_the_air.into_iter() {
        if let Some(monkey) = monkeys.get_mut(&to_monkey) {
            monkey.catch_items(items);
        } else {
            panic!("Item was thrown to nonexistent monkey {to_monkey}");
        }
    }
    show_round_result(round_number, monkeys);
}

fn write_list<I, T>(f: &mut Formatter, items: I) -> Result<(), std::fmt::Error>
where
    I: Iterator<Item = T>,
    T: Display,
{
    let mut first = true;
    for item in items {
        if !first {
            f.write_str(", ")?;
        } else {
            first = false;
        }
        write!(f, "{}", item)?;
    }
    Ok(())
}

impl Display for Monkey {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Monkey {}:\n  Starting items: ", self.id)?;
        write_list(f, self.items.iter())?;
        write!(
            f,
            concat!(
                "\n",
                "  Operation: {}\n",
                "  Test: divisible by {}\n",
                "  If true: throw to monkey {}\n",
                "  If false: throw to monkey {}\n"
            ),
            self.operation, self.test_divisor, self.if_true, self.if_false
        )
    }
}

impl TryFrom<&str> for Monkey {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Self, <Self as TryFrom<&str>>::Error> {
        let lines: Vec<&str> = s.split_terminator('\n').collect();
        if lines.len() != 6 {
            return Err(Fail(format!(
                "expected a monkey on 6 lines of nput, got {} lines",
                lines.len()
            )));
        }
        fn fail(s: &str, detail: &str) -> Fail {
            Fail(format!("monkey format error on [{s}]: {detail}"))
        }
        let id: u32 = match scanf!(lines[0], "Monkey {u32}:") {
            Ok(id) => id,
            Err(e) => {
                return Err(fail(lines[0], &format!("expected monkey id: {e}")));
            }
        };
        let items: Vec<Item> = match scanf!(lines[1], "  Starting items: {str}") {
            Ok(itemlist) => {
                let rv: Result<Vec<Item>, Fail> =
                    itemlist.split(", ").map(Item::try_from).collect();
                rv?
            }
            Err(e) => {
                return Err(fail(lines[1], &format!("expected starting items: {e}")));
            }
        };
        let operation = match scanf!(lines[2], "  Operation: {str}") {
            Ok(op_str) => match Operation::try_from(op_str) {
                Ok(op) => op,
                Err(e) => {
                    return Err(fail(lines[2], &format!("expected a valid operation: {e}")));
                }
            },
            Err(e) => {
                return Err(fail(lines[2], &format!("expected operation: {e}")));
            }
        };
        let test_divisor = match scanf!(lines[3], "  Test: divisible by {u32}") {
            Ok(n) => n,
            Err(e) => {
                return Err(fail(lines[3], &format!("expected test: {e}")));
            }
        };
        let if_true = match scanf!(lines[4], "    If true: throw to monkey {u32}") {
            Ok(n) => n,
            Err(_) => {
                return Err(fail(lines[4], "expected 'If true'"));
            }
        };
        let if_false = match scanf!(lines[5], "    If false: throw to monkey {u32}") {
            Ok(n) => n,
            Err(_) => {
                return Err(fail(lines[5], "expected 'If false'"));
            }
        };
        Ok(Monkey {
            id,
            items,
            operation,
            test_divisor,
            if_true,
            if_false,
            inspection_count: 0,
        })
    }
}

fn parse_monkeys(s: &str) -> Result<BTreeMap<u32, Monkey>, Fail> {
    s.split("\n\n")
        .map(Monkey::try_from)
        .map(|x| match x {
            Ok(monkey) => Ok((monkey.id, monkey)),
            Err(e) => Err(e),
        })
        .collect()
}

#[cfg(test)]
fn example() -> String {
    let text = str::from_utf8(include_bytes!("example.txt")).expect("valid example");
    text.to_string()
}

fn solve_part1(monkeys: &mut BTreeMap<u32, Monkey>) -> usize {
    for round in 1..=20 {
        inspect_items_round(round, monkeys);
    }
    let mut counts: Vec<usize> = monkeys
        .iter()
        .map(|(id, m)| {
            println!("Monkey {id} inspected items {} times.", m.inspection_count);
            m.inspection_count
        })
        .collect();
    counts.sort();
    counts.iter().rev().take(2).product()
}

#[test]
fn test_part1_example() {
    let mut monkeys = parse_monkeys(&example()).expect("example should be valid");
    assert_eq!(monkeys.len(), 4);
    for (_, monkey) in monkeys.iter() {
        println!("{monkey}");
    }
    assert_eq!(solve_part1(&mut monkeys), 10605);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    let mut monkeys = parse_monkeys(input).expect("input should be valid");
    println!("Day 11 part 1: {}", solve_part1(&mut monkeys));
}
