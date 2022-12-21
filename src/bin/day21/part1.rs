use sscanf::scanf;
use std::collections::HashMap;

use lib::error::Fail;

type Number = i64;

#[derive(Debug, PartialEq, Eq)]
enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl TryFrom<char> for Op {
    type Error = Fail;
    fn try_from(ch: char) -> Result<Op, Fail> {
        match ch {
            '+' => Ok(Op::Add),
            '-' => Ok(Op::Subtract),
            '*' => Ok(Op::Multiply),
            '/' => Ok(Op::Divide),
            _ => Err(Fail(format!("unrecognised operation {ch}"))),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Expression {
    Value(Number),
    Binary(String, Op, String),
}

impl TryFrom<&str> for Expression {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Expression, Fail> {
        if let Ok(value) = s.parse() {
            Ok(Expression::Value(value))
        } else if let Ok((left, op, right)) = scanf!(s, "{str} {char} {str}") {
            Ok(Expression::Binary(
                left.to_owned(),
                Op::try_from(op)?,
                right.to_owned(),
            ))
        } else {
            Err(Fail(format!("failed to parse expresion {s}")))
        }
    }
}

#[test]
fn test_parse_expression() {
    assert_eq!(
        Expression::try_from("pppw + sjmn"),
        Ok(Expression::Binary(
            "pppw".to_string(),
            Op::Add,
            "sjmn".to_string()
        ))
    );
    assert_eq!(
        Expression::try_from("humn / dvpt"),
        Ok(Expression::Binary(
            "humn".to_string(),
            Op::Divide,
            "dvpt".to_string()
        ))
    );
}

#[derive(Debug, PartialEq, Eq)]
struct Monkey {
    name: String,
    expr: Expression,
}

impl TryFrom<&str> for Monkey {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Monkey, Fail> {
        match s.split_once(": ") {
            Some((name, expr)) => Ok(Monkey {
                name: name.to_string(),
                expr: Expression::try_from(expr)?,
            }),
            None => Err(Fail(format!("unable to parse input line {s}"))),
        }
    }
}

#[test]
fn test_parse_monkey() {
    assert_eq!(
        Monkey::try_from("root: pppw + sjmn"),
        Ok(Monkey {
            name: "root".to_string(),
            expr: Expression::Binary("pppw".to_string(), Op::Add, "sjmn".to_string()),
        })
    );
    assert_eq!(
        Monkey::try_from("ptdq: humn * dvpt"),
        Ok(Monkey {
            name: "ptdq".to_string(),
            expr: Expression::Binary("humn".to_string(), Op::Multiply, "dvpt".to_string()),
        })
    );
}

pub struct Symtab {
    monkeys: HashMap<String, Monkey>,
}

impl TryFrom<&str> for Symtab {
    type Error = Fail;

    fn try_from(s: &str) -> Result<Symtab, Fail> {
        let monkeys: HashMap<String, Monkey> = s
            .split_terminator('\n')
            .map(|s| Monkey::try_from(s).map(|m| (m.name.clone(), m)))
            .collect::<Result<HashMap<String, Monkey>, Fail>>()?;
        Ok(Symtab { monkeys })
    }
}

impl Op {
    fn eval(&self, left: &Expression, right: &Expression, symtab: &Symtab) -> Result<Number, Fail> {
        let left: Number = symtab.eval(left)?;
        let right: Number = symtab.eval(right)?;
        match self {
            Op::Add => Ok(left + right),
            Op::Subtract => Ok(left - right),
            Op::Multiply => Ok(left * right),
            Op::Divide => Ok(left / right),
        }
    }
}

impl Symtab {
    fn lookup(&self, name: &str) -> Result<&Expression, Fail> {
        match self.monkeys.get(name) {
            Some(monkey) => Ok(&monkey.expr),
            None => Err(Fail(format!("unknown monkey {name}"))),
        }
    }

    fn eval(&self, expr: &Expression) -> Result<Number, Fail> {
        match expr {
            Expression::Binary(left, op, right) => {
                op.eval(self.lookup(left)?, self.lookup(right)?, &self)
            }
            Expression::Value(x) => Ok(*x),
        }
    }
}

#[cfg(test)]
fn example() -> &'static str {
    concat!(
        "root: pppw + sjmn\n",
        "dbpl: 5\n",
        "cczh: sllz + lgvd\n",
        "zczc: 2\n",
        "ptdq: humn - dvpt\n",
        "dvpt: 3\n",
        "lfqf: 4\n",
        "humn: 5\n",
        "ljgn: 2\n",
        "sjmn: drzm * dbpl\n",
        "sllz: 4\n",
        "pppw: cczh / lfqf\n",
        "lgvd: ljgn * ptdq\n",
        "drzm: hmdt - zczc\n",
        "hmdt: 32\n",
    )
}

#[cfg(test)]
fn example_symtab() -> Result<Symtab, Fail> {
    Symtab::try_from(example())
}

pub fn solve_part1(syms: &Symtab) -> Result<Number, Fail> {
    syms.lookup("root")
        .and_then(|expression| syms.eval(expression))
}

#[test]
fn test_solve_part1() {
    let syms = example_symtab().expect("example should be valid");
    assert_eq!(solve_part1(&syms), Ok(152));
}
