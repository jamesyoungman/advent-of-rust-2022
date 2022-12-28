use sscanf::scanf;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use lib::error::Fail;

type Number = i64;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let ch = match self {
            Op::Add => '+',
            Op::Subtract => '-',
            Op::Multiply => '*',
            Op::Divide => '/',
        };
        f.write_fmt(format_args!("{ch}"))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Definition {
    Value(Number),
    UnknownValue,
    Binary(String, Op, String),
}

impl TryFrom<&str> for Definition {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Definition, Fail> {
        if let Ok(value) = s.parse() {
            Ok(Definition::Value(value))
        } else if let Ok((left, op, right)) = scanf!(s, "{str} {char} {str}") {
            Ok(Definition::Binary(
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
fn test_parse_definition() {
    assert_eq!(
        Definition::try_from("pppw + sjmn"),
        Ok(Definition::Binary(
            "pppw".to_string(),
            Op::Add,
            "sjmn".to_string()
        ))
    );
    assert_eq!(
        Definition::try_from("humn / dvpt"),
        Ok(Definition::Binary(
            "humn".to_string(),
            Op::Divide,
            "dvpt".to_string()
        ))
    );
}

#[derive(Debug, PartialEq, Eq)]
struct Monkey {
    name: String,
    def: Definition,
}

impl TryFrom<&str> for Monkey {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Monkey, Fail> {
        match s.split_once(": ") {
            Some((name, rhs)) => Ok(Monkey {
                name: name.to_string(),
                def: Definition::try_from(rhs)?,
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
            def: Definition::Binary("pppw".to_string(), Op::Add, "sjmn".to_string()),
        })
    );
    assert_eq!(
        Monkey::try_from("ptdq: humn * dvpt"),
        Ok(Monkey {
            name: "ptdq".to_string(),
            def: Definition::Binary("humn".to_string(), Op::Multiply, "dvpt".to_string()),
        })
    );
}

#[derive(Debug, Eq, PartialEq)]
pub struct Symtab {
    monkeys: HashMap<String, Definition>,
}

impl TryFrom<&str> for Symtab {
    type Error = Fail;

    fn try_from(s: &str) -> Result<Symtab, Fail> {
        let monkeys: HashMap<String, Definition> = s
            .split_terminator('\n')
            .map(|s| Monkey::try_from(s).map(|m| (m.name, m.def)))
            .collect::<Result<HashMap<String, Definition>, Fail>>()?;
        Ok(Symtab { monkeys })
    }
}

impl Symtab {
    fn lookup(&self, name: &str) -> Result<Definition, Fail> {
        if name == "humn" {
            Ok(Definition::UnknownValue)
        } else {
            match self.monkeys.get(name) {
                Some(def) => Ok(def.clone()),
                None => Err(Fail(format!("unknown monkey {name}"))),
            }
        }
    }

    fn remove(&mut self, name: &str) -> Option<Definition> {
        self.monkeys.remove(name)
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

impl Op {
    fn eval(&self, left: &Number, right: &Number) -> Result<Number, Fail> {
        use Op::*;
        match self {
            Add => Ok(left + right),
            Subtract => Ok(left - right),
            Multiply => Ok(left * right),
            Divide => {
                if *right == 0 {
                    Err(Fail("division by zero".to_string()))
                } else {
                    let r = left % right;
                    if r == 0 {
                        Ok(left / right)
                    } else {
                        Err(Fail(format!(
                            "cannot compute {left}/{right} since it has a remainder"
                        )))
                    }
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Equation {
    left: Expression,
    right: Expression,
}

impl Display for Equation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}={}", self.left, self.right)
    }
}

impl TryFrom<&str> for Equation {
    type Error = Fail;

    fn try_from(s: &str) -> Result<Equation, Fail> {
        let mut symbols = Symtab::try_from(s)?;

        assert!(symbols.remove("humn").is_some()); // value is immaterial in part 2.
        let (left_sym, right_sym) = match symbols.remove("root") {
            Some(definition) => match definition {
                Definition::Binary(left, _op, right) => (left, right),
                Definition::Value(n) => {
                    return Err(Fail(format!(
                        "did not expect a deifned value for root: {n}"
                    )));
                }
                Definition::UnknownValue => {
                    return Err(Fail("did not expect 'root: humn'".to_string()));
                }
            },
            None => {
                return Err(Fail("no root expression".to_string()));
            }
        };
        let left_expr = Expression::from_symbol(&left_sym, &symbols)?;
        let right_expr = Expression::from_symbol(&right_sym, &symbols)?;
        Ok(Equation {
            left: left_expr,
            right: right_expr,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Expression {
    Value(Number),
    UnknownValue,
    Binary(Box<Expression>, Op, Box<Expression>),
}

fn binexpr(left: Expression, op: Op, right: Expression) -> Expression {
    Expression::Binary(Box::new(left), op, Box::new(right))
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expression::Value(n) => write!(f, "{n}"),
            Expression::UnknownValue => f.write_str("h"),
            Expression::Binary(left, op, right) => {
                write!(f, "({left}{op}{right})")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Simplified {
    Yes(Expression),
    No(Expression),
}

impl Display for Simplified {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Simplified::Yes(expr) => expr,
            Simplified::No(expr) => expr,
        }
        .fmt(f)
    }
}

impl Simplified {
    fn expr(&self) -> &Expression {
        match self {
            Simplified::Yes(expr) => expr,
            Simplified::No(expr) => expr,
        }
    }

    fn depends_on_unknown(&self) -> bool {
        self.expr().depends_on_unknown()
    }
}

impl From<Simplified> for Expression {
    fn from(simplified: Simplified) -> Expression {
        match simplified {
            Simplified::Yes(expr) => expr,
            Simplified::No(expr) => expr,
        }
    }
}

fn simplify(mut expression: Expression, mut depth: usize) -> Simplified {
    let mut changed: bool = false;
    loop {
        depth += 1;
        if depth > 1000 {
            panic!("simplification depth limit exceeded on {expression}",);
        }
        let simplified = expression.simplify_step(depth);
        expression = match simplified {
            Simplified::Yes(expr) => {
                changed = true;
                expr
            }
            Simplified::No(expr) => {
                if changed {
                    return Simplified::Yes(expr);
                } else {
                    return Simplified::No(expr);
                }
            }
        }
    }
}

fn simplify_if_possible(expr: Expression, depth: usize) -> Expression {
    simplify(expr, depth).into()
}

impl Expression {
    fn depends_on_unknown(&self) -> bool {
        match self {
            Expression::Value(_) => false,
            Expression::UnknownValue => true,
            Expression::Binary(left, _, right) => {
                left.depends_on_unknown() || right.depends_on_unknown()
            }
        }
    }

    /// Apply addition identities
    fn simplify_step_add(
        left: Box<Expression>,
        right: Box<Expression>,
        _depth: usize,
    ) -> Simplified {
        use Expression::Value;
        match (left.as_ref(), right.as_ref()) {
            (_, Value(0)) | (Value(0), _) => Simplified::Yes(*left),
            (x, y) if x == y => {
                Simplified::Yes(Expression::Binary(Box::new(Value(2)), Op::Multiply, right))
            }
            _ => Simplified::No(Expression::Binary(left, Op::Add, right)),
        }
    }

    /// Apply subtraction identities
    fn simplify_step_subtract(
        left: Box<Expression>,
        right: Box<Expression>,
        _depth: usize,
    ) -> Simplified {
        use Expression::Value;
        match (left.as_ref(), right.as_ref()) {
            // Subtraction identities
            (_, Value(0)) => Simplified::Yes(*left),
            (x, y) if x == y => Simplified::Yes(Value(0)),
            _ => Simplified::No(Expression::Binary(left, Op::Subtract, right)),
        }
    }

    /// Apply multiplication identities
    fn simplify_step_multiply(
        left: Box<Expression>,
        right: Box<Expression>,
        _depth: usize,
    ) -> Simplified {
        use Expression::{Binary, Value};
        match (left.as_ref(), right.as_ref()) {
            // Multiplication identities
            (_, Value(0)) | (Value(0), _) => Simplified::Yes(Value(0)),
            (_, Value(1)) => Simplified::Yes(*left),
            (Value(1), _) => Simplified::Yes(*right),
            (x, Binary(left, Op::Add, right)) => Simplified::Yes(
                // x*(left+right) = (x*left+x*right)
                Expression::Binary(
                    Box::new(Expression::Binary(
                        Box::new(x.clone()),
                        Op::Multiply,
                        left.clone(),
                    )),
                    Op::Add,
                    Box::new(Expression::Binary(
                        Box::new(x.clone()),
                        Op::Multiply,
                        right.clone(),
                    )),
                ),
            ),
            _ => Simplified::No(Expression::Binary(left, Op::Multiply, right)),
        }
    }

    /// Apply division identities
    fn simplify_step_divide(
        left: Box<Expression>,
        right: Box<Expression>,
        _depth: usize,
    ) -> Simplified {
        use Expression::Value;
        match (left.as_ref(), right.as_ref()) {
            // Division identities
            (x, y) if x == y => Simplified::Yes(Value(1)),
            (_, Value(1)) => Simplified::Yes(*left),
            (Value(0), _) => Simplified::Yes(Value(0)),
            _ => Simplified::No(Expression::Binary(left, Op::Divide, right)),
        }
    }

    fn simplify_step_binop(
        left: Box<Expression>,
        op: Op,
        right: Box<Expression>,
        depth: usize,
    ) -> Simplified {
        if let (Expression::Value(x), Expression::Value(y)) = (left.as_ref(), right.as_ref()) {
            let value: i64 = op.eval(x, y).expect("expression evaluation should succeed");
            return Simplified::Yes(Expression::Value(value));
        }
        let left: Box<Expression> = Box::new(simplify(*left, depth + 1).into());
        let right: Box<Expression> = Box::new(simplify(*right, depth + 1).into());
        match op {
            Op::Add => Self::simplify_step_add(left, right, depth),
            Op::Subtract => Self::simplify_step_subtract(left, right, depth),
            Op::Multiply => Self::simplify_step_multiply(left, right, depth),
            Op::Divide => Self::simplify_step_divide(left, right, depth),
        }
    }

    fn simplify_step(self, depth: usize) -> Simplified {
        match self {
            Expression::Value(_) | Expression::UnknownValue => Simplified::No(self.clone()),
            Expression::Binary(left_box, op, right_box) => {
                Self::simplify_step_binop(left_box, op, right_box, depth)
            }
        }
    }

    fn from_symbol(name: &str, symbols: &Symtab) -> Result<Expression, Fail> {
        match symbols.lookup(name) {
            Ok(def) => {
                let expr = Expression::from_definition(&def, symbols)?;
                Ok(expr)
            }
            Err(e) => Err(e),
        }
    }

    fn from_definition(def: &Definition, symbols: &Symtab) -> Result<Expression, Fail> {
        let result = match def {
            Definition::Value(n) => Expression::Value(*n),
            Definition::UnknownValue => Expression::UnknownValue,
            Definition::Binary(left_name, op, right_name) => {
                let left = symbols.lookup(left_name)?;
                let right = symbols.lookup(right_name)?;
                let left_expr: Expression = Expression::from_definition(&left, symbols)?;
                let right_expr: Expression = Expression::from_definition(&right, symbols)?;
                let left_expr = simplify_if_possible(left_expr, 0);
                let right_expr = simplify_if_possible(right_expr, 0);
                Expression::Binary(Box::new(left_expr), *op, Box::new(right_expr))
            }
        };
        Ok(result)
    }
}

const DEPTH_LIMIT: usize = 200;

impl Equation {
    fn solve(left: Expression, right: Expression, mut depth: usize) -> Result<Number, Fail> {
        use Expression::*;
        assert!(depth < DEPTH_LIMIT);
        depth += 1;

        if left.depends_on_unknown() {
            assert!(!right.depends_on_unknown());
        } else if right.depends_on_unknown() {
            return Equation::solve(right, left, depth + 1);
        } else {
            return Err(Fail(format!(
                "neither the side of the equation depends on the unknown: {left}={right}"
            )));
        }
        let simplified_left = simplify(left, depth);

        let simplified_right = simplify(right, depth);
        assert!(simplified_left.depends_on_unknown());
        assert!(!simplified_right.depends_on_unknown());

        match (simplified_left.into(), simplified_right.into()) {
            (_, UnknownValue) => unreachable!(),
            (UnknownValue, Value(n)) => Ok(n),
            (Value(x), Value(y)) => {
                if x == y {
                    Err(Fail(format!("any value of the unknown satisfies {x}={y}")))
                } else {
                    Err(Fail(format!(
                        "no possible value of thu enknwon satisfies {x}={y}"
                    )))
                }
            }
            (Binary(left, Op::Add, right), Value(n)) => {
                Equation::solve(*left, binexpr(Value(n), Op::Subtract, *right), depth)
            }
            (Binary(left, Op::Subtract, right), Value(n)) => {
                // left-right=n
                if left.depends_on_unknown() {
                    // f(h)-right=n
                    // => f(h)=n+right
                    Equation::solve(*left, binexpr(Value(n), Op::Add, *right), depth)
                } else {
                    // left-f(h)=n
                    // => -f(h)=n-left
                    // => f(h)=left-n
                    Equation::solve(*right, binexpr(*left, Op::Subtract, Value(n)), depth)
                }
            }
            (Binary(left, Op::Multiply, right), Value(n)) => {
                // left*right=n
                if left.depends_on_unknown() {
                    // f(h)*right=n
                    Equation::solve(*left, binexpr(Value(n), Op::Divide, *right), depth)
                } else {
                    // left*f(h)=n
                    // => f(h)=n/left
                    Equation::solve(*right, binexpr(Value(n), Op::Divide, *left), depth)
                }
            }
            (Binary(left, Op::Divide, right), Value(n)) => {
                if left.depends_on_unknown() {
                    Equation::solve(*left, binexpr(Value(n), Op::Multiply, *right), depth)
                } else {
                    todo!()
                }
            }
            (UnknownValue, Binary(_left, _op, _right)) => {
                todo!()
            }
            (Value(_n), Binary(_left, _op, _right)) => {
                todo!()
            }
            (Binary(ll, lop, lr), Expression::Binary(rl, rop, rr)) => {
                if lop == rop {
                    if lr == rr {
                        Equation::solve(*ll, *rl, depth)
                    } else if ll == rl {
                        Equation::solve(*lr, *rr, depth)
                    } else {
                        Err(Fail(format!(
                            "don't yet know how to solve {ll}{lop}{lr}={rl}{rop}{rr}"
                        )))
                    }
                } else {
                    Err(Fail(format!(
                        "don't yet know how to solve {ll}{lop}{lr}={rl}{rop}{rr}"
                    )))
                }
            }
        }
    }
}

#[test]
fn test_ptdq() {
    let symtab = Symtab::try_from(example()).expect("symbol table should be valid");
    match symtab.lookup("ptdq") {
        Ok(def) => {
            let expr = Expression::from_definition(&def, &symtab)
                .expect("should be able to expand definition as an expression");
            let repr = format!("{expr}");
            assert!(
                repr.contains('h'),
                "{repr} should expand to something including the unknown"
            );

            match simplify(expr, 0) {
                Simplified::Yes(simpler) => {
                    let repr = format!("{simpler}");
                    assert!(
                        repr.contains('h'),
                        "{repr} should expand to something including the unknown"
                    );
                }
                Simplified::No(not_simpler) => {
                    let repr = format!("{not_simpler}");
                    assert!(
                        repr.contains('h'),
                        "{repr} should expand to something including the unknown"
                    );
                }
            }
        }
        Err(e) => {
            panic!("symbol ptdq should exist in the example: {e}");
        }
    }
}

#[test]
fn test_simplify_binop() {
    let input = Expression::Binary(
        Box::new(Expression::UnknownValue),
        Op::Subtract,
        Box::new(Expression::Value(3)),
    );
    assert_eq!(
        simplify(input, 0),
        Simplified::No(Expression::Binary(
            Box::new(Expression::UnknownValue),
            Op::Subtract,
            Box::new(Expression::Value(3))
        ))
    );
}

pub fn solve(s: &str) -> Result<Number, Fail> {
    let eqn = Equation::try_from(s)?;
    println!("original equation to solve is {eqn}");
    Equation::solve(eqn.left, eqn.right, 0)
}

#[test]
fn test_solve_part2() {
    let input = example();
    assert_eq!(solve(input), Ok(301));
}
