use std::ops::{Index, IndexMut};
use std::str;

use sscanf::scanf;

use lib::error::Fail;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum RobotType {
    Ore,
    Clay,
    Obsidian,
    Geode,
}
use RobotType::*;

const ALL_ROBOT_TYPES: [RobotType; 4] = [Ore, Clay, Obsidian, Geode];

impl TryFrom<&str> for RobotType {
    type Error = Fail;
    fn try_from(s: &str) -> Result<RobotType, Fail> {
        match s {
            "ore" => Ok(RobotType::Ore),
            "clay" => Ok(RobotType::Clay),
            "obsidian" => Ok(RobotType::Obsidian),
            "geode" => Ok(RobotType::Geode),
            _ => Err(Fail(format!("unknown robot type {s}"))),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Robot {
    which: RobotType,
    costs: Stock,
}

impl TryFrom<&str> for Robot {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Robot, Fail> {
        match scanf!(s, "Each {str} robot costs {str}") {
            Ok((name, costs)) => {
                let which = RobotType::try_from(name)?;
                let mut ore_cost = 0;
                let mut clay_cost = 0;
                let mut obsidian_cost = 0;
                for cost_item in costs.split(" and ") {
                    match cost_item
                        .split_once(' ')
                        .map(|(howmany, what)| (howmany.parse(), what))
                    {
                        Some((Ok(n), "ore")) => ore_cost = n,
                        Some((Ok(n), "clay")) => clay_cost = n,
                        Some((Ok(n), "obsidian")) => obsidian_cost += n,
                        _ => {
                            return Err(Fail(format!("failed to parse cost item '{cost_item}'")));
                        }
                    }
                }
                Ok(Robot {
                    which,
                    costs: Stock([ore_cost, clay_cost, obsidian_cost, 0]),
                })
            }
            Err(_) => Err(Fail(format!("failed to parse robot recipe '{s}'"))),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Blueprint {
    id: u32,
    ore_robot: Robot,
    clay_robot: Robot,
    obsidian_robot: Robot,
    geode_robot: Robot,
}

impl Index<&RobotType> for Blueprint {
    type Output = Robot;
    fn index(&self, which: &RobotType) -> &Self::Output {
        match which {
            RobotType::Ore => &self.ore_robot,
            RobotType::Clay => &self.clay_robot,
            RobotType::Obsidian => &self.obsidian_robot,
            RobotType::Geode => &self.geode_robot,
        }
    }
}

impl Index<RobotType> for Blueprint {
    type Output = Robot;
    fn index(&self, which: RobotType) -> &Self::Output {
        self.index(&which)
    }
}

fn next_robot<'a, I>(it: &mut I) -> Result<Robot, Fail>
where
    I: Iterator<Item = &'a str>,
{
    match it.next() {
        Some(s) => Ok(Robot::try_from(s)?),
        None => Err(Fail("blueprint ended too soon".to_string())),
    }
}

impl TryFrom<&str> for Blueprint {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Blueprint, Fail> {
        let s = format!("{} ", s.trim());
        if let Some((intro, recipes)) = s.split_once(": ") {
            match scanf!(intro, "Blueprint {u32}") {
                Ok(id) => {
                    let mut it = recipes.split(". ");
                    match next_robot(&mut it) {
                        Ok(ore_robot) => match next_robot(&mut it) {
                            Ok(clay_robot) => match next_robot(&mut it) {
                                Ok(obsidian_robot) => match next_robot(&mut it) {
                                    Ok(geode_robot) => match it.next() {
                                        Some("") | None => Ok(Blueprint {
                                            id,
                                            ore_robot,
                                            clay_robot,
                                            obsidian_robot,
                                            geode_robot,
                                        }),
                                        Some(tail) => Err(Fail(format!(
                                            "unexpected content at end of blueprint: '{tail}'"
                                        ))),
                                    },
                                    Err(e) => Err(Fail(format!(
                                        "failed to parse recipe for geode robot: {e}"
                                    ))),
                                },
                                Err(e) => Err(Fail(format!(
                                    "failed to parse recipe for obsidian robot: {e}"
                                ))),
                            },
                            Err(e) => {
                                Err(Fail(format!("failed to parse recipe for clay robot: {e}")))
                            }
                        },
                        Err(e) => Err(Fail(format!("failed to parse recipe for ore robot: {e}"))),
                    }
                }
                Err(_) => Err(Fail(format!("blueprint has unexpected start {intro}"))),
            }
        } else {
            Err(Fail(format!("blueprint {s} contains no ':'")))
        }
    }
}

#[test]
fn test_parse_blueprint() {
    let blueprint_string = "Blueprint 26: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 7 clay. Each geode robot costs 3 ore and 9 obsidian.";
    assert_eq!(
        Blueprint::try_from(blueprint_string),
        Ok(Blueprint {
            id: 26,
            ore_robot: Robot {
                which: RobotType::Ore,
                costs: Stock::new(&[(RobotType::Ore, 4)]),
            },
            clay_robot: Robot {
                which: RobotType::Clay,
                costs: Stock::new(&[(RobotType::Ore, 3)]),
            },
            obsidian_robot: Robot {
                which: RobotType::Obsidian,
                costs: Stock::new(&[(RobotType::Ore, 3), (RobotType::Clay, 7)]),
            },
            geode_robot: Robot {
                which: RobotType::Geode,
                costs: Stock::new(&[(RobotType::Ore, 3), (RobotType::Obsidian, 9)]),
            }
        })
    );
}

#[derive(Debug, PartialEq, Eq)]
enum Action {
    Build(RobotType),
    Idle,
}

#[derive(Debug, PartialEq, Eq)]
struct Stock([i8; 4]);

impl Stock {
    fn is_sufficient_for(&self, cost: &Self) -> bool {
        self.0
            .iter()
            .zip(cost.0.iter())
            .all(|(have, need)| have >= need)
    }

    fn consume(&mut self, cost: &Self) {
        self.0
            .iter_mut()
            .zip(cost.0.iter())
            .for_each(|(have, need)| *have -= need)
    }

    fn empty() -> Stock {
        Stock([0; 4])
    }

    fn new(items: &[(RobotType, i8)]) -> Stock {
        let mut result: Stock = Stock([0; 4]);
        for (rt, count) in items.iter() {
            result[rt] = *count;
        }
        result
    }
}

impl IndexMut<&RobotType> for Stock {
    fn index_mut(&mut self, which: &RobotType) -> &mut Self::Output {
        &mut self.0[*which as usize]
    }
}

impl Index<&RobotType> for Stock {
    type Output = i8;
    fn index(&self, which: &RobotType) -> &Self::Output {
        &self.0[*which as usize]
    }
}

impl Index<RobotType> for Stock {
    type Output = i8;
    fn index(&self, which: RobotType) -> &Self::Output {
        self.index(&which)
    }
}

#[derive(Debug, PartialEq, Eq)]
struct State {
    ores: Stock,
    robots: Stock,
    minute: i8,
}

impl Default for State {
    fn default() -> State {
        State {
            ores: Stock::empty(),
            robots: Stock::new(&[(RobotType::Ore, 1)]),
            minute: 0,
        }
    }
}

impl State {
    fn possible_actions(&self, blueprint: &Blueprint) -> Vec<Action> {
        let mut result = Vec::with_capacity(ALL_ROBOT_TYPES.len());
        result.push(Action::Idle);
        for rt in ALL_ROBOT_TYPES.iter() {
            let cost: &Stock = &blueprint[rt].costs;
            if self.ores.is_sufficient_for(cost) {
                result.push(Action::Build(*rt));
            }
        }
        result
    }

    fn start_build(&mut self, action: &Action, blueprint: &Blueprint) {
        match action {
            Action::Build(robot_type) => {
                let cost: &Stock = &blueprint[robot_type].costs;
                self.ores.consume(cost);
            }
            Action::Idle => (),
        }
    }

    fn complete_build(&mut self, action: &Action) {
        match action {
            Action::Build(robot_type) => {
                self.robots[robot_type] += 1;
            }
            Action::Idle => (),
        }
    }

    fn harvest_resources(&mut self) {
        for rt in ALL_ROBOT_TYPES.iter() {
            self.ores[rt] += self.robots[rt];
        }
    }

    fn simulate_action(&mut self, action: &Action, blueprint: &Blueprint) {
        self.start_build(action, blueprint);
        self.harvest_resources();
        self.complete_build(action);
        self.minute += 1;
    }
}

#[cfg(test)]
fn example_blueprint_string() -> &'static str {
    concat!(
        "Blueprint 1:",
        " Each ore robot costs 4 ore.",
        " Each clay robot costs 2 ore.",
        " Each obsidian robot costs 3 ore and 14 clay.",
        " Each geode robot costs 2 ore and 7 obsidian.",
        "\n",
        "Blueprint 2:",
        " Each ore robot costs 2 ore.",
        " Each clay robot costs 3 ore.",
        " Each obsidian robot costs 3 ore and 8 clay.",
        " Each geode robot costs 3 ore and 12 obsidian.",
        "\n"
    )
}

fn parse_blueprints(s: &str) -> Vec<Blueprint> {
    s.split_terminator('\n')
        .map(|s| Blueprint::try_from(s).expect("example blueprint is valid"))
        .collect()
}

#[cfg(test)]
fn example_blueprints() -> Vec<Blueprint> {
    parse_blueprints(example_blueprint_string())
}

#[test]
fn example_walkthrough() {
    let blueprint = &example_blueprints()[0];
    let mut state = State::default();
    assert_eq!(state.robots[RobotType::Ore], 1);
    state.simulate_action(&Action::Idle, &blueprint);
    assert_eq!(
        state,
        State {
            ores: Stock::new(&[(RobotType::Ore, 1)]),
            robots: Stock::new(&[(RobotType::Ore, 1)]),
            minute: 1,
        }
    );
    state.simulate_action(&Action::Idle, &blueprint);
    assert_eq!(
        state,
        State {
            ores: Stock::new(&[(RobotType::Ore, 2)]),
            robots: Stock::new(&[(RobotType::Ore, 1)]),
            minute: 2,
        }
    );
    state.simulate_action(&Action::Build(RobotType::Clay), &blueprint);
    assert_eq!(
        state,
        State {
            ores: Stock::new(&[(RobotType::Ore, 1)]),
            robots: Stock::new(&[(RobotType::Ore, 1), (RobotType::Clay, 1)]),
            minute: 3,
        }
    );
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    let blueprints = parse_blueprints(input);
}
