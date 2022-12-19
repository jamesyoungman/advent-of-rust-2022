//use std::str;

use sscanf::scanf;

use lib::error::Fail;

#[derive(Debug, PartialEq, Eq)]
enum RobotType {
    Ore,
    Clay,
    Obsidian,
    Geode,
}

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
    ore_cost: u32,
    clay_cost: u32,
    obsidian_cost: u32,
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
                    ore_cost,
                    clay_cost,
                    obsidian_cost,
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
                ore_cost: 4,
                clay_cost: 0,
                obsidian_cost: 0,
            },
            clay_robot: Robot {
                which: RobotType::Clay,
                ore_cost: 3,
                clay_cost: 0,
                obsidian_cost: 0,
            },
            obsidian_robot: Robot {
                which: RobotType::Obsidian,
                ore_cost: 3,
                clay_cost: 7,
                obsidian_cost: 0,
            },
            geode_robot: Robot {
                which: RobotType::Geode,
                ore_cost: 3,
                clay_cost: 0,
                obsidian_cost: 9,
            }
        })
    );
}

fn main() {}
