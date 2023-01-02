use std::cmp::Reverse;
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};
use std::str;

use priority_queue::PriorityQueue;
use sscanf::scanf;

use lib::error::Fail;

type StockCount = i16;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
enum RobotType {
    Ore,
    Clay,
    Obsidian,
    Geode,
}
use RobotType::*;

impl Display for RobotType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Ore => "ore",
            Clay => "clay",
            Obsidian => "obsidian",
            Geode => "geode",
        })
    }
}

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

impl Blueprint {
    fn any_recipe_requiring_more_than(&self, rt: &RobotType, n: StockCount) -> Option<RobotType> {
        ALL_ROBOT_TYPES
            .iter()
            .copied()
            .find(|robot| self[*robot].costs[rt] > n)
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum Action {
    Build(RobotType),
    Idle,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
struct Stock([StockCount; 4]);

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

    fn new(items: &[(RobotType, StockCount)]) -> Stock {
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
    type Output = StockCount;
    fn index(&self, which: &RobotType) -> &Self::Output {
        &self.0[*which as usize]
    }
}

impl Index<RobotType> for Stock {
    type Output = StockCount;
    fn index(&self, which: RobotType) -> &Self::Output {
        self.index(&which)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
struct State {
    ores: Stock,
    robots: Stock,
}

impl Default for State {
    fn default() -> State {
        State {
            ores: Stock::empty(),
            robots: Stock::new(&[(RobotType::Ore, 1)]),
        }
    }
}

trait Scored {
    fn score(&self) -> StockCount;
    fn total_robots(&self) -> StockCount;
    fn preference(&self) -> (StockCount, StockCount) {
        (self.score(), self.total_robots())
    }
}

impl Scored for State {
    fn score(&self) -> StockCount {
        self.ores[RobotType::Geode]
    }

    fn total_robots(&self) -> StockCount {
        self.robots.0.iter().sum()
    }
}

impl State {
    fn possible_next_states(
        &self,
        blueprint: &Blueprint,
        minutes_remaining: usize,
    ) -> Vec<(State, Vec<Action>)> {
        let select_actions = |actions: Vec<Action>| -> (State, Vec<Action>) {
            let next_state = self.with_actions(&actions, blueprint);
            (next_state, actions)
        };

        if minutes_remaining <= 1 {
            // There's no point building anything, because the
            // resulting robot would have no time to do anything.
            return vec![select_actions(vec![Action::Idle])];
        }

        let mut result = Vec::with_capacity(ALL_ROBOT_TYPES.len() + 1);
        for rt in ALL_ROBOT_TYPES.iter().rev() {
            let cost: &Stock = &blueprint[rt].costs;
            if self.ores.is_sufficient_for(cost) {
                if rt == &RobotType::Geode {
                    result.push(select_actions(vec![Action::Build(*rt)]));
                } else {
                    // We could build a robot of type `rt`.  But if we
                    // already have N such robots, this is only useful if
                    // the blueprint contains at least one robot type
                    // which needs more than N units of that resource.
                    // This is so because we will already get N units of
                    // the resource each turn,m and can never use up more
                    // than N units per turn.
                    let robots_onhand = self.robots[rt];
                    if blueprint
                        .any_recipe_requiring_more_than(rt, robots_onhand)
                        .is_some()
                    {
                        result.push(select_actions(vec![Action::Build(*rt)]));
                    }
                }
            }
        }
        result.push((
            self.with_actions(&[Action::Idle], blueprint),
            vec![Action::Idle],
        ));
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
    }

    fn with_actions(&self, actions: &[Action], blueprint: &Blueprint) -> State {
        actions.iter().fold(self.clone(), |mut state, action| {
            state.simulate_action(action, blueprint);
            state
        })
    }

    /// Returns a naive upper bound on the maximum number of geodes
    /// collectable in `minutes_remaining` from this state.  It
    /// currently ignores the blueprint.
    fn naive_upper_bound(&self, minutes_remaining: usize) -> StockCount {
        let initial = (self.ores[RobotType::Geode], self.robots[RobotType::Geode]);
        let (ores, _robots) = (0..minutes_remaining)
            .fold(initial, |(ores, robots), _time| (ores + robots, robots + 1));
        ores
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Solution {
    actions: Vec<Action>,
    score: StockCount,
    total_robots: StockCount,
}

impl Solution {
    fn has_more_geodes_than(&self, other: &Solution) -> bool {
        self.score > other.score
    }

    fn is_single_candidate(&self, minutes_remaining: usize) -> bool {
        self.actions.len() == minutes_remaining
    }
}

impl Scored for Solution {
    fn score(&self) -> StockCount {
        self.score
    }

    fn total_robots(&self) -> StockCount {
        self.total_robots
    }
}

fn select_best<I, S>(mut solutions: I) -> Option<S>
where
    I: Iterator<Item = S>,
    S: Scored,
{
    fn prefer_better<S: Scored>(current: S, candidate: S) -> S {
        if current.score() > candidate.score() {
            current
        } else {
            candidate
        }
    }

    match solutions.next() {
        None => None,
        Some(first) => Some(solutions.fold(first, prefer_better)),
    }
}

fn solve_bruteforce(
    state: State,
    blueprint: &Blueprint,
    minutes_remaining: usize,
) -> Option<Solution> {
    if minutes_remaining == 0 {
        Some(Solution {
            actions: Vec::new(),
            score: state.score(),
            total_robots: state.total_robots(),
        })
    } else {
        let best: Option<Solution> = select_best(
            state
                .possible_next_states(blueprint, minutes_remaining)
                .into_iter()
                .filter_map(|(state, actions)| {
                    if let Some(mut sol) =
                        solve_bruteforce(state, blueprint, minutes_remaining - actions.len())
                    {
                        sol.actions.extend(actions);
                        Some(sol)
                    } else {
                        None
                    }
                }),
        );
        if let Some(b) = best.as_ref() {
            println!(
                "best solution at {} minutes remaining yields {}",
                minutes_remaining, b.score
            );
        }
        best
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct BranchBoundNode {
    partial_solution: Solution,
    state: State,
    minutes_remaining: usize,
}

impl BranchBoundNode {
    fn with_actions(&self, actions: Vec<Action>, blueprint: &Blueprint) -> BranchBoundNode {
        let state = self.state.with_actions(&actions, blueprint);
        BranchBoundNode {
            partial_solution: Solution {
                actions: {
                    let mut v = self.partial_solution.actions.clone();
                    v.extend(actions);
                    v
                },
                score: state.score(),
                total_robots: state.total_robots(),
            },
            state,
            minutes_remaining: self.minutes_remaining - 1,
        }
    }
}

fn construct_worst_solution(state: State, minutes_remaining: usize) -> BranchBoundNode {
    // Our worst solution is simply to do nothing.
    let initial: BranchBoundNode = BranchBoundNode {
        partial_solution: Solution {
            actions: Vec::new(),
            score: state.score(),
            total_robots: state.total_robots(),
        },
        state,
        minutes_remaining,
    };

    fn do_nothing(mut node: BranchBoundNode, time: usize) -> BranchBoundNode {
        node.partial_solution.actions.push(Action::Idle);
        node.state.harvest_resources();
        node.partial_solution.score = node.state.score();
        node.minutes_remaining = time;
        node
    }
    (0..minutes_remaining).rev().fold(initial, do_nothing)
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Debug)]
struct Priority {
    score: StockCount,
    total_robots: StockCount,
    minutes_used: usize,
}

fn rev_priority<S: Scored>(thing: &S, minutes_used: usize) -> Reverse<Priority> {
    Reverse(Priority {
        score: thing.score(),
        total_robots: thing.total_robots(),
        minutes_used,
    })
}

fn branch(
    node: &BranchBoundNode,
    current_optimum: &BranchBoundNode,
    blueprint: &Blueprint,
    total_minutes: usize,
    pq: &mut PriorityQueue<BranchBoundNode, Reverse<Priority>>,
) -> usize {
    let mut pruned: usize = 0;
    let mut fanout: usize = 0;
    if node.minutes_remaining > 0 {
        for (next_state, actions) in node
            .state
            .possible_next_states(blueprint, node.minutes_remaining)
        {
            let ub = next_state.naive_upper_bound(node.minutes_remaining - actions.len());
            let current = current_optimum.state.score();
            if ub < current {
                // the bound on this node idndicates that it cannot be worth pursuing.
                println!("pruning because {ub} < {current}");
                pruned += 1;
            } else {
                let next = node.with_actions(actions, blueprint);
                let minutes_used = total_minutes - node.minutes_remaining;
                let reversed_pri = rev_priority(&next.partial_solution, minutes_used);
                pq.push(next, reversed_pri);
                fanout += 1;
            }
        }
        //println!("branch: fanout is {fanout}");
    } else {
        //println!("branch: no time left");
    }
    pruned
}

fn solve_branch_and_bound(
    state: State,
    blueprint: &Blueprint,
    initial_minutes_remaining: usize,
) -> Option<Solution> {
    println!("solving for blueprint {}", blueprint.id);
    assert!(initial_minutes_remaining > 0);
    let mut prune_count: usize = 0;
    let mut current_optimum: BranchBoundNode = dbg!(construct_worst_solution(
        state.clone(),
        initial_minutes_remaining
    ));
    let mut pq: PriorityQueue<BranchBoundNode, Reverse<Priority>> = PriorityQueue::new();
    let initial_rp = rev_priority(&state, 0);
    let state_score = state.score();
    let total_robots = state.total_robots();
    let initial = BranchBoundNode {
        partial_solution: Solution {
            actions: Vec::new(), // no actions yet
            score: state_score,
            total_robots,
        },
        state,
        minutes_remaining: initial_minutes_remaining,
    };
    pq.push(initial, initial_rp);

    let mut iterations: usize = 0;
    while let Some((partial_node, _reversed_score)) = pq.pop() {
        iterations += 1;
        if iterations > 100_000 {
            return None;
        }
        //println!(
        //    "considering partial_node {partial_node:?} with score {} and reversed_score {:?}...",
        //    partial_node.state.score(),
        //    reversed_score,
        //);
        let single = partial_node
            .partial_solution
            .is_single_candidate(initial_minutes_remaining);
        let better = partial_node.state.score() > current_optimum.state.score();
        let interesting = partial_node.state.score() > 0 || current_optimum.state.score() > 0;
        if single {
            if better {
                println!("solve_branch_and_bound: tenatively accepting {partial_node:?}");
                current_optimum = partial_node;
                dbg!(&current_optimum);
            } else {
                // discard this node, we already have a better solution
                if interesting {
                    println!("solve_branch_and_bound: dropping {partial_node:?} because it is no better than {current_optimum:?} ({} vs {})",
			     partial_node.state.score(),
			     current_optimum.state.score(),
		    );
                }
            }
        } else {
            //println!("solve_branch_and_bound: branching on {partial_node:?}");
            prune_count += branch(
                &partial_node,
                &current_optimum,
                blueprint,
                initial_minutes_remaining,
                &mut pq,
            );
        }
    }
    println!("{prune_count} branches were pruned");
    assert!(current_optimum
        .partial_solution
        .is_single_candidate(initial_minutes_remaining));
    Some(current_optimum.partial_solution)
}

//#[cfg(test)]
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

//#[cfg(test)]
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
        }
    );
    state.simulate_action(&Action::Idle, &blueprint);
    assert_eq!(
        state,
        State {
            ores: Stock::new(&[(RobotType::Ore, 2)]),
            robots: Stock::new(&[(RobotType::Ore, 1)]),
        }
    );
    state.simulate_action(&Action::Build(RobotType::Clay), &blueprint);
    assert_eq!(
        state,
        State {
            ores: Stock::new(&[(RobotType::Ore, 1)]),
            robots: Stock::new(&[(RobotType::Ore, 1), (RobotType::Clay, 1)]),
        }
    );
}

fn blueprint_1_max_24() {
    let blueprint = &example_blueprints()[0];
    let solution =
        solve_bruteforce(State::default(), &blueprint, 24).expect("should find a solution");
    dbg!(&solution);
    assert_eq!(solution.score, 9);
}

#[test]
fn blueprint_1_max_2() {
    let blueprint = &example_blueprints()[0];
    // This the state for the example walkthrough at the start of
    // minute 22.
    let state = State {
        ores: Stock::new(&[
            (RobotType::Ore, 3),
            (RobotType::Clay, 29),
            (RobotType::Obsidian, 2),
            (RobotType::Geode, 3),
        ]),
        robots: Stock::new(&[
            (RobotType::Ore, 1),
            (RobotType::Clay, 4),
            (RobotType::Obsidian, 2),
            (RobotType::Geode, 2),
        ]),
    };
    let solution = solve_bruteforce(state, &blueprint, 3).expect("should find a solution");
    dbg!(&solution);
    assert_eq!(solution.score, 9);
}

#[test]
fn blueprint_1_max_6() {
    let blueprint = &example_blueprints()[0];
    // This the state for the example walkthrough at the start of
    // minute 18.
    let state = State {
        ores: Stock::new(&[
            (RobotType::Ore, 3),
            (RobotType::Clay, 13),
            (RobotType::Obsidian, 8),
            (RobotType::Geode, 0),
        ]),
        robots: Stock::new(&[
            (RobotType::Ore, 1),
            (RobotType::Clay, 4),
            (RobotType::Obsidian, 2),
            (RobotType::Geode, 0),
        ]),
    };
    let bf_solution =
        solve_bruteforce(state.clone(), &blueprint, 7).expect("should find a solution");
    dbg!(&bf_solution);
    assert_eq!(bf_solution.score, 9);

    let bb_solution = solve_branch_and_bound(state, blueprint, 7);
    dbg!(&bb_solution);
    match bb_solution {
        Some(solution) => {
            assert_eq!(solution.score, 9);
        }
        None => {
            panic!("could not solve the 7-minute-only problem");
        }
    }
}

impl Scored for (&Blueprint, Solution) {
    fn score(&self) -> StockCount {
        self.1.score()
    }

    fn total_robots(&self) -> StockCount {
        self.1.total_robots()
    }
}

fn quality_level(bp_id: u32, score: StockCount) -> i64 {
    i64::from(bp_id) * i64::from(score)
}

fn solve_part1(s: &str) -> Option<i64> {
    let blueprints = parse_blueprints(s);
    let initial_state = State::default();
    let solutions: Vec<(&Blueprint, Solution)> = blueprints
        .iter()
        .map(|bp| {
            println!("solving blueprint {}...", bp.id);
            solve_branch_and_bound(initial_state.clone(), bp, 24).map(|solution| (bp, solution))
        })
        .filter_map(|sol| sol)
        .collect();
    if solutions.len() < blueprints.len() {
        println!("some blueprints could not be solved");
        None
    } else {
        Some(
            solutions
                .iter()
                .map(|(bp, solution)| quality_level(bp.id, solution.score()))
                .sum(),
        )
    }
}

//#[test]
fn test_solve_part1() {
    let example = example_blueprint_string();
    assert_eq!(solve_part1(example), Some(33));
}

fn main() {
    //let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    let example = example_blueprint_string();
    println!("{:?}", solve_part1(example));
}
