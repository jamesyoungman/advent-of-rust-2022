use std::cmp::Reverse;
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};
use std::str;

use sscanf::scanf;

use lib::error::Fail;

type StockCount = i16;
type Minutes = i8;

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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Action {
    Build(RobotType),
    Idle(Minutes),
}

fn duration(actions: &[Action]) -> Minutes {
    actions.iter().fold(0, |acc, action| {
        acc + match action {
            Action::Idle(t) => *t,
            Action::Build(_) => 1,
        }
    })
}

fn minutes_needed_to_cover(
    current: StockCount,
    target: StockCount,
    acquisition_rate: StockCount,
) -> Option<Minutes> {
    if current >= target {
        Some(0) // we have enough on hand already
    } else {
        if acquisition_rate > 0 {
            let shortfall = target - current;
            Minutes::try_from(shortfall / acquisition_rate).ok()
        } else {
            None
        }
    }
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

    /// Given current stock levels and already-built robots, compute
    /// how long it will take for us to cover the cost of building
    /// some other robot.  Return None if we cannot collect enough
    /// material to build that robot, ever (e.g. because it requires
    /// an ore we cannot collect yet).
    fn minutes_needed_to_cover_cost(
        &self,
        target: &Self,
        acquisition_rate: &Self,
    ) -> Option<Minutes> {
        let mut result: Minutes = Minutes::MAX;
        for (i, rt) in ALL_ROBOT_TYPES.iter().enumerate() {
            match minutes_needed_to_cover(self[rt], target[rt], acquisition_rate[rt]) {
                None => {
                    return None;
                }
                Some(t) => {
                    if i == 0 || result < t {
                        result = t;
                    }
                }
            }
        }
        Some(result)
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
        minutes_remaining: Minutes,
    ) -> Vec<(State, Vec<Action>)> {
        self.possible_next_states_bigsteps(blueprint, minutes_remaining)
    }

    fn possible_next_states_bigsteps(
        &self,
        blueprint: &Blueprint,
        minutes_remaining: Minutes,
    ) -> Vec<(State, Vec<Action>)> {
        let select_actions = |actions: Vec<Action>| -> (State, Vec<Action>) {
            assert!(duration(&actions) <= minutes_remaining);
            let next_state = self.with_actions(&actions, blueprint);
            (next_state, actions)
        };

        if minutes_remaining == 0 {
            return vec![];
        } else if minutes_remaining == 1 {
            // There's no point building anything, because the
            // resulting robot would have no time to do anything.
            return vec![select_actions(vec![Action::Idle(1)])];
        }
        let mut result = Vec::with_capacity(ALL_ROBOT_TYPES.len() + 1);
        for rt in ALL_ROBOT_TYPES.iter().rev() {
            if let Some(t) = self
                .ores
                .minutes_needed_to_cover_cost(&blueprint[rt].costs, &self.robots)
            {
                // We can build a robot of type `rt` by waiting
                // `t` minutes to accumulate the resources, then
                // building.
                if t < minutes_remaining {
                    let mut actions = Vec::with_capacity(2);
                    if t > 0 {
                        // First gather the resources (if needed)...
                        actions.push(Action::Idle(t));
                    }
                    actions.push(Action::Build(*rt)); // ... then build the robot.
                    result.push(select_actions(actions));
                }
            }
        }
        if result.is_empty() {
            // There was no robot type we could build by waiting.  So
            // we cannot build any robot at all.  Therefore we must be
            // idle for the rest of the available time.
            result.push(select_actions(vec![Action::Idle(minutes_remaining)]));
        }
        result
    }

    fn possible_next_states_tinysteps(
        &self,
        blueprint: &Blueprint,
        minutes_remaining: Minutes,
    ) -> Vec<(State, Vec<Action>)> {
        let select_actions = |actions: Vec<Action>| -> (State, Vec<Action>) {
            let next_state = self.with_actions(&actions, blueprint);
            (next_state, actions)
        };

        if minutes_remaining < 1 {
            return vec![];
        } else if minutes_remaining == 1 {
            // There's no point building anything, because the
            // resulting robot would have no time to do anything.
            return vec![select_actions(vec![Action::Idle(1)])];
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
            self.with_actions(&[Action::Idle(1)], blueprint),
            vec![Action::Idle(1)],
        ));
        result
    }

    fn start_build(&mut self, action: &Action, blueprint: &Blueprint) {
        match action {
            Action::Build(robot_type) => {
                let cost: &Stock = &blueprint[robot_type].costs;
                self.ores.consume(cost);
            }
            Action::Idle(_) => (),
        }
    }

    fn complete_build(&mut self, action: &Action) {
        match action {
            Action::Build(robot_type) => {
                self.robots[robot_type] += 1;
            }
            Action::Idle(_) => (),
        }
    }

    fn harvest_resources(&mut self, minutes: Minutes) {
        let duration =
            StockCount::try_from(minutes).expect("idle duration should be in range of StockCount");
        for rt in ALL_ROBOT_TYPES.iter() {
            self.ores[rt] += self.robots[rt] * duration;
        }
    }

    fn simulate_action(&mut self, action: &Action, blueprint: &Blueprint) {
        match &action {
            Action::Build(_) => {
                self.start_build(action, blueprint);
                self.harvest_resources(1);
                self.complete_build(action);
            }
            Action::Idle(n) => {
                self.harvest_resources(*n);
            }
        }
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
    fn naive_upper_bound(&self, minutes_remaining: Minutes) -> StockCount {
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

    fn is_single_candidate(&self, minutes_remaining: Minutes) -> bool {
        duration(&self.actions) == minutes_remaining
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct BranchBoundNode {
    partial_solution: Solution,
    state: State,
    minutes_remaining: Minutes,
}

impl BranchBoundNode {
    fn with_actions(&self, actions: Vec<Action>, blueprint: &Blueprint) -> BranchBoundNode {
        let minutes_used = duration(&actions);
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
            minutes_remaining: self.minutes_remaining - minutes_used,
        }
    }
}

fn construct_worst_solution(
    state: State,
    minutes_remaining: Minutes,
    blueprint: &Blueprint,
) -> BranchBoundNode {
    // Our worst solution is simply to do nothing.
    let idle_actions = vec![Action::Idle(minutes_remaining)];
    let newstate = state.with_actions(&idle_actions, blueprint);
    BranchBoundNode {
        partial_solution: Solution {
            actions: idle_actions,
            score: newstate.score(),
            total_robots: newstate.total_robots(),
        },
        state: newstate,
        minutes_remaining: 0,
    }
}

#[derive(PartialOrd, Ord, Eq, PartialEq, Debug)]
struct Priority(StockCount);

fn rev_priority<S: Scored>(thing: &S) -> Reverse<Priority> {
    Reverse(Priority(thing.score()))
}

fn branch(
    node: &BranchBoundNode,
    current_optimum: &BranchBoundNode,
    blueprint: &Blueprint,
    _total_minutes: Minutes,
    pq: &mut Vec<BranchBoundNode>,
) -> usize {
    let mut pruned: usize = 0;
    if node.minutes_remaining > 0 {
        for (next_state, actions) in node
            .state
            .possible_next_states(blueprint, node.minutes_remaining)
        {
            let ub = next_state.naive_upper_bound(node.minutes_remaining - duration(&actions));
            let current = current_optimum.state.score();
            if ub < current {
                // the bound on this node idndicates that it cannot be worth pursuing.
                //println!("pruning because {ub} < {current}");
                pruned += 1;
            } else {
                let next = node.with_actions(actions, blueprint);
                pq.push(next);
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
    initial_minutes_remaining: Minutes,
) -> Option<Solution> {
    println!("solving for blueprint {}", blueprint.id);
    assert!(initial_minutes_remaining > 0);
    let mut prune_count: usize = 0;
    let mut current_optimum: BranchBoundNode =
        construct_worst_solution(state.clone(), initial_minutes_remaining, blueprint);
    let initial_rp = rev_priority(&state);
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

    let mut pq: Vec<BranchBoundNode> = Vec::new();
    pq.push(initial);

    while let Some(partial_node) = pq.pop() {
        if partial_node
            .partial_solution
            .is_single_candidate(initial_minutes_remaining)
        {
            if partial_node.state.score() > current_optimum.state.score() {
                //println!("solve_branch_and_bound: tenatively accepting {partial_node:?}");
                current_optimum = partial_node;
            } else {
                prune_count += 1;
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
    state.simulate_action(&Action::Idle(1), &blueprint);
    assert_eq!(
        state,
        State {
            ores: Stock::new(&[(RobotType::Ore, 1)]),
            robots: Stock::new(&[(RobotType::Ore, 1)]),
        }
    );
    state.simulate_action(&Action::Idle(1), &blueprint);
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

//#[test]
//fn blueprint_1_max_24() {
//    let blueprint = &example_blueprints()[0];
//    let solution =
//        solve_branch_and_bound(State::default(), &blueprint, 24).expect("should find a solution");
//    dbg!(&solution);
//    assert_eq!(solution.score, 9);
//}

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
    let solution = solve_branch_and_bound(state, &blueprint, 3).expect("should find a solution");
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
        solve_branch_and_bound(state.clone(), &blueprint, 7).expect("should find a solution");
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
                .map(|(bp, solution)| {
                    let q = quality_level(bp.id, solution.score());
                    println!("optimum solution for blueprint {} {solution:?}", bp.id);
                    println!("quality level of blueprint {} is {q}", bp.id);
                    q
                })
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
