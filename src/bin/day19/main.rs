use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};
use std::str;

use sscanf::scanf;

use lib::error::Fail;

type StockCount = u16;
type Minutes = i8;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Verbosity {
    Minimum,
    Modest,
    Maximum,
}

impl Verbosity {
    fn atleast(&self, other: &Verbosity) -> bool {
        self >= other
    }
}

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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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
    fn compute_max_requirements(&self) -> Vec<(RobotType, StockCount)> {
        ALL_ROBOT_TYPES
            .iter()
            .map(|ore_type| {
                (
                    *ore_type,
                    ALL_ROBOT_TYPES
                        .iter()
                        .map(|robot_type| self[robot_type].costs[ore_type])
                        .max()
                        .unwrap_or(0),
                )
            })
            .collect()
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

fn ceildiv(n: StockCount, d: StockCount) -> Option<Minutes> {
    if d != 0 {
        let result = (n + d - 1) / d;
        Minutes::try_from(result).ok()
    } else {
        None
    }
}

#[test]
fn test_ceildiv() {
    assert_eq!(ceildiv(10, 5), Some(2));
    assert_eq!(ceildiv(10, 4), Some(3));
}

fn minutes_needed_to_cover(
    current: StockCount,
    target: StockCount,
    acquisition_rate: StockCount,
) -> Option<Minutes> {
    if current >= target {
        Some(0) // we have enough on hand already
    } else if acquisition_rate > 0 {
        let shortfall = target - current;
        ceildiv(shortfall, acquisition_rate)
    } else {
        None
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
struct Stock([StockCount; 4]);

impl Display for Stock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut empty = true;
        for (i, rt) in ALL_ROBOT_TYPES
            .iter()
            .filter(|&rt| self[rt] != 0)
            .enumerate()
        {
            if i > 0 {
                f.write_str(" and ")?;
            }
            write!(f, "{} {}", self[rt], rt)?;
            empty = false;
        }
        if empty {
            f.write_str("nothing")?;
        }
        Ok(())
    }
}

impl Stock {
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
        max_requirement_by_type: &[(RobotType, StockCount)],
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
        for (rt, (max_requirement_rt, max_requirement_count)) in ALL_ROBOT_TYPES
            .iter()
            .zip(max_requirement_by_type.iter())
            .rev()
        {
            assert_eq!(rt, max_requirement_rt);
            if rt != &RobotType::Geode && max_requirement_count <= &self.robots[rt] {
                // This blueprint never calls for more of this ore
                // than we already have robots on hand, so in the 1
                // minute it takes the factory to consume that
                // resource, we will generate enough of that resource
                // to make another robot.  So we don't need any more
                // of that kind of robot.
                //
                // The obvious exception here is geodes.  No recipe
                // requires geodes to make a robot, but we should
                // still construct geode robots, because we want
                // geodes.
                continue;
            }
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

    fn start_build(&mut self, action: &Action, blueprint: &Blueprint, verbosity: &Verbosity) {
        match action {
            Action::Build(robot_type) => {
                let cost: &Stock = &blueprint[robot_type].costs;
                self.ores.consume(cost);
                if verbosity.atleast(&Verbosity::Maximum) {
                    println!("Spend {cost} to start building a {robot_type}-collecting robot");
                    println!("You now have {}.", &self.ores);
                }
            }
            Action::Idle(_) => (),
        }
    }

    fn complete_build(&mut self, action: &Action, verbosity: &Verbosity) {
        match action {
            Action::Build(robot_type) => {
                self.robots[robot_type] += 1;
                if verbosity.atleast(&Verbosity::Maximum) {
                    println!(
                        "The {robot_type}-collecting robot is ready; you now have {} of them.",
                        self.robots[robot_type]
                    );
                }
            }
            Action::Idle(_) => (),
        }
    }

    fn harvest_resources(&mut self, verbosity: &Verbosity) {
        for rt in ALL_ROBOT_TYPES.iter() {
            let added = self.robots[rt];
            self.ores[rt] += added;
            if added > 0 && verbosity.atleast(&Verbosity::Maximum) {
                println!(
                    "{} {}-collecting robot(s) collect(s) {} {}; you now have {} {}",
                    self.robots[rt], rt, self.robots[rt], rt, self.ores[rt], rt
                );
            }
        }
    }

    fn simulate_action(
        &mut self,
        action: &Action,
        time: &mut Minutes,
        blueprint: &Blueprint,
        verbosity: &Verbosity,
    ) {
        match &action {
            Action::Build(_) => {
                if verbosity.atleast(&Verbosity::Maximum) {
                    println!("\n== Minute {time} ==");
                }
                self.start_build(action, blueprint, verbosity);
                self.harvest_resources(verbosity);
                self.complete_build(action, verbosity);
                *time += 1;
            }
            Action::Idle(n) => {
                for _ in 0..*n {
                    if verbosity.atleast(&Verbosity::Maximum) {
                        println!("\n== Minute {time} ==");
                    }
                    self.harvest_resources(verbosity);
                    *time += 1;
                }
            }
        }
    }

    fn with_actions(&self, actions: &[Action], blueprint: &Blueprint) -> State {
        actions.iter().fold(self.clone(), |mut state, action| {
            let mut dummy_time = 0;
            state.simulate_action(action, &mut dummy_time, blueprint, &Verbosity::Minimum);
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
    fn explain(&self, blueprint: &Blueprint) {
        let end_state =
            self.actions
                .iter()
                .fold((1, State::default()), |(mut t, mut state), action| {
                    state.simulate_action(action, &mut t, blueprint, &Verbosity::Maximum);
                    (t, state)
                });
        println!("final state: {end_state:?}");
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

#[derive(PartialOrd, Ord, Eq, PartialEq, Debug, Clone, Copy)]
struct Priority(StockCount);

fn priority<S: Scored>(thing: &S) -> Priority {
    Priority(thing.score())
}

#[derive(Debug, Default)]
struct BasicPriorityQueue<T, P> {
    items: BTreeMap<P, Vec<T>>,
}

impl<T, P> BasicPriorityQueue<T, P> {
    fn new() -> BasicPriorityQueue<T, P> {
        BasicPriorityQueue {
            items: BTreeMap::new(),
        }
    }

    fn insert(&mut self, item: T, pri: P)
    where
        P: Ord + PartialOrd + Copy,
    {
        if let std::collections::btree_map::Entry::Vacant(e) = self.items.entry(pri) {
            e.insert(vec![item]);
        } else {
            match self.items.get_mut(&pri) {
                Some(v) => {
                    v.push(item);
                }
                None => unreachable!(),
            }
        }
    }

    fn pop(&mut self) -> Option<T>
    where
        P: PartialOrd + Ord + Copy,
    {
        let mut goners: Vec<P> = Vec::new();
        let mut result: Option<T> = None;
        for (pri, items) in self.items.iter_mut().rev() {
            let r = items.pop();
            if items.is_empty() {
                goners.push(*pri);
            }
            if r.is_some() {
                result = r;
                break;
            }
        }
        for pri in goners.into_iter() {
            self.items.remove(&pri);
        }
        result
    }
}

#[test]
fn test_basic_priority_queue() {
    let mut pq = BasicPriorityQueue::default();
    pq.insert('g', 10);
    pq.insert('k', 20);
    assert_eq!(pq.pop(), Some('k'));
    assert_eq!(pq.pop(), Some('g'));
    assert_eq!(pq.pop(), None);
}

fn branch(
    node: &BranchBoundNode,
    current_optimum: &BranchBoundNode,
    blueprint: &Blueprint,
    _total_minutes: Minutes,
    max_requirements_by_ore_type: &[(RobotType, StockCount)],
    pq: &mut BasicPriorityQueue<BranchBoundNode, Priority>,
) -> usize {
    let mut pruned: usize = 0;
    if node.minutes_remaining > 0 {
        for (next_state, actions) in node.state.possible_next_states(
            blueprint,
            node.minutes_remaining,
            max_requirements_by_ore_type,
        ) {
            let ub = next_state.naive_upper_bound(node.minutes_remaining - duration(&actions));
            let current = current_optimum.state.score();
            if ub < current {
                // the bound on this node idndicates that it cannot be worth pursuing.
                //println!("pruning because {ub} < {current}");
                pruned += 1;
            } else {
                let next = node.with_actions(actions, blueprint);
                let pri = priority(&next.state);
                pq.insert(next, pri);
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
    verbosity: &Verbosity,
) -> Option<Solution> {
    if verbosity.atleast(&Verbosity::Maximum) {
        println!(
            "solving for blueprint {} with branch-and-bound...",
            blueprint.id
        );
    }
    let max_requirements_by_ore_type = blueprint.compute_max_requirements();

    assert!(initial_minutes_remaining > 0);
    let mut prune_count: usize = 0;
    let mut current_optimum: BranchBoundNode =
        construct_worst_solution(state.clone(), initial_minutes_remaining, blueprint);
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

    let mut pq = BasicPriorityQueue::new();
    let pri = priority(&initial.state);
    pq.insert(initial, pri);

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
                &max_requirements_by_ore_type,
                &mut pq,
            );
        }
    }
    if verbosity.atleast(&Verbosity::Maximum) {
        println!("{prune_count} branches were pruned");
    }
    assert!(current_optimum
        .partial_solution
        .is_single_candidate(initial_minutes_remaining));
    Some(current_optimum.partial_solution)
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
    let mut time = 1;
    state.simulate_action(&Action::Idle(1), &mut time, &blueprint, &Verbosity::Maximum);
    assert_eq!(
        state,
        State {
            ores: Stock::new(&[(RobotType::Ore, 1)]),
            robots: Stock::new(&[(RobotType::Ore, 1)]),
        }
    );
    state.simulate_action(&Action::Idle(1), &mut time, &blueprint, &Verbosity::Maximum);
    assert_eq!(
        state,
        State {
            ores: Stock::new(&[(RobotType::Ore, 2)]),
            robots: Stock::new(&[(RobotType::Ore, 1)]),
        }
    );
    state.simulate_action(
        &Action::Build(RobotType::Clay),
        &mut time,
        &blueprint,
        &Verbosity::Maximum,
    );
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
    let solution = solve_branch_and_bound(state, &blueprint, 3, &Verbosity::Maximum)
        .expect("should find a solution");
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
    let bb_solution = solve_branch_and_bound(state, blueprint, 7, &Verbosity::Maximum);
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

fn solve_part1(s: &str, verbosity: &Verbosity) -> Option<i64> {
    let blueprints = parse_blueprints(s);
    let initial_state = State::default();
    let solutions: Vec<(&Blueprint, Solution)> = blueprints
        .iter()
        .flat_map(|bp| {
            solve_branch_and_bound(initial_state.clone(), bp, 24, verbosity)
                .map(|solution| (bp, solution))
        })
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
                    if verbosity.atleast(&Verbosity::Modest) {
                        println!("optimum solution for blueprint {} {solution:?}", bp.id);
                        solution.explain(bp);
                        println!("quality level of blueprint {} is {q}", bp.id);
                    }
                    q
                })
                .sum(),
        )
    }
}

#[test]
fn test_solve_part1() {
    let example = example_blueprint_string();
    assert_eq!(solve_part1(example, &Verbosity::Modest), Some(33));
}

#[cfg(test)]
fn just_blueprint_2() -> Blueprint {
    let example = example_blueprint_string();
    parse_blueprints(example)
        .iter()
        .find(|bp| bp.id == 2)
        .expect("blueprint 2 should exist")
        .clone()
}

#[test]
fn test_blueprint_2_32_minutes() {
    let bp2 = just_blueprint_2();
    let initial_state = State::default();
    let verbosity = Verbosity::Minimum;
    let time_liit = Minutes::from(32);
    let solution = solve_branch_and_bound(initial_state, &bp2, time_liit, &verbosity)
        .expect("blueprint 2 should have a solution");
    assert_eq!(solution.score(), 62);
}

fn solve_part2(s: &str, verbosity: &Verbosity) -> Option<i64> {
    let mut blueprints = parse_blueprints(s);
    blueprints.truncate(3); // only keep first 3 for part 2.

    let initial_state = State::default();
    let solutions: Vec<(&Blueprint, Solution)> = blueprints
        .iter()
        .flat_map(|bp| {
            solve_branch_and_bound(initial_state.clone(), bp, 32, verbosity)
                .map(|solution| (bp, solution))
        })
        .collect();
    if solutions.len() < blueprints.len() {
        println!("some blueprints could not be solved");
        None
    } else {
        Some(
            solutions
                .iter()
                .inspect(|(bp, solution)| {
                    if verbosity.atleast(&Verbosity::Modest) {
                        println!("\n\n==== Solution for Blueprint {}", bp.id);
                        solution.explain(bp);
                    }
                })
                .map(|(_, solution)| i64::from(solution.score()))
                .product(),
        )
    }
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    println!(
        "Day 19 part 1: {}",
        solve_part1(input, &Verbosity::Minimum).expect("should be able to solve part 1")
    );
    println!(
        "Day 19 part 2: {}",
        solve_part2(input, &Verbosity::Minimum).expect("should be able to solve part 2")
    );
}
