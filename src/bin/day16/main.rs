use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::str;

use sscanf::scanf;

use lib::error::Fail;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
struct ValveName(char, char);

impl TryFrom<&str> for ValveName {
    type Error = Fail;
    fn try_from(name: &str) -> Result<ValveName, Fail> {
        let mut it = name.chars();
        if let Some(first) = it.next() {
            if let Some(second) = it.next() {
                return Ok(ValveName(first, second));
            }
        }
        Err(Fail(format!("{name} is not a valid valve id")))
    }
}

impl Display for ValveName {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}{}", self.0, self.1)
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
struct ValveId(u32);

impl From<u32> for ValveId {
    fn from(n: u32) -> ValveId {
        ValveId(n)
    }
}

impl Display for ValveId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
struct Valve {
    flow_rate: u32,
    neighbours: Vec<ValveId>,
}

#[derive(Debug)]
struct Network {
    name_to_id: HashMap<ValveName, ValveId>,
    id_to_name: HashMap<ValveId, ValveName>,
    valves: HashMap<ValveId, Valve>,
    next_id: ValveId,
}

impl Network {
    fn assign_id(&mut self) -> ValveId {
        let result = self.next_id;
        self.next_id = ValveId(self.next_id.0 + 1);
        result
    }

    fn find_or_add_name_str(&mut self, name: &str) -> Result<ValveId, Fail> {
        match ValveName::try_from(name) {
            Ok(name) => Ok(self.find_or_add_name(name)),
            Err(e) => Err(e),
        }
    }

    fn find_or_add_name(&mut self, name: ValveName) -> ValveId {
        match self.name_to_id.get(&name) {
            Some(&id) => id,
            None => {
                let id = self.assign_id();
                self.id_to_name.insert(id, name.clone());
                self.name_to_id.insert(name, id);
                id
            }
        }
    }

    fn verify_invariant(&self) {
        assert!(!self.valves.contains_key(&self.next_id));
        for (id, valve) in self.valves.iter() {
            // Check forward and reverse mapping for id.
            match self.id_to_name.get(&id) {
                None => {
                    panic!("Value id {id} has no name");
                }
                Some(name) => match self.name_to_id.get(name) {
                    Some(reverse_id) => {
                        if id != reverse_id {
                            panic!("foward and reverse mappings for {id} are inconsistent");
                        }
                    }
                    None => {
                        panic!("reverse mapping is missing for {name}");
                    }
                },
            }

            for n in valve.neighbours.iter() {
                match self.valves.get(n) {
                    None => {
                        panic!(
                            "Valve {} has neighbour {} but that id is missing from self.valves",
                            id, n
                        );
                    }
                    Some(_) => (),
                }
            }
        }
    }

    fn initial_state(&self) -> NetworkState {
        self.valves.keys().fold(
            NetworkState {
                all_valves: 0,
                open_valves: 0,
            },
            |mut state: NetworkState, valve: &ValveId| {
                state.add_valve(valve);
                state
            },
        )
    }

    fn flow_rate(&self, location: &ValveId) -> u32 {
        self.valves[location].flow_rate
    }
}

impl TryFrom<&str> for Network {
    type Error = Fail;
    fn try_from(s: &str) -> Result<Network, Fail> {
        fn parse_line(nw: &mut Network, s: &str) -> Result<(), Fail> {
            fn ingest_valve(
                nw: &mut Network,
                name: &str,
                flow_rate: u32,
                neighbour_names: &str,
            ) -> Result<(), Fail> {
                let neighbour_names: &str = match scanf!(neighbour_names, "valve {str}") {
                    Ok(name) => name,
                    Err(_) => match scanf!(neighbour_names, "valves {str}") {
                        Ok(names) => names,
                        Err(e) => {
                            return Err(Fail(format!(
                                "cannot parse neighbour list {neighbour_names}"
                            )));
                        }
                    },
                };
                let neighbours: Vec<ValveId> = neighbour_names
                    .split(", ")
                    .map(|s| nw.find_or_add_name_str(s))
                    .collect::<Result<Vec<ValveId>, Fail>>()?;
                let id = nw.find_or_add_name_str(name)?;
                nw.valves.insert(
                    id,
                    Valve {
                        flow_rate,
                        neighbours,
                    },
                );
                Ok(())
            }
            if let Ok((name, flow_rate, neighbour_names)) =
                scanf!(s, "Valve {str} has flow rate={u32}; tunnels lead to {str}")
            {
                ingest_valve(nw, name, flow_rate, neighbour_names)
            } else if let Ok((name, flow_rate, neighbour_names)) =
                scanf!(s, "Valve {str} has flow rate={u32}; tunnel leads to {str}")
            {
                ingest_valve(nw, name, flow_rate, neighbour_names)
            } else {
                Err(Fail(format!("cannot parse input line {}", s)))
            }
        }
        let mut result: Network = Network::default();
        for line in s.split_terminator('\n') {
            parse_line(&mut result, line)?;
        }
        result.verify_invariant();
        Ok(result)
    }
}

impl Default for Network {
    fn default() -> Network {
        Network {
            name_to_id: HashMap::new(),
            id_to_name: HashMap::new(),
            valves: HashMap::new(),
            next_id: ValveId(1),
        }
    }
}

//#[cfg(test)]
fn example() -> &'static str {
    concat!(
        "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n",
        "Valve BB has flow rate=13; tunnels lead to valves CC, AA\n",
        "Valve CC has flow rate=2; tunnels lead to valves DD, BB\n",
        "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n",
        "Valve EE has flow rate=3; tunnels lead to valves FF, DD\n",
        "Valve FF has flow rate=0; tunnels lead to valves EE, GG\n",
        "Valve GG has flow rate=0; tunnels lead to valves FF, HH\n",
        "Valve HH has flow rate=22; tunnel leads to valve GG\n",
        "Valve II has flow rate=0; tunnels lead to valves AA, JJ\n",
        "Valve JJ has flow rate=21; tunnel leads to valve II\n",
    )
}

#[cfg(test)]
fn example_network() -> Network {
    Network::try_from(example()).expect("example should be valid")
}

#[test]
fn test_parse_example() {
    let network = example_network();
    let aa: ValveId = *network
        .name_to_id
        .get(&ValveName('A', 'A'))
        .expect("start point AA should not be missing");
    let bb: ValveId = *network
        .name_to_id
        .get(&ValveName('B', 'B'))
        .expect("valve DD should not be missing");

    assert_eq!(network.flow_rate(&aa), 0);
    assert_eq!(network.flow_rate(&bb), 13);
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
struct NetworkState {
    all_valves: u64,
    open_valves: u64,
}

struct NetworkStateOpenValveIter<'a> {
    state: &'a NetworkState,
    bitnum: u32,
}

impl<'a> Iterator for NetworkStateOpenValveIter<'a> {
    type Item = ValveId;
    fn next(&mut self) -> Option<ValveId> {
        while self.bitnum <= 63 {
            let id = ValveId(self.bitnum);
            self.bitnum += 1;
            if self.state.is_open(&id) {
                return Some(id);
            }
        }
        None
    }
}

impl NetworkState {
    fn bit(id: &ValveId) -> u64 {
        match 1_u64.checked_shl(id.0) {
            Some(n) => n,
            None => {
                panic!("valve ids should be no greater than 63: {}", id.0);
            }
        }
    }

    fn add_valve(&mut self, id: &ValveId) {
        self.all_valves |= NetworkState::bit(&id)
    }

    fn open_valve(&mut self, id: &ValveId) {
        self.open_valves |= NetworkState::bit(id)
    }

    fn close_valve(&mut self, id: &ValveId) {
        self.open_valves &= !NetworkState::bit(id)
    }

    fn is_open(&self, id: &ValveId) -> bool {
        self.open_valves & NetworkState::bit(id) != 0
    }

    fn is_closed(&self, id: &ValveId) -> bool {
        self.open_valves & NetworkState::bit(id) == 0
    }

    fn open_valves(&self) -> NetworkStateOpenValveIter {
        NetworkStateOpenValveIter {
            state: self,
            bitnum: 1,
        }
    }

    fn with_opened_valve(&self, valve: &ValveId) -> NetworkState {
        NetworkState {
            all_valves: self.all_valves,
            open_valves: self.open_valves | NetworkState::bit(valve),
        }
    }
}

#[test]
fn test_network_state_valve_changes() {
    let mut state: NetworkState = Default::default();
    assert!(!state.is_open(&ValveId(1)));
    assert!(!state.is_open(&ValveId(2)));
    assert!(!state.is_open(&ValveId(3)));
    state.open_valve(&ValveId(2));
    assert!(!state.is_open(&ValveId(1)));
    assert!(state.is_open(&ValveId(2)));
    assert!(!state.is_open(&ValveId(3)));
    state.close_valve(&ValveId(2));
    assert!(!state.is_open(&ValveId(1)));
    assert!(!state.is_open(&ValveId(2)));
    assert!(!state.is_open(&ValveId(3)));
}

#[test]
fn test_network_state_iteration() {
    let mut state: NetworkState = Default::default();
    state.open_valve(&ValveId(1));
    state.open_valve(&ValveId(2));
    state.open_valve(&ValveId(3));
    state.open_valve(&ValveId(63));
    let v: Vec<ValveId> = state.open_valves().collect();
    assert_eq!(v, vec![ValveId(1), ValveId(2), ValveId(3), ValveId(63)]);
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum Action {
    Move(ValveId),
    Open,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
struct State {
    location: ValveId,
    valves: NetworkState,
}

impl State {
    fn this_valve_is_closed(&self) -> bool {
        self.valves.is_closed(&self.location)
    }

    fn new(network: &Network, begin: &ValveId) -> State {
        State {
            location: *begin,
            valves: network.initial_state(),
        }
    }

    fn with_opened_valve(&self) -> State {
        State {
            location: self.location,
            valves: self.valves.with_opened_valve(&self.location),
        }
    }

    fn with_move(&self, whence: &ValveId) -> State {
        State {
            location: *whence,
            valves: self.valves,
        }
    }

    fn neighbouring_states(&self, network: &Network) -> Vec<(u32, Action, State)> {
        let mut result = Vec::new();
        let valve = &network.valves[&self.location];
        if self.this_valve_is_closed() {
            let newvalves = self.valves.with_opened_valve(&self.location);
            assert!(newvalves != self.valves);
            result.push((
                valve.flow_rate,
                Action::Open,
                State {
                    location: self.location,
                    valves: newvalves,
                },
            ));
        }
        result.extend(
            valve
                .neighbours
                .iter()
                .map(|n| (0, Action::Move(*n), self.with_move(n))),
        );
        result
    }
}

fn best_action(
    verbose: bool,
    total_minutes: u32,
    minutes_left: u32,
    current: State,
    history: &mut Vec<Option<Action>>,
    network: &Network,
) -> (u32, Option<Action>, State) {
    if minutes_left == 0 {
        return (0, None, current);
    }

    let mut best: Option<(u32, Action, State)> = None;

    let mut consider = |(flow_change, action, newstate): (u32, Action, State)| match best {
        None => {
            best = Some((flow_change, action, newstate));
        }
        Some((existing_change, existing_action, _)) => {
            if existing_change < flow_change
                || (existing_change == flow_change && existing_action == Action::Open)
            {
                best = Some((flow_change, action, newstate));
            }
        }
    };

    let valve = match network.valves.get(&current.location) {
        Some(v) => v,
        None => {
            dbg!(&current);
            panic!(
                "we are in location {} but there is no such valve in the network",
                current.location.0
            );
        }
    };
    for (flow_delta, action, next_state) in current.neighbouring_states(&network) {
        let (flow, maybe_action, state) = best_action(
            verbose,
            total_minutes,
            minutes_left - 1,
            next_state,
            history,
            network,
        );
        history.pop();
        let total_flow = flow + flow_delta;
        consider((total_flow, action, state));
    }
    let result = match best {
        None => {
            history.push(None);
            (0, None, current)
        }
        Some((flow, action, newstate)) => {
            history.push(Some(action));
            (flow, Some(action), newstate)
        }
    };
    if verbose {
        println!(
            "At minute {} in location {}, best action in state {:b} is {:?} for flow {}",
            total_minutes - minutes_left,
            current.location.0,
            current.valves.open_valves,
            result.1,
            result.0
        );
    }
    result
}

#[test]
fn test_best_action() {
    let nw = Network::try_from(concat!(
        "Valve AA has flow rate=0; tunnel leads to valve DD\n",
        "Valve DD has flow rate=20; tunnel leads to valve AA\n",
    ))
    .expect("test_best_action input should be valid");

    let aa: ValveId = *nw
        .name_to_id
        .get(&ValveName('A', 'A'))
        .expect("start point AA should not be missing");
    let dd: ValveId = *nw
        .name_to_id
        .get(&ValveName('D', 'D'))
        .expect("valve DD should not be missing");

    // Best plan is:
    // 3 minutes remaining: move to DD; flow=0
    // 2 minutes remaining: open DD; flow=0
    // 1 minutes remaining: action unimportant, 1 minute of flow at rate 20
    let state = State::new(&nw, &aa);
    let mut history = Vec::new();
    let (flow, action, _next_state) = best_action(true, 3, 3, state, &mut history, &nw);
    dbg!(&history);
    assert_eq!(action, Some(Action::Move(dd)));
    assert_eq!(flow, 20);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).expect("valid input");
    let network = Network::try_from(input).expect("input should be valid");
    let aa = network
        .name_to_id
        .get(&ValveName('A', 'A'))
        .expect("start point AA should not be missing");
    let state = State::new(&network, &aa);
    let mut history = Vec::new();
    println!(
        "{:?}",
        best_action(false, 30, 16, state, &mut history, &network)
    );
    dbg!(&history);
}
