//use regex::Regex;
use sscanf::scanf;
use std::collections::HashMap;
use std::str;

use lib::error::Fail;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
struct NodeId(usize);

#[derive(Debug, Clone)]
struct Node {
    parent: Option<NodeId>,
    name: String,
    // If filesize is Some(n) it's a file.
    filesize: Option<usize>,
}

#[derive(Debug, Clone)]
struct FileSystem {
    nodes: HashMap<NodeId, Node>,
    next_id: NodeId,
    root: Option<NodeId>,
}

fn charge_size_to_parent(
    fs: &FileSystem,
    total_by_node: &mut HashMap<NodeId, usize>,
    node: &Node,
    len: usize,
) {
    if let Some(parent_id) = node.parent {
        if let Some(parent_node) = fs.nodes.get(&parent_id) {
            total_by_node
                .entry(parent_id)
                .and_modify(|size| {
                    *size += len;
                    dbg!(size);
                })
                .or_insert(len);
            charge_size_to_parent(fs, total_by_node, &parent_node, len);
        }
    }
}

impl FileSystem {
    fn new() -> FileSystem {
        FileSystem {
            nodes: HashMap::new(),
            next_id: NodeId(2),
            root: None,
        }
    }

    fn root(&self) -> Option<NodeId> {
        self.root
    }

    fn assign_next_id(&mut self) -> NodeId {
        let result = self.next_id;
        self.next_id = NodeId(self.next_id.0 + 1);
        result
    }

    fn add_dir(&mut self, parent: Option<NodeId>, name: &str) -> NodeId {
        let id = self.assign_next_id();
        let newnode = Node {
            parent,
            name: name.to_string(),
            filesize: None,
        };
        self.nodes.insert(id, newnode);
        if name == "/" {
            self.root = Some(id);
        }
        id
    }

    fn add_file(&mut self, parent: Option<NodeId>, name: &str, len: usize) -> NodeId {
        let id = self.assign_next_id();
        let newnode = Node {
            parent,
            name: name.to_string(),
            filesize: Some(len),
        };
        self.nodes.insert(id, newnode);
        id
    }

    fn transitive_sizes_per_directory(&self) -> Result<HashMap<NodeId, usize>, Fail> {
        let mut total_by_node = HashMap::new();
        self.nodes
            .iter()
            .filter_map(|(_node_id, node)| node.filesize.map(|size| (node, size)))
            .for_each(|(node, size)| {
                println!(
                    "charging size of {} (which is {}) to its parents",
                    node.name, size
                );
                charge_size_to_parent(self, &mut total_by_node, &node, size);
            });
        dbg!(&total_by_node);
        Ok(total_by_node)
    }

    fn find_child_by_name(&self, cwd: Option<NodeId>, name: &str) -> Option<(&NodeId, &Node)> {
        self.nodes
            .iter()
            .filter(|(_node_id, node)| node.parent == cwd)
            .find(|(_, node)| node.name == name)
    }

    fn find_parent_of(&self, node_id: &NodeId) -> Option<NodeId> {
        self.nodes.get(node_id).map(|node| node.parent).flatten()
    }
}

fn parse_commands(s: &str) -> Result<FileSystem, Fail> {
    let mut cwd: Option<NodeId> = None;
    let mut fs: FileSystem = FileSystem::new();

    for command_or_output in s.split_terminator('\n') {
        match command_or_output.strip_prefix("$ ") {
            Some(command) => {
                if command == "ls" {
                    // nothing to do
                } else {
                    match scanf!(command, "cd {str}") {
                        Ok(dirname) => {
                            if dirname == ".." {
                                if let Some(wd) = cwd {
                                    if let Some(parent_id) = fs.find_parent_of(&wd) {
                                        cwd = Some(parent_id);
                                    } else {
                                        panic!("failed to find parent");
                                    }
                                } else {
                                    panic!("you have to cd down before you can cd ..");
                                }
                            } else {
                                match fs.find_child_by_name(cwd, dirname) {
                                    Some((child_id, _child)) => {
                                        cwd = Some(*child_id);
                                    }
                                    None => {
                                        eprintln!(
					"the user did cd {} (with cwd {:?}) without having seen {} in ls output!",
					dirname, cwd, dirname
				    );
                                        cwd = Some(fs.add_dir(cwd, dirname));
                                    }
                                }
                            }
                        }
                        Err(_) => {
                            return Err(Fail(format!("unrecognised command {s}")));
                        }
                    }
                }
            }
            None => match scanf!(command_or_output, "dir {}", str) {
                Ok(dirname) => {
                    println!("saw dir {} (parent {:?}) in ls output", dirname, cwd);
                    fs.add_dir(cwd, dirname);
                }
                Err(_) => match scanf!(command_or_output, "{usize:r10} {str}") {
                    Ok((file_len, filename)) => {
                        println!("saw file {} in ls output", filename);
                        fs.add_file(cwd, filename, file_len);
                    }
                    Err(_) => {
                        return Err(Fail(format!("output is not for a directory or file: {s}")));
                    }
                },
            },
        }
    }
    dbg!(&fs);
    Ok(fs)
}

fn solve_part1(text: &str) -> Result<usize, Fail> {
    let fs = parse_commands(text)?;
    let transitive_sizes = fs.transitive_sizes_per_directory()?;
    dbg!(&transitive_sizes);
    Ok(transitive_sizes
        .iter()
        .filter(|(node_id, size)| {
            match fs.nodes.get(node_id) {
                Some(node) => {
                    println!("size of {} is {}", node.name, size);
                }
                None => {
                    println!("size of {} is {}", node_id.0, size);
                }
            }
            **size <= 100_000
        })
        .map(|(_node, size)| size)
        .sum())
}

#[cfg(test)]
const EXAMPLE: &str = concat!(
    "$ cd /\n",
    "$ ls\n",
    "dir a\n",
    "14848514 b.txt\n",
    "8504156 c.dat\n",
    "dir d\n",
    "$ cd a\n",
    "$ ls\n",
    "dir e\n",
    "29116 f\n",
    "2557 g\n",
    "62596 h.lst\n",
    "$ cd e\n",
    "$ ls\n",
    "584 i\n",
    "$ cd ..\n",
    "$ cd ..\n",
    "$ cd d\n",
    "$ ls\n",
    "4060174 j\n",
    "8033020 d.log\n",
    "5626152 d.ext\n",
    "7214296 k\n",
);

#[test]
fn test_part1_example() {
    assert_eq!(solve_part1(EXAMPLE).expect("example should succeed"), 95437);
}

fn solve_part2(text: &str) -> Result<usize, Fail> {
    let fs = parse_commands(text)?;
    let transitive_sizes = fs.transitive_sizes_per_directory()?;
    let total_space = 70000000;
    let space_needed = 30000000;
    match fs.root() {
        None => Err(Fail("no filesystem root".to_string())),
        Some(root_node_id) => match transitive_sizes.get(&root_node_id) {
            None => Err(Fail("no total usage for root".to_string())),
            Some(space_used) => {
                let space_free = total_space - space_used;
                let to_free = space_needed - space_free;
                let mut options: Vec<usize> = transitive_sizes
                    .into_iter()
                    .filter_map(|(_node_id, size)| if size > to_free { Some(size) } else { None })
                    .collect();
                options.sort();
                match options.iter().next() {
                    Some(n) => Ok(*n),
                    None => Err(Fail("no deletion options identified".to_string())),
                }
            }
        },
    }
}

#[test]
fn test_part2_example() {
    assert_eq!(
        solve_part2(EXAMPLE).expect("example should succeed"),
        24933642
    );
}

fn main() {
    let text = str::from_utf8(include_bytes!("input.txt")).expect("valid input file");
    println!(
        "Day 07 part 1: {}",
        solve_part1(text).expect("should not fail")
    );
    println!(
        "Day 07 part 2: {}",
        solve_part2(text).expect("should not fail")
    );
}
