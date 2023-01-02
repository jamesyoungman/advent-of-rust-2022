use std::fmt::{self, Display, Formatter};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum CompassDirection {
    North,
    South,
    West,
    East,
}

impl CompassDirection {
    pub fn reversed(&self) -> CompassDirection {
        use CompassDirection::*;
        match self {
            North => South,
            South => North,
            East => West,
            West => East,
        }
    }
}

impl Display for CompassDirection {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use CompassDirection::*;
        f.write_str(match self {
            North => "S",
            South => "N",
            East => "W",
            West => "E",
        })
    }
}

impl From<CompassDirection> for char {
    fn from(d: CompassDirection) -> char {
        use CompassDirection::*;
        match d {
            North => 'N',
            East => 'E',
            South => 'S',
            West => 'W',
        }
    }
}

pub const ALL_MOVE_OPTIONS: [CompassDirection; 4] = [
    CompassDirection::North,
    CompassDirection::East,
    CompassDirection::South,
    CompassDirection::West,
];

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct Position {
    pub x: i64,
    pub y: i64,
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{},{}", self.x, self.y)
    }
}

impl Position {
    pub fn move_direction(&self, d: &CompassDirection) -> Position {
        match d {
            CompassDirection::North => Position {
                y: self.y - 1,
                ..*self
            },
            CompassDirection::South => Position {
                y: self.y + 1,
                ..*self
            },
            CompassDirection::East => Position {
                x: self.x + 1,
                ..*self
            },
            CompassDirection::West => Position {
                x: self.x - 1,
                ..*self
            },
        }
    }

    pub fn neighbour_xbearing(&self, to: &Position) -> Result<Option<CompassDirection>, String> {
        match self.x - to.x {
            -1 => Ok(Some(CompassDirection::West)),
            0 => Ok(None),
            1 => Ok(Some(CompassDirection::East)),
            _ => Err(format!(
                "x-coordinates {} and {} are too far apart",
                self.x, to.x
            )),
        }
    }

    pub fn neighbour_ybearing(&self, to: &Position) -> Result<Option<CompassDirection>, String> {
        match self.y - to.y {
            -1 => Ok(Some(CompassDirection::North)),
            0 => Ok(None),
            1 => Ok(Some(CompassDirection::South)),
            _ => Err(format!(
                "y-coordinates {} and {} are too far apart",
                self.y, to.y
            )),
        }
    }
}

pub fn maybe_update_min(min: &mut Option<i64>, val: i64) {
    match min {
        None => {
            *min = Some(val);
        }
        Some(v) if *v > val => *min = Some(val),
        Some(_) => (),
    }
}

pub fn maybe_update_max(max: &mut Option<i64>, val: i64) {
    match max {
        None => {
            *max = Some(val);
        }
        Some(v) if *v < val => *max = Some(val),
        Some(_) => (),
    }
}

pub fn update_min(min: &mut i64, val: i64) {
    if val < *min {
        *min = val;
    }
}

pub fn update_max(max: &mut i64, val: i64) {
    if val > *max {
        *max = val;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct BoundingBox {
    pub top_left: Position,
    pub bottom_right: Position,
}

pub fn bounds<'a, I>(points: I) -> Option<BoundingBox>
where
    I: IntoIterator<Item = &'a Position>,
{
    let mut min_x: Option<i64> = None;
    let mut max_x: Option<i64> = None;
    let mut min_y: Option<i64> = None;
    let mut max_y: Option<i64> = None;
    for p in points.into_iter() {
        maybe_update_min(&mut min_x, p.x);
        maybe_update_max(&mut max_x, p.x);
        maybe_update_min(&mut min_y, p.y);
        maybe_update_max(&mut max_y, p.y);
    }
    match (min_x, max_x, min_y, max_y) {
        (Some(xlow), Some(xhigh), Some(ylow), Some(yhigh)) => Some(BoundingBox {
            top_left: Position { x: xlow, y: ylow },
            bottom_right: Position { x: xhigh, y: yhigh },
        }),
        _ => None,
    }
}

pub fn manhattan(a: &Position, b: &Position) -> i64 {
    let dx = (a.x - b.x).abs();
    let dy = (a.y - b.y).abs();
    dx + dy
}

#[test]
fn test_manhattan() {
    assert_eq!(
        manhattan(&Position { x: 1, y: -2 }, &Position { x: 12, y: 7 }),
        11 + 9
    );
}
