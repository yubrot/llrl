macro_rules! diverges {
    ($e:expr) => {
        match $e {
            Some(_) => panic!("Evaluation must not return"),
            None => return Ok(None),
        }
    };
}

macro_rules! continues {
    ($e:expr) => {
        match $e {
            Some(a) => a,
            None => return Ok(None),
        }
    };
}

macro_rules! unsupported_op {
    ($op:expr) => {
        panic!("Unsupported operation {}", $op)
    };
    ($op:expr, size: $size:expr) => {
        panic!("Unsupported operation {} (size={})", $op, $size)
    };
    ($op:expr, from: $from:expr, to: $to:expr) => {
        panic!("Unsupported operation {} (from={}, to={})", $op, $from, $to)
    };
}
