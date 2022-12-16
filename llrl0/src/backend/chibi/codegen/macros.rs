macro_rules! eval_continues {
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
