pub fn result_binop<T, Q, E, F>(
    left_operand: Result<T, E>,
    right_operand: Result<Q, E>,
    op: F,
) -> Result<T, E>
where
    Q: Into<T>,
    F: Fn(T, T) -> T,
{
    Ok(op(left_operand?, right_operand?.into()))
}

pub fn sum_result<T, Q, E>(total: Result<T, E>, current: Result<Q, E>) -> Result<T, E>
where
    T: std::ops::Add<Output = T>,
    Q: Into<T>,
{
    result_binop(total, current, |a, b| a + b)
}

#[test]
fn test_sum_result() {
    let input: Vec<Result<i32, ()>> = vec![Ok(1), Ok(2), Ok(800)];
    let total: Result<i32, ()> = input.into_iter().fold(Ok(-1), sum_result);
    assert!(matches!(total, Ok(802)), "{total:?}");
}

#[test]
fn test_sum_result_empty() {
    let input: Vec<Result<i32, ()>> = vec![];
    let total: Result<i32, ()> = input.into_iter().fold(Ok(0), sum_result);
    assert!(matches!(total, Ok(0)), "{total:?}");
}

#[test]
fn test_sum_result_widen() {
    let input: Vec<Result<i32, ()>> = vec![];
    let total: Result<i64, ()> = input.into_iter().fold(Ok(0_i64), sum_result);
    assert!(matches!(total, Ok(0_i64)), "{total:?}");
}

#[test]
fn test_sum_result_of_conversion() {
    let input: Vec<&str> = vec!["1", "2"];
    let total: Result<i64, _> = input
        .iter()
        .map(|s| s.parse::<i64>())
        .fold(Ok(0_i64), sum_result);
    assert!(matches!(total, Ok(3_i64)), "{total:?}");
}

#[test]
fn test_sum_result_propagate_error() {
    let input: Vec<&str> = vec!["1", "2", "not-a-number"];
    let total: Result<i64, _> = input
        .iter()
        .map(|s| s.parse::<i64>())
        .fold(Ok(0_i64), sum_result);
    assert!(matches!(total, Err(_)), "{total:?}");
}
