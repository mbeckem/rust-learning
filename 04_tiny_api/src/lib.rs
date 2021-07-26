/// Multiplies two integers.
/// Panics if the result would overflow.
///
/// _Example_
/// ```
/// use tiny_api::multiply;
/// let result = multiply(2, 3);
/// assert_eq!(result, 6);
/// ```
pub fn multiply(x: i32, y: i32) -> i32 {
    return x.checked_mul(y).expect("multiplication overflowed");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_multiply_numbers() {
        assert_eq!(multiply(4, 2), 8);
    }

    #[test]
    #[should_panic]
    fn should_panic_on_overflow() {
        multiply(1 << 31, 2);
    }
}
