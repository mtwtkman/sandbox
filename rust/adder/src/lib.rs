//! module `adder`
//! # Examples
//!
//! ```
//! assert_eq!(4, adder::add_two(2));
//! ```
//!

/// function `add_two`
/// # Exmples
///
/// ```
/// use adder::add_two;
///
/// assert_eq!(4, add_two(2));
/// ```

pub fn add_two(a: i32) -> i32 {
    a + 2
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(4, add_two(2));
    }
}
