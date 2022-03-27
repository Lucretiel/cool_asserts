use std::{any::Any, ops::Deref};

/// Assert that an expression panics.
///
/// This macro asserts that a given expression panics. If the expression
/// doesn't panic, this macro will panic, with a description including the
/// expression itself, as well as the return value of the expression.
///
/// This macro is intended to replace `#[should_panic]` in rust tests:
///
/// - It allows for checking that a particular expression or statement panics,
///   rather than an entire test function.
/// - It allows checking for the presence or absence of multiple substrings.
///   `should_panic` can only test for a single substring (with `expected`),
///   and can't test for absence at all.
/// - It allows checking for multiple panicking expressions in the same test
///   case
///
/// ## Examples
///
/// ```
/// // This example passes
/// use cool_asserts::assert_panics;
///
/// assert_panics!({
///     let _x = 1 + 2;
///     panic!("Panic Message");
/// });
/// ```
/// ```should_panic
/// // This example panics
/// use cool_asserts::assert_panics;
///
/// assert_panics!(1 + 2);
/// ```
///
/// # Substring checking
///
/// Optionally, provide a list of conditions of the form `includes(pattern)`
/// or `excludes(pattern)`. If given, the assertion will check that the
/// panic message contains or does not contain each of the given patterns.
/// It uses [`str::contains`], which means that anything
/// implementing [`Pattern`][::std::str::Pattern] can be used as a matcher.
///
/// ## Examples
///
/// ```
/// use cool_asserts::assert_panics;
///
/// assert_panics!(
///     panic!("Custom message: {}", "message"),
///     includes("message: message"),
///     includes("Custom"),
///     excludes("Foo"),
/// );
/// ```
///
/// ```should_panic
/// // THIS EXAMPLE PANICS
///
/// use cool_asserts::assert_panics;
///
/// // The expression panics, which passes the assertion, but it fails
/// // the message test, which means the overall assertion fails.
/// assert_panics!(
///     panic!("Message"),
///     excludes("Message")
/// );
/// ```
///
/// # Generic message testing
///
/// If the `includes(..)` and `excludes(..)` are not powerful enough,
/// `assert_panics` can also accept a closure as an argument. If the expression
/// panics, the closure is called with the panic message as an `&str`. This
/// closure can contain any additional assertions you wish to perform on the
/// panic message.
///
/// ## Examples
///
/// ```
/// use cool_asserts::assert_panics;
///
/// assert_panics!(panic!("{}, {}!", "Hello", "World"), |msg| {
///     assert_eq!(msg, "Hello, World!")
/// });
/// ```
/// ```
/// use cool_asserts::assert_panics;
///
/// assert_panics!(
///     assert_panics!(
///         panic!("Message"),
///         |msg| panic!("Custom Message")
///     ),
///     includes("Custom Message"),
///     excludes("assertion failed")
/// )
/// ```
///
/// # Generic panic values
///
/// If you need to test panics that aren't event messages– that is, that aren't
/// [`String`] or `&str` values provided by most panics (including most
/// assertions)– you can provide a reference type in the closure. The macro
/// will attempt to cast the panic value to that type. It will fail the
/// assertion if the type doesn't match; otherwise the closure will be called.
///
/// ```
/// use cool_asserts::assert_panics;
///
/// assert_panics!(std::panic::panic_any(10i64), |value: &i64| {
///     assert_eq!(value, &10);
/// })
/// ```
///
/// ```
/// use cool_asserts::assert_panics;
///
/// assert_panics!(
///     assert_panics!(
///         std::panic::panic_any(10i64),
///         |value: &i32| {
///             panic!("CUSTOM PANIC");
///         }
///     ),
///     includes("expression panic type mismatch"),
///     includes("i32"),
///     // Currently, type_name of Any returns &dyn Any, rather than the actual type
///     // includes("i64"),
///     excludes("expression didn't panic"),
///     excludes("CUSTOM PANIC")
/// );
/// ```
#[macro_export]
macro_rules! assert_panics {
    ($expression:expr, |$panic:ident: Box<$(dyn)? Any $(+ Send)? $(+ 'static)?>| $body:expr) => (
        match ::std::panic::catch_unwind(|| {$expression}) {
            Ok(result) => $crate::assertion_failure!(
                "expression didn't panic",
                expression: stringify!($expression),
                returned debug: result,
            ),
            Err($panic) => $body,
        }
    );

    ($expression:expr, |$msg:ident $(: &str)?| $body:expr) => (
        $crate::assert_panics!($expression, |panic: Box<dyn Any>|
            match $crate::get_panic_message(&panic) {
                Some($msg) => $body,
                None => $crate::assertion_failure!(
                    "expression panic type wasn't String or &str",
                    expression: stringify!($expression),
                ),
            }
        )
    );

    ($expression:expr, |$value:ident: &$type:ty| $body:expr) => (
        $crate::assert_panics!($expression, |panic: Box<dyn Any>|
            match panic.downcast_ref::<$type>() {
                Some($value) => $body,
                None => $crate::assertion_failure!(
                    "expression panic type mismatch",
                    expression: stringify!($expression),
                    expected: stringify!($type),
                )
            }
        )
    );

    ($expression:expr $(,)?) => (
        $crate::assert_panics!($expression, |_panic: Box<dyn Any>| {})
    );


    ($expression:expr $(, $rule:ident($arg:expr))+ $(,)?) => {
        $crate::assert_panics!($expression, |msg| {
            $($crate::check_rule!($expression, msg, $rule($arg));)+
        })
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! check_rule {
    ($expression:expr, $msg:ident, includes($incl:expr)) => (
        if !$msg.contains($incl) {
            $crate::assertion_failure!(
                "panic message didn't include expected string",
                expression: stringify!($expression),
                message debug: $msg,
                includes debug: $incl,
            );
        }
    );

    ($expression:expr, $msg:ident, excludes($excl:expr)) => (
        if $msg.contains($excl) {
            $crate::assertion_failure!(
                "panic message included disallowed string",
                expression: stringify!($expression),
                message debug: $msg,
                excludes debug: $excl,
            );
        }
    );
}

#[cfg(test)]
mod test_assert_panics {
    use std::panic::panic_any;

    use crate::assert_panics;
    mod bootstrap_tests {
        use crate::assert_panics;

        // We use a few should_panic tests to bootstrap– ensure that assert_panics
        // basically works– then we do the more complex testing with itself.
        // If all these tests pass, assert_panics has at least enough working
        // that it can test itself more thoroughly.
        #[test]
        fn passes_with_panic() {
            assert_panics!(panic!());
        }

        #[test]
        #[should_panic(expected = "expression didn't panic")]
        fn fails_without_panic() {
            assert_panics!(1 + 1);
        }

        #[test]
        #[should_panic(expected = "panic message didn't include expected string")]
        fn fails_without_substring() {
            assert_panics!(panic!("{} {}", "This", "Message"), includes("That Message"))
        }

        #[test]
        #[should_panic(expected = "panic message included disallowed string")]
        fn fails_with_substring() {
            assert_panics!(panic!("{} {}", "This", "Message"), excludes("Message"))
        }
    }

    #[test]
    fn fails_without_panic() {
        assert_panics!(
            assert_panics!(1 + 1),
            includes("assertion failed at"),
            includes("expression didn't panic"),
            includes("expression: 1 + 1"),
            includes("returned: 2")
        );
    }

    #[test]
    fn fails_without_substring() {
        assert_panics!(
            assert_panics!(panic!("{} {}", "This", "Message"), includes("That Message")),
            includes("assertion failed at"),
            excludes("expression didn't panic"),
            includes("panic message didn't include expected string"),
            includes("expression: panic!(\"{} {}\", \"This\", \"Message\")"),
            includes(" message: \"This Message\""),
            includes("includes: \"That Message\""),
        );
    }

    #[test]
    fn fails_with_substring() {
        assert_panics!(
            assert_panics!(panic!("{} {}", "This", "Message"), excludes("Message")),
            includes("assertion failed at"),
            excludes("expression didn't panic"),
            includes("panic message included disallowed string"),
            includes("expression: panic!(\"{} {}\", \"This\", \"Message\")"),
            includes(" message: \"This Message\""),
            includes("excludes: \"Message\""),
        );
    }

    #[test]
    fn string_closure() {
        assert_panics!(panic!("{}, {}!", "Hello", "World"), |msg| {
            assert_eq!(msg, "Hello, World!");
        })
    }

    #[test]
    fn str_closure() {
        assert_panics!(panic!("Hello, World!"), |msg| {
            assert_eq!(msg, "Hello, World!");
        })
    }

    #[test]
    fn str_closure_mismatch() {
        assert_panics!(
            assert_panics!(panic_any(32), |_msg| {
                panic!("CUSTOM PANIC");
            }),
            includes("assertion failed at"),
            includes("expression panic type wasn't String or &str"),
            excludes("CUSTOM PANIC")
        )
    }

    #[test]
    fn str_closure_panics() {
        assert_panics!(
            assert_panics!(panic!("Hello, World!"), |_msg| {
                panic!("NOPE");
            }),
            includes("NOPE"),
            excludes("Hello, World!"),
        );
    }

    #[test]
    fn typed_closure() {
        assert_panics!(panic_any(244i32), |value: &i32| {
            assert_eq!(value, &244);
        })
    }

    #[test]
    fn typed_closure_mismatch() {
        assert_panics!(
            assert_panics!(panic_any(244i32), |_value: &i64| { panic!("CUSTOM PANIC") }),
            excludes("CUSTOM PANIC"),
            includes("expression panic type mismatch"),
            includes("expression: panic_any(244i32)"),
            includes("expected: i64"),
        );
    }
}

/// Get the panic message as a `&str`, if available
///
/// While a panic value can be any type, *usually* it is either a `String`
/// or a `str`, hidden inside a `Box<dyn Any + Send + Sync>` (see
/// [`std::thread::Result`] for more info). This function gets the panic
/// message as an &str (if available) from a panic value.
///
/// ```
/// use std::panic::catch_unwind;
/// use cool_asserts::get_panic_message;
///
/// let result = catch_unwind(|| panic!("{}, {}!", "Hello", "World"));
/// let panic = result.unwrap_err();
/// let message = get_panic_message(&panic).unwrap();
///
/// assert_eq!(message, "Hello, World!");
#[inline]
pub fn get_panic_message(panic: &Box<dyn Any + Send>) -> Option<&str> {
    panic
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| panic.downcast_ref::<&'static str>().map(Deref::deref))
}

#[cfg(test)]
mod test_get_panic_message {
    use super::get_panic_message;
    use std::panic::{catch_unwind, panic_any};

    #[test]
    fn str_message() {
        let result =
            catch_unwind(|| panic!("Hello, World!")).expect_err("Function didn't panic????");
        assert_eq!(get_panic_message(&result), Some("Hello, World!"));
    }

    #[test]
    fn string_message() {
        let result = catch_unwind(|| panic_any("Hello, World!".to_owned()))
            .expect_err("Function didn't panic????");

        assert_eq!(get_panic_message(&result), Some("Hello, World!"));
    }

    #[test]
    fn formatted_message() {
        let result = catch_unwind(|| panic!("{}, {}!", "Hello", "World"))
            .expect_err("Function didn't panic????");
        assert_eq!(get_panic_message(&result), Some("Hello, World!"));
    }

    #[test]
    fn other_message() {
        let result = catch_unwind(|| panic_any(25)).expect_err("Function didn't panic????");
        assert_eq!(get_panic_message(&result), None);
    }
}
