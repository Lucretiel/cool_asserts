//! A collection of useful assertions and other utilities for writing
//! tests in Rust.

use std::any::Any;
use std::ops::Deref;

/// Get the type name of a value as a string. Same as [`std::any::type_name`],
/// but it operates on a value, which allows it to work on inferred types.
#[inline]
#[doc(hidden)]
pub fn ref_type_name<T: ?Sized>(_: &T) -> &'static str {
    ::core::any::type_name::<T>()
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
    use std::panic::catch_unwind;

    #[test]
    fn str_message() {
        let result =
            catch_unwind(|| panic!("Hello, World!")).expect_err("Function didn't panic????");
        assert_eq!(get_panic_message(&result), Some("Hello, World!"));
    }

    #[test]
    fn string_message() {
        let result = catch_unwind(|| panic!("Hello, World!".to_string()))
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
        let result = catch_unwind(|| panic!(25)).expect_err("Function didn't panic????");
        assert_eq!(get_panic_message(&result), None);
    }
}

/// Compute the max of list of expressions.
///
/// This is given as a macro so that it can be fully evaluated at compile
/// time.
///
/// It guarantees that:
///
/// - All the expressions are evaluated in order, if relevant
/// - Each expression is only evaluated once.
#[macro_export]
#[doc(hidden)]
macro_rules! max {
    ($value:expr $(,)?) => ($value);
    ($lhs:expr $(, $tail:expr)+ $(,)?) => {{
        let lhs = $lhs;
        let tail = $crate::max!($($tail),+);
        if lhs < tail { tail } else { lhs }
    }}
}

/// Get a partial format string for assertion_failure given the spec
#[macro_export]
#[doc(hidden)]
macro_rules! make_assertion_failure_fmt {
    () => {
        "\n {:>padding$}: {}"
    };
    (debug) => {
        "\n {:>padding$}: {:#?}"
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! make_assertion_failure_tail {
    () => {};
    ($($_arg:tt)+) => {
        "\n  {}"
    };
}

/// Panic with an assertion failure message
///
/// This macro is used in failing assertions to issue a panic with a message
/// in a standardized format, which includes:
///
/// - A static description of what went wrong.
/// - The file and line number of the error.
/// - Optionally, key-value components of the failed assertion (for example,
///   the `lhs` and `rhs` of a failed `assert_eq!`). These components are
///   visually aligned for an easier debugging experience.
/// - Optionally, a trailing formatted message.
///
/// The format of this macro is designed to be similar to the message printed
/// by [`assert_eq!`] or [`assert_ne!`]. The exact output is not currently
/// guaranteed to be identical with matching semver versions, though this may
/// change in the future.
///
/// # Example:
///
/// ```
/// use std::panic::catch_unwind;
/// use cool_asserts::{assertion_failure, get_panic_message};
///
/// let result = catch_unwind(|| assertion_failure!(
///     "Assertion Message",
///     // Add key-value data to the debug message
///     key: 10,
///
///     // Use the "debug" keyword to debug-print something.
///     // Additionally, notice in the output that the key and long-key
///     // are visually aligned.
///     long_key debug: "Hello\tWorld!";
///
///     // After a semicolon, add a trailing format message
///     "Trailing message: {}, {}!", "Hello", "World"
/// ));
/// let panic = result.unwrap_err();
/// let message = get_panic_message(&panic).unwrap();
/// assert_eq!(
///     message,
///  r#"assertion failed at src/lib.rs:7: `(Assertion Message)`
///       key: 10
///  long_key: "Hello\tWorld!"
///   Trailing message: Hello, World!"#
///    );
/// ```
#[macro_export]
macro_rules! assertion_failure {
    ($message:literal $($(, $key:ident $($spec:ident)?: $value:expr)+)? $(; $($fmt:tt)+)? ) => {
        panic!(
            concat!(
                "assertion failed at {file}:{line}: `({message})`",

                // This inserts a series of "\n{}: {}" fields for (key, value) pairs
                $($($crate::make_assertion_failure_fmt!($($spec)?),)+)?

                // This inserts a trailing "\n{}" if there is a fmt messag
                $($crate::make_assertion_failure_tail!($($fmt)+),)?
            ),
            $($(stringify!($key), $value,)+)?
            $(format_args!($($fmt)+),)?
            message=$message,
            file=file!(),
            line=line!(),
            $(padding = $crate::max!($(stringify!($key).len(),)+),)?
        )
    };

    ($message:literal $($(, $key:ident $($spec:ident)?: $value:expr)+)? ,) => {
        $crate::assertion_failure!($message $($(, $key $($spec)?: $value)+)?)
    };

    ($message:literal $($(, $key:ident $($spec:ident)?: $value:expr)+)? ;) => {
        $crate::assertion_failure!($message $($(, $key $($spec)?: $value)+)?)
    };
}

#[cfg(test)]
mod assertion_failure_tests {
    use super::{assert_panics, assertion_failure};
    #[test]
    fn just_message() {
        assert_panics!(
            assertion_failure!("Failure Message"),
            includes("assertion failed at"),
            includes("Failure Message"),
            excludes("\n"),
        )
    }

    #[test]
    fn keys_aligned() {
        assert_panics!(
            assertion_failure!(
                "Failure Message",
                key: 10,
                long_key: 20,
            ),
            includes("assertion failed at"),
            includes("Failure Message"),
            includes("     key: 10\n"),
            includes("long_key: 20"),
        );
    }

    #[test]
    fn keys_trailing_fmt() {
        assert_panics!(
            assertion_failure!(
                "Failure Message",
                key: 10,
                long_key: 20;
                "{}, {}!", "Hello", "World"
            ),
            includes("assertion failed at"),
            includes("Failure Message"),
            includes("     key: 10\n"),
            includes("long_key: 20\n"),
            includes("\n  Hello, World!")
        );
    }
}

/// Assert that the value of an expression matches a given pattern.
///
/// This assertion checks that the value of an expression matches a pattern.
/// It panics if the expression does not match.
///
/// Optionally, provide a block with `=> { <block> }`; the block will be run
/// with the matched pattern if the match was successful. This allows running
/// additional assertions on the matched pattern.
///
/// You can also add a trailing format message that will be included with
/// the panic in the event of an assertion failure, just like with
/// [`assert_eq!`].
///
/// # Example
///
/// ```
/// use cool_asserts::{assert_matches, assert_panics};
///
/// assert_panics!{
///     assert_matches!(Some(10), None),
///     includes("value does not match pattern"),
/// }
///
/// assert_matches!(Some(10), Some(x) => {
///     assert_eq!(x, 10);
/// })
/// ```
#[macro_export]
macro_rules! assert_matches {
    ($expression:expr, $( $pattern:pat )|+ $(if $guard:expr)? $(=> $block:expr)?, ) => {
        $crate::assert_matches!($expression, $( $pattern )|+ $(if $guard)? $(=> $block)?)
    };
    ($expression:expr, $( $pattern:pat )|+ $(if $guard:expr)? $(=> $block:expr)? $(, $($fmt_arg:tt)+)?) => ({
        match $expression {
            $( $pattern )|+ $(if $guard)? => { $($block)? },
            v => $crate::assertion_failure!(
                "value does not match pattern",
                value debug: v,
                pattern: stringify!($($pattern)|+ $(if $guard)?)
                $(; $($fmt_arg)+)?
            )
        }
    });
}

#[cfg(test)]
mod test_assert_matches {
    use super::{assert_matches, assert_panics};

    #[derive(Debug)]
    struct TestStruct {
        x: isize,
        y: isize,
    }

    const TEST_VALUE: Option<TestStruct> = Some(TestStruct { x: 10, y: 20 });

    #[test]
    fn basic_case() {
        assert_matches!(TEST_VALUE, Some(_));
    }

    #[test]
    fn trailing_comma() {
        assert_matches!(TEST_VALUE, Some(_),);
    }

    #[test]
    fn basic_guard() {
        assert_matches!(TEST_VALUE, Some(v) if v.y == 20);
    }

    #[test]
    fn basic_block() {
        assert_matches!(TEST_VALUE, Some(v) => {
            assert_eq!(v.x, 10);
        })
    }

    #[test]
    fn basic_guard_block() {
        assert_matches!(TEST_VALUE, Some(v) if v.y == 20 => {
            assert_eq!(v.x, 10);
        })
    }

    #[test]
    fn multiple_patterns() {
        #[derive(Debug)]
        enum Thing {
            NoValue,
            Value1(i32),
            Value2(i32),
        }

        assert_matches!(Thing::Value1(10), Thing::Value1(x) | Thing::Value2(x) => assert_eq!(x, 10));
        assert_matches!(Thing::Value2(10), Thing::Value1(x) | Thing::Value2(x) => assert_eq!(x, 10));

        assert_panics!(
            assert_matches!(Thing::NoValue, Thing::Value1(x) | Thing::Value2(x) => assert_eq!(x, 10)),
            includes("pattern: Thing::Value1(x) | Thing::Value2(x)")
        );
    }

    #[test]
    fn basic_failure() {
        assert_panics!(
            assert_matches!(TEST_VALUE, None),
            includes("assertion failed"),
            includes("value does not match pattern"),
            includes("pattern: None"),
            includes(&format!("value: {:#?}", TEST_VALUE))
        );
    }

    #[test]
    fn basic_guard_failure() {
        assert_panics!(
            assert_matches!(TEST_VALUE, Some(v) if v.y == 0),
            includes("assertion failed"),
            includes("value does not match pattern"),
            includes("pattern: Some(v) if v.y == 0"),
            includes(&format!("value: {:#?}", TEST_VALUE))
        );
    }

    #[test]
    fn basic_block_failure() {
        assert_panics!(
            assert_matches!(TEST_VALUE, None => panic!("Custom Panic")),
            includes("assertion failed"),
            includes("value does not match pattern"),
            includes("pattern: None"),
            includes(&format!("value: {:#?}", TEST_VALUE)),
            excludes("Custom Panic"),
        );
    }

    #[test]
    fn failure_in_block() {
        assert_panics!(
            assert_matches!(TEST_VALUE, Some(TestStruct{..}) => panic!("Custom Panic")),
            includes("Custom Panic"),
            excludes("assertion failed"),
            excludes("value does not match pattern"),
        )
    }
}

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
/// will attermpt to cast the panic value to that type. It will fail the
/// assertion if the type doesn't match; otherwise the closure will be called.
///
/// ```
/// use cool_asserts::assert_panics;
///
/// assert_panics!(panic!(10i64), |value: &i64| {
///     assert_eq!(value, &10);
/// })
/// ```
///
/// ```
/// use cool_asserts::assert_panics;
///
/// assert_panics!(
///     assert_panics!(
///         panic!(10i64),
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
                    actual: $crate::ref_type_name(&panic),
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
                expected debug: $incl,
            );
        }
    );

    ($expression:expr, $msg:ident, excludes($excl:expr)) => (
        if $msg.contains($excl) {
            $crate::assertion_failure!(
                "panic message included disallowed string",
                expression: stringify!($expression),
                message debug: $msg,
                disallowed debug: $excl,
            );
        }
    );
}

#[cfg(test)]
mod test_assert_panics {
    use super::assert_panics;
    mod bootstrap_tests {
        use super::assert_panics;

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
            includes("message: \"This Message\""),
            includes("expected: \"That Message\""),
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
            includes("message: \"This Message\""),
            includes("disallowed: \"Message\""),
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
            assert_panics!(panic!(32), |_msg| {
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
        assert_panics!(panic!(244 as i32), |value: &i32| {
            assert_eq!(value, &244);
        })
    }

    #[test]
    fn typed_closure_mismatch() {
        assert_panics!(
            assert_panics!(panic!(244 as i32), |_value: &i64| {
                panic!("CUSTOM PANIC")
            }),
            excludes("CUSTOM PANIC"),
            includes("expression panic type mismatch"),
            includes("expression: panic!(244 as i32)"),
            includes("expected: i64"),
        );
    }
}
