/// Compute the max of list of expressions.
///
/// This is given as a macro so that it can be fully evaluated in a const
/// context
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

/// Get a partial format string for an assertion_failure key-value pair given
/// the spec
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

/// Get a partial format string for an assertion_failure trailing message
#[macro_export]
#[doc(hidden)]
macro_rules! make_assertion_failure_tail {
    () => {};
    ($($_arg:tt)+) => {
        "\n{}"
    };
}

/// Returns the first token and discards all subsequent tokens. Used as a
/// helper in macro repetitios that don't need to make use of the repeated
/// variable, like count!
#[macro_export]
#[doc(hidden)]
macro_rules! discard_tail {
    ($token:tt $($_discard:tt)*) => {
        $token
    };
}

/// Count the number of top-level tokens
///
/// count!(1 2) => 2
/// count!((1 2) 3) => 2
#[doc(hidden)]
#[macro_export]
macro_rules! count {
    ( $( $thing:tt )* ) => {
        0 $(
            + $crate::discard_tail!(1 $thing)
        )*
    }
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
///  r#"assertion failed at src/assertion_failure.rs:7: `(Assertion Message)`
///       key: 10
///  long_key: "Hello\tWorld!"
///   Trailing message: Hello, World!"#
///    );
/// ```
#[macro_export]
macro_rules! assertion_failure {
    ($message:literal $($(, $key:ident $($spec:ident)? : $value:expr)+)? $(; $( $fmt_pattern:literal $($fmt:tt)* )?)? ) => {
        panic!(
            concat!(
                "assertion failed at {file}:{line}: `({message})`",

                // This inserts a series of "\n{}: {}" fields for (key, value) pairs
                $($($crate::make_assertion_failure_fmt!($($spec)?),)+)?

                // This inserts a trailing "\n{}" if there is a fmt messag
                $($($crate::make_assertion_failure_tail!($fmt_pattern))?)?
            ),
            $($(stringify!($key), $value,)+)?
            $($(
                $crate::indent_write::indentable::Indented{
                    item: format_args!($fmt_pattern $($fmt)+),
                    indent: "  ",
                },
            )?)?
            message=$message,
            file=file!(),
            line=line!(),
            $(padding = $crate::max!($(stringify!($key).len(),)+),)?
        )
    };

    ($message:literal $($(, $key:ident $($spec:ident)? : $value:expr)+)? ,) => {
        $crate::assertion_failure!($message $($(, $key $($spec)?: $value)+)?)
    };
}

#[cfg(test)]
mod assertion_failure_tests {
    use crate::{assert_panics, assertion_failure};
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

    #[test]
    fn mutliline_trailing_fmt() {
        assert_panics!(
            assertion_failure!(
                "Failure Message";
                "{}\n\n{}", "Hello", "World"
            ),
            includes("assertion failed at"),
            includes("Failure Message"),
            includes("\n  Hello\n\n  World")
        )
    }
}
