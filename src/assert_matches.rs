mod iter_all;
mod iter_split;

/// Basically the same as assert_matches, but designed for matching a single
/// item from an iterator assert_matches:
///
/// - Includes an index
/// - allowed to be infallible
#[doc(hidden)]
#[macro_export]
macro_rules! assert_item_matches {
    (
        $index:expr,
        $item:expr,
        $pattern:pat $(if $guard:expr)? $(=> $block:expr)?
        $(, $( $fmt_pattern:literal $($fmt_arg:tt)* )?)?
    ) => {
        match $item {
            $pattern $(if $guard)? => { $($block)? },

            #[allow(unreachable_patterns)]
            ref v => $crate::assertion_failure!(
                "value from iterator does not match pattern",
                index debug: $index,
                value debug: v,
                pattern: stringify!($pattern $(if $guard)?)
                $($(; $fmt_pattern $($fmt_arg)*)?)?
            )
        }
    }
}

/**
Assert that the value of an expression matches a given pattern.

This assertion checks that the value of an expression matches a pattern.
It panics if the expression does not match.

Optionally, provide a block with `=> { <block> }`; the block will be run
with the matched pattern if the match was successful. This allows running
additional assertions on the matched pattern, or returning values to the
caller of `assert_matches`

You can also add a trailing format message that will be included with
the panic in the event of an assertion failure, just like with
[`assert_eq!`].

# Example

```
use cool_asserts::{assert_matches, assert_panics};

assert_panics!{
    assert_matches!(Some(10), None),
    includes("value does not match pattern"),
}

assert_matches!(Some(10), Some(x) => {
    assert_eq!(x, 10);
});

// assert_matches can return a value
let x = assert_matches!(Some(10), Some(x) => x);
assert_eq!(x, 10);
```

# Iteratables

`assert_matches` can handle matching on iterables. If the pattern is a `[..]`
slice pattern, the test value is treated as an iterable, and each item in the
iterator is matched against the corresponding item in the pattern:

```
use cool_asserts::assert_matches;

let data = vec![5, 5].into_iter().chain(Some(4)).enumerate();

assert_matches!(data, [(0, 5), (1, 5), (2, 4)]);
```

In addition to matching the patterns themselves, the iterator must also
precisely match the length of the `[ .. ]` iterator pattern. If the iterator
is too long, `assert_matches` will helpfully include the *first* item that was
in excess of the pattern's length, as well as the total length of the iterator

```
use cool_asserts::{assert_matches, assert_panics};

assert_panics!(
    assert_matches!(vec!['a', 'b', 'c', 'd'], ['a', 'b']),
    includes("too long"),
    includes("actual_length: 4"),
    includes("expected_length: 2"),
    includes("first_overflow: 'c'"),
);

assert_panics!(
    assert_matches!(vec!['a'], ['a', 'b', 'c']),
    includes("too short"),
    includes("actual_length: 1"),
    includes("expected_length: 3"),
);
```

When used in this way, you can use multiple `=> { block }` arms *inside*
the iterator pattern. Each block will be evaluated in order, and a tuple
containing all the block results will be returned from `assert_matches!`. Like
with a regular pattern, you can also include an `if` guard:

```
use cool_asserts::assert_matches;

let data = 1..6;

let (a, (), c) = assert_matches!(data, [
    a => a + 1,
    b => { assert_eq!(b, 2); },
    3,
    4,
    c if c > 2 => c,
]);

assert_eq!(a, 2);
assert_eq!(c, 5);
```

Just like with regular slice patterns, you can use `..` inside the pattern to
match all the items except for those at the beginning and end:

```
use std::iter;
use cool_asserts::assert_matches;

assert_matches!(vec![0, 1, 2, 3, 4, 5], [0, .., 5]);
assert_matches!(vec![0, 1, 2, 3, 4, 5], [.., 4, 5]);

// It supports infinite iterators as long as the
// last pattern in the match is ..
assert_matches!(iter::repeat(3), [3, 3, 3, ..]);
```

Also like with regular slice patterns, you can bind a name to this middle
pattern and use it in the `=> block`. However, because `assert_matches!` is
matching any iterable rather than a slice, the value will itself be an
`Iterator` containing all the middle elements, rather than a subslice:

```
use cool_asserts::assert_matches;

let data = vec![1, 1, 2, 2, 2, 2, 3, 3];

let (middle_sum,) = assert_matches!(
    data,
    [
        1,
        1,
        middle @ .. => middle.reduce(|a, b| a + b),
        3,
        3,
    ]
);

assert_eq!(middle_sum, Some(8));
```
*/
#[macro_export]
macro_rules! assert_matches {
    // Disallow iterator matches with only a single .. parameter and no other
    // parameters (since this is an infallible no-op)
    (
        $expression:expr,
        [
            $($($name:ident)+ @)? .. $(if $guard:expr)? $(=> $block:expr)? $(,)?
        ] $($tail:tt)*
    ) => {{
        compile_error!("[..] is an infallible iterator pattern")
    }};


    // Version with a single trailing block, that is executed with the entire
    // binding
    (
        $expression:expr,
        [
            $($patterns:tt)*
        ] $(if $guard:expr)? => $block:expr
        $(, $( $fmt_pattern:literal $($fmt_arg:tt)* )? )?
    ) => {
        match $expression {
            iterable => {
                let mut iterator = ::core::iter::IntoIterator::into_iter(iterable);
                let target_length = $crate::compute_target_length!(0; $($patterns)*);

                $crate::assert_matches_iter_all!(
                    $(if: $guard,)?
                    block: $block,
                    iter: iterator,
                    index: 0,
                    expected_length: target_length,
                    patterns: [ $($patterns)* ],
                    $($( fmt: ( $fmt_pattern $($fmt_arg)* ), )?)?
                )
            }
        }
    };

    // Version with individual, per-item blocks, that are evaluated
    // independently, in order.
    (
        $expression:expr,
        [
            $($patterns:tt)*
        ]
        $(, $( $fmt_pattern:literal $($fmt_arg:tt)* )? )?
    ) => {
        match $expression {
            iterable => {
                let mut iterator = ::core::iter::IntoIterator::into_iter(iterable);

                let target_length = $crate::compute_target_length!(0; $($patterns)*);

                $crate::assert_matches_iter_split!(
                    iter: iterator,
                    index: 0,
                    expected_length: target_length,
                    collected: (),
                    patterns: [ $($patterns)* ],
                    $($( fmt: ( $fmt_pattern $($fmt_arg)* ), )?)?
                )
            }
        }
    };

    // General purpose version, for non-iterators
    (
        $expression:expr,
        $pattern:pat $(if $guard:expr)? $(=> $block:expr)?
        $(, $( $fmt_pattern:literal $($fmt_arg:tt)* )? )?
    ) => {
        match $expression {
            $pattern $(if $guard)? => { $($block)? },

            ref v => $crate::assertion_failure!(
                "value does not match pattern",
                value debug: v,
                pattern: stringify!($pattern $(if $guard)?)
                $($(; $fmt_pattern $($fmt_arg)*)?)?
            )
        }
    };
}

#[cfg(test)]
mod test_assert_matches {
    use crate::{assert_matches, assert_panics};

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
    fn block_returns() {
        let x = assert_matches!(TEST_VALUE, Some(TestStruct{x, y: 20}) => x);
        assert_eq!(x, 10);
    }

    #[test]
    fn basic_guard_block() {
        assert_matches!(TEST_VALUE, Some(v) if v.y == 20 => {
            assert_eq!(v.x, 10);
        })
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

    #[test]
    fn failure_doesnt_move() {
        #[derive(Debug)]
        struct Thing2 {
            value: i32,
        }

        #[derive(Debug)]
        struct Thing {
            thing2: Thing2,
        }

        let thing = Thing {
            thing2: Thing2 { value: 10 },
        };

        let thing_ref = &thing;

        assert_matches!(thing_ref.thing2, Thing2 { value: 10 })
    }
}
