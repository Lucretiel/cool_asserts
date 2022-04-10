mod iter_all;
mod iter_split;

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

`assert_matches` can handle matching on iterators. If the pattern is a `[..]`
slice pattern, the test value is treated as an iterator, and each item in the
iterator is matched

*/
#[macro_export]
macro_rules! assert_matches {
    // Disallow iterator matches with only a single .. parameter and no other
    // parameters (since this is an infallible no-op)
    (
        $expression:expr,
        [
            $($($name:ident)+ @)? .. $(if $guard:expr)? $(=> $block:expr)? $(,)?
        ] $(,)?
    ) => {{
        compile_error!("[..] is an infallible iterator pattern")
    }};


    (
        $expression:expr,
        [
            $($patterns:tt)*
        ] $(if $guard:expr)? => $block:expr $(,)?
    ) => {{
        let mut iterator = ::core::iter::IntoIterator::into_iter($expression);
        let target_length = $crate::compute_target_length!(0; $($patterns)*);

        $crate::assert_matches_iter_all!(
            $(if: $guard,)?
            block: $block,
            iter: iterator,
            index: 0,
            expected_length: target_length,
            patterns: [ $($patterns)* ],
        )
    }};

    (
        $expression:expr,
        [
            $($patterns:tt)*
        ] $(,)?
    ) => {{
        let mut iterator = ::core::iter::IntoIterator::into_iter($expression);
        let target_length = $crate::compute_target_length!(0; $($patterns)*);

        $crate::assert_matches_iter_split!(
            iter: iterator,
            index: 0,
            expected_length: target_length,
            collected: (),
            patterns: [ $($patterns)* ],
        )
    }};



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
                $($(; $fmt_pattern $($fmt_arg)+)?)?
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
