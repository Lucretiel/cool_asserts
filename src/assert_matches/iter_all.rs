#[macro_export]
#[doc(hidden)]
macro_rules! assert_matches_iter_all {
    // Entry point, resembles assert_matches
    (
        $expression:expr,
        [
            $( $pattern:pat ,)*
        ]
        $(if $final_guard:expr)?
        $(=> $block:expr)?,
        $( $fmt_pattern:literal $($fmt_arg:tt)* )?
    ) => {{
        let mut iterator = ::core::iter::IntoIterator::into_iter($expression);
        let target_length = $crate::compute_target_length!($($pattern,)*);

        $crate::assert_matches_iter_all!(
            iter: iterator,
            index: 0,
            target_length: target_length,
            patterns: [ $($pattern $(if $guard)?,)* ]
        )
        }};

    // Base case: no more patterns. Perform a final length check, then evaluate
    // the block
    (
        iter: $iter:ident,
        index: $index:expr,
        target_length: $target:expr,
        patterns: [],
        $(guard: $final_guard:expr,)?
        block: $block:expr,
    ) => {
        match ::core::iter::Iterator::next(&mut $iter) {
            ::core::option::Option::Some(overflow) => $crate::assertion_failure!(
                "iterable was too long",
                actual_length: $index + 1 + ::core::iter::Iterator::count($iter),
                target_length: $index,
                first_overflow debug: overflow,
            ),
            ::core::option::Option::None => {
                $(
                    assert!($final_guard);
                )?

                $block
            }
        }
    };

    // Normal case: a pattern
    (
        iter: $iter:ident,
        index: $index:expr,
        target_length: $target:expr,
        patterns: [
            $pattern:pat $(if $guard:expr)?,
            $($tail_pattern:pat $(if $tail_guard:expr)?,)*
        ],
        $(guard: $final_guard:expr,)?
        block: $block:expr,
    ) => {
        match ::core::iter::Iterator::next(&mut $iter) {
            ::core::option::Option::Some(item) => $crate::assert_matches!(
                item,
                $pattern $(if $guard)? => assert_matches_iter_all!(
                    iter: $iter,
                    index: $index + 1,
                    target_length: $target,
                    patterns: [ $($tail_pattern $(if $tail_guard)?,)* ]
                    $(guard: $final_guard,)?
                    block: $block,
                ),
                "in iterable at index {}", $index
            ),
            ::core::option::Option::None => $crate::assertion_failure!(
                "iterable was too short",
                actual_length: $index,
                target_length: $target,
            )
        };
    };

}
