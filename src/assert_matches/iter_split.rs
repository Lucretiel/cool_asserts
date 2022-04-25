/// Ensure that this combination of name binding, guard, and block is valid.
/// There must be a name binding if and only if there's a guard or block.
#[doc(hidden)]
#[macro_export]
macro_rules! check_bind_guard_block {
    // If there's no name binding, there shouldn't be a guard or a block
    (
        {}
        {$($guard:expr)?}
        {$($block:expr)?}
    ) => {{
        $({
            stringify!($guard);
            compile_error!("can't use an `if` guard in a `..` rest pattern without a `name @` binding")
        })?

        $({
            stringify!($block);
            compile_error!("can't use an `=>` block in a `..` rest pattern without a `name @` binding")
        })?
    }};

    // If a name binding, there should be a guard block
    (
        {$($name:ident)+}
        {}
        {}
    ) => {
        compile_error!("don't use a `name @ ..` binding without a `=>` block or an `if` guard")
    };

    (
        {$($name:ident)+}
        {$($guard:expr)?}
        {$($block:expr)?}
    ) => {{}}
}

#[doc(hidden)]
#[macro_export]
macro_rules! check_guard {
    ($message:literal, $guard:expr ; $($fmt_pattern:literal $($fmt_arg:tt)*)?) => {
        if ! ($guard) {
            $crate::assertion_failure!(
                $message,
                guard: stringify!($guard);
                $($fmt_pattern $($fmt_arg)*)?
            )
        }
    };

    ($message:literal, ; $($fmt_pattern:literal $($fmt_arg:tt)*)?) => {}
}

/// This exists to supress the clippy lint that complains when you end a block
/// with an empty tuple (it wants you to write `{stmt; stmt;}` instead of
/// `{stmt; stmt; ()}`).
#[doc(hidden)]
#[macro_export]
macro_rules! tupleify {
    ($($item:expr,)*) => {($($item,)*)};
}

/// Helper to build an array of precisely the correct length out of the
/// the iterator, where the correct length is the number of patterns in the
/// patterns list.
#[doc(hidden)]
#[macro_export]
macro_rules! build_tail_array {
    (
        iter: $iter:ident,
        index: $index:expr,
        expected_length: $target:expr,
        patterns: [
            $($($pattern:pat $(if $guard:expr)? $(=> $block:expr)?),+ $(,)?)?
        ],
        $(fmt: ( $fmt_pattern:literal $($fmt_arg:tt)* ), )?
    ) => {{
            let mut index = $index;
            let mut iter = ::core::iter::Iterator::inspect(&mut $iter, |_| {index += 1;});

            match (move || Some([$($(
                {
                    stringify!($pattern);
                    ::core::iter::Iterator::next(&mut iter)?
                },
            )+)?]))() {
                ::core::option::Option::Some(array) => array,
                ::core::option::Option::None => $crate::assertion_failure!(
                    "iterable was too short",
                    actual_length: index,
                    expected_length: $target;
                    $($fmt_pattern $($fmt_arg)*)?
                )
            }
        }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! assert_matches_iter_split {
    // Base case: no more patterns. Perform a final length check, then return
    // the collected values.
    (
        iter: $iter:ident,
        index: $index:expr,
        expected_length: $target:expr,
        collected: ($($collected:expr,)*),
        patterns: [],
        $( fmt: ( $fmt_pattern:literal $($fmt_arg:tt)* ), )?
    ) => {
        match ::core::iter::Iterator::next(&mut $iter) {
            ::core::option::Option::Some(overflow) => $crate::assertion_failure!(
                "iterable was too long",
                actual_length: $index + 1 + ::core::iter::Iterator::count($iter),
                expected_length: $index,
                first_overflow debug: overflow;
                $($fmt_pattern $($fmt_arg)*)?
            ),
            ::core::option::Option::None => ($($collected,)*),
        }
    };

    // Rest pattern as the last pattern. We can just forward the iterator
    // directly in this case, potentially even returning it from assert matches.
    (
        iter: $iter:ident,
        index: $index:expr,
        expected_length: $target:expr,
        collected: ($($collected:expr,)*),
        patterns: [
            $($($bind:ident)+ @)? .. $(if $guard:expr)? $(=> $block:expr)? $(,)?
        ],
        $( fmt: ( $fmt_pattern:literal $($fmt_arg:tt)* ), )?
    ) => {{
        // Explicitly discard the target length, in case it wasn't used before
        // now
        let _discard = $target;

        $crate::check_bind_guard_block!(
            {$($($bind)+)?}
            {$($guard)?}
            {$($block)?}
        );

        $(let $($bind)+ = $iter;)?

        $crate::check_guard!(
            ".. trailing rest parameter failed guard",
            $($guard)?;
            $($fmt_pattern $($fmt_arg)*)?
        );

        $crate::tupleify!(
            $($collected,)*
            $($block,)?
        )
    }};

    // Rest pattern in a beginning / middle position. We have to create an
    // iterator that will iterate all but the last N items of the input
    // iterator and pass it to the guard and / or block.
    (
        iter: $iter:ident,
        index: $index:expr,
        expected_length: $target:expr,
        collected: ($($collected:expr,)*),
        patterns: [
            $($($bind:ident)+ @)? .. $(if $guard:expr)? $(=> $block:expr)?,
            $($tail:tt)+
        ],
        $( fmt: ( $fmt_pattern:literal $($fmt_arg:tt)* ), )?
    ) => {{
        $crate::check_bind_guard_block!(
            {$($($bind)+)?}
            {$($guard)?}
            {$($block)?}
        );

        // Create a buffer length N = the number of remaining patterns, and
        // fill it with the next N items from the iterator
        let buffer: [_; $crate::compute_target_length_no_slice!(0; $($tail)+)] = $crate::build_tail_array!(
            iter: $iter,
            index: $index,
            expected_length: $target,
            patterns: [$($tail)+],
            $( fmt: ( $fmt_pattern $($fmt_arg)* ), )?
        );

        // Use LoopBuffer to create an iterator that will yield all but the
        // last N items of the iterator. When this is done, the LoopBuffer will
        // contain those last N items of the iterator, which we will forward to
        // the remaining patterns.
        let mut buffer = $crate::iterators::LoopBuffer::new(buffer);
        let mut index = $index;

        $(stringify!($block); let block_output =)? {
            let iter = buffer.bind_iterator($iter);
            let iter = ::core::iter::Iterator::inspect(iter, |_| {index += 1});
            let iter = ::core::iter::Iterator::fuse(iter);

            // Only make it mutable if we'll need to bind it later
            $(stringify!($($bind)+); let mut iter = iter;)?

            $(stringify!($block); let block_output =)? {
                // We have to forward an &mut version of `iter` to the $block,
                // because we need to retain ownership of it so that it can
                // be drained, in case the $block didn't fully consume it
                // for whatever reason.
                $(let $($bind)+ = $crate::iterators::erase_type(&mut iter); )?

                $crate::check_guard!(
                    ".. rest parameter failed guard",
                    $($guard)? ;
                    $($fmt_pattern $($fmt_arg)*)?
                );

                $($block)?
            };

            // Once the block gets done with the iterator, ensure it's drained
            // the rest of the way. This ensures that `index` is up to date and
            // that `buffer` contains the last N elements from the original
            // iterator.
            ::core::iter::Iterator::for_each(iter, |_| {});

            $(stringify!($block); block_output)?
        };

        let tail = buffer.into_array();
        let mut tail = ::core::iter::IntoIterator::into_iter(tail);

        // Tail is an iterator of the last N items. Recurse back into assert
        // matches for the last N patterns
        $crate::assert_matches_iter_split!(
            iter: tail,
            index: index,
            expected_length: $target,
            collected: ($($collected,)* $({stringify!($block); block_output},)?),
            patterns: [$($tail)+],
            $( fmt: ( $fmt_pattern $($fmt_arg)* ), )?
        )
    }};

    (
        iter: $iter:ident,
        index: $index:expr,
        expected_length: $target:expr,
        collected: ($($collected:expr,)*),
        patterns: [
            $pattern:pat $(if $guard:expr)? $(=> $block:expr)?
            $(, $($tail:tt)*)?
        ],
        $( fmt: ( $fmt_pattern:literal $($fmt_arg:tt)* ), )?
    ) => {{
        $(stringify!($block); let block_output =)? match ::core::iter::Iterator::next(&mut $iter) {
            ::core::option::Option::Some(item) => $crate::assert_item_matches!(
                $index,
                item,
                $pattern $(if $guard)? $(=> $block)?,
                $($fmt_pattern $($fmt_arg)*)?
            ),
            ::core::option::Option::None => $crate::assertion_failure!(
                "iterable was too short",
                actual_length: $index,
                expected_length: $target;
                $($fmt_pattern $($fmt_arg)*)?
            ),
        };

        $crate::assert_matches_iter_split!(
            iter: $iter,
            index: $index + 1,
            expected_length: $target,
            collected: ($($collected,)* $({stringify!($block); block_output} ,)?),
            patterns: [ $($($tail)*)? ],
            $( fmt: ( $fmt_pattern $($fmt_arg)* ), )?
        )
    }}
}

#[cfg(test)]
mod tests {
    use crate::{assert_matches, assert_panics};

    fn get_data() -> impl IntoIterator<Item = Option<i32>> {
        [Some(0), Some(1), None, None, Some(2), Some(3), None]
    }

    #[test]
    fn basic_match() {
        let data = get_data();

        assert_matches!(
            data,
            [Some(_a), Some(1), None, None, Some(2), Some(3), None]
        )
    }

    #[test]
    fn basic_block() {
        let data = get_data();

        let (a, b) = assert_matches!(data, [
            Some(a) => a + 1,
            Some(1),
            None,
            b => b,
            Some(2),
            Some(3),
            None
        ]);

        assert_eq!(a, 1);
        assert_eq!(b, None);
    }

    #[test]
    fn basic_guard() {
        let data = get_data();

        let () = assert_matches!(data, [
            Some(a) if a == 0,
            Some(1),
            None,
            b if b == None,
            Some(2),
            Some(3),
            None
        ]);
    }

    #[test]
    fn basic_guard_block() {
        let data = vec![Some(0), Some(1), None];
        let data = data.into_iter().chain([None, Some(2), Some(3), None]);

        let (a, c) = assert_matches!(data, [
            Some(0),
            Some(a) if a > 0 => a,
            None,
            b if b == None,
            Some(2),
            Some(c) => c,
            None
        ]);

        assert_eq!(a, 1);
        assert_eq!(c, 3);
    }

    #[test]
    fn nameless_temporary_borrow() {
        fn make_vec() -> Vec<u32> {
            vec![2, 3, 4]
        }

        assert_matches!(make_vec().as_slice(), [2, 3, 4]);
    }

    #[test]
    fn iter_failure_mismatch() {
        let data = vec![None, Some(3), None];

        assert_panics!(
            assert_matches!(data, [None, Some(3), Some(3)]),
            includes("assertion failed"),
            includes("value from iterator does not match pattern"),
            includes("  index: 2"),
            includes("  value: None"),
            includes("pattern: Some(3)"),
        );
    }

    #[test]
    fn iter_failure_iter_too_short() {
        let data = vec![1, 2, 3];

        assert_panics!(
            assert_matches!(data, [1, 2, 3, 4, a @ 5 => a, b @ 6 => b]),
            includes("assertion failed"),
            includes("iterable was too short"),
            includes("actual_length: 3"),
            includes("expected_length: 6"),
        );
    }

    /// Test that, if an iterator is too short, the pattern match failure
    /// happens first
    #[test]
    fn iter_failure_iter_too_short_mismatch() {
        let data = vec![1, 2, 3];

        assert_panics!(
            assert_matches!(data, [1, 4, 3, 4, a @ 5 => a, b @ 6 => b]),
            includes("assertion failed"),
            includes("value from iterator does not match pattern"),
            includes("  index: 1"),
            includes("  value: 2"),
            includes("pattern: 4"),
        );
    }

    #[test]
    fn iter_failure_iter_too_long() {
        let data = vec![1, 2, 3];
        let data = data.into_iter().chain([4, 5, 6]);

        assert_panics!(
            assert_matches!(data, [1, 2, 3]),
            includes("assertion failed"),
            includes("iterable was too long"),
            includes("actual_length: 6"),
            includes("expected_length: 3"),
            includes("first_overflow: 4"),
        );
    }

    /// Test that, if an iterator is too long, the pattern match failure
    /// happens first
    #[test]
    fn iter_failure_iter_too_long_mismatch() {
        let data = vec![1, 2, 3];
        let data = data.into_iter().chain([4, 5, 6]);

        assert_panics!(
            assert_matches!(data, [1, 4, 3]),
            includes("assertion failed"),
            includes("value from iterator does not match pattern"),
            includes("  value: 2"),
            includes("pattern: 4"),
            includes("  index: 1"),
        );
    }

    #[test]
    fn basic_leading_rest() {
        let data = get_data();

        assert_matches!(data, [.., Some(3), None]);
    }

    #[test]
    fn basic_middle_rest() {
        let data = vec![Some(0), Some(1), None];
        let data = data.into_iter().chain([None, Some(2), Some(3), None]);

        assert_matches!(data, [Some(0), .., None]);
    }

    #[test]
    fn rest_bound() {
        let data = vec![Some(0), Some(1), None];
        let data = data.into_iter().chain([None, Some(2), Some(3), None]);

        assert_matches!(data, [
            Some(0),
            mut inner @ .. => assert_eq!(inner.next().unwrap(), Some(1)),
            None,
        ]);
    }

    #[test]
    fn rest_guard() {
        let data = vec![Some(0), Some(1), None];
        let data = data.into_iter().chain([None, Some(2), Some(3), None]);

        let () = assert_matches!(data, [
            Some(0),
            inner @ .. if inner.count() == 5,
            None,
        ]);
    }

    #[test]
    fn basic_trailing_rest() {
        let data = get_data();
        assert_matches!(data, [Some(0), Some(_), None, ..]);
    }

    #[test]
    fn trailing_rest_doesnt_drain() {
        let mut count = 0;
        let data = get_data().into_iter().inspect(|_| count += 1);

        let (a, b) = assert_matches!(data, [Some(a) => a, Some(b) => b, None, ..]);

        assert_eq!(a, 0);
        assert_eq!(b, 1);
        assert_eq!(count, 3);
    }

    #[test]
    fn rest_other_bind() {
        let data = get_data();
        let (a, b) = assert_matches!(data, [Some(a) => a, .., b => b]);
        assert_eq!(a, 0);
        assert_eq!(b, None);
    }

    #[test]
    fn rest_tail_bound() {
        let data = get_data();
        assert_matches!(data, [Some(0), Some(_), None, data @ .. => assert_eq!(data.count(), 4)]);
    }

    #[test]
    fn rest_tail_leak() {
        let data = get_data();

        let (tail,) = assert_matches!(data, [
            Some(0),
            Some(_),
            None,
            data @ .. => data.enumerate()
        ]);

        let collected: Vec<_> = tail.collect();
        assert_eq!(
            collected,
            [(0, None), (1, Some(2)), (2, Some(3)), (3, None),]
        )
    }

    #[test]
    fn zero_size_rest() {
        let data = [1, 2, 3, 4].iter();
        assert_matches!(data, [1, 2, .., 3, 4]);
    }

    #[test]
    fn leading_rest_too_short() {
        let data = vec![1, 2, 3];
        assert_panics!(
            assert_matches!(data, [.., 1, 2, 3, 4, 5]),
            includes("iterable was too short"),
            includes("actual_length: 3"),
            includes("expected_length: 5.."),
        )
    }

    #[test]
    fn middle_rest_too_short() {
        let data = vec![1, 2, 3];
        assert_panics!(
            assert_matches!(data, [1, 2, .., 2, 1]),
            includes("iterable was too short"),
            includes("actual_length: 3"),
            includes("expected_length: 4.."),
        )
    }

    #[test]
    fn trailing_rest_too_short() {
        let data = vec![1, 2, 3];
        assert_panics!(
            assert_matches!(data, [1, 2, 3, 4, ..]),
            includes("iterable was too short"),
            includes("actual_length: 3"),
            includes("expected_length: 4.."),
        )
    }

    #[test]
    fn rest_early_pattern_fail() {
        let data = get_data();

        assert_panics!(
            assert_matches!(data, [Some(0), Some(1), Some(2), .., None]),
            includes("value from iterator does not match pattern"),
            includes("  value: None"),
            includes("pattern: Some(2)"),
            includes("  index: 2")
        )
    }

    #[test]
    fn rest_late_pattern_fail() {
        let data = get_data();

        assert_panics!(
            assert_matches!(data, [Some(0), Some(_), .., None, None]),
            includes("value from iterator does not match pattern"),
            includes("  value: Some("),
            includes("pattern: None"),
            includes("  index: 5"),
        )
    }

    #[test]
    fn rest_guard_fail() {
        let data = get_data();

        assert_panics!(
            assert_matches!(data, [Some(0), data @ .. if data.count() == 2, None]),
            includes(".. rest parameter failed guard"),
            includes("guard: data.count() == 2"),
        )
    }

    #[test]
    fn with_fmt_success() {
        let data = get_data();

        assert_matches!(data, [Some(0), Some(1), ..], "error: {}", 10)
    }

    #[test]
    fn with_fmt_fail() {
        let data = vec![None, Some(3), None];

        assert_panics!(
            assert_matches!(
                data,
                [None, Some(3), Some(3), ..],
                "formatted error: {}",
                10
            ),
            includes("assertion failed"),
            includes("value from iterator does not match pattern"),
            includes("  index: 2"),
            includes("  value: None"),
            includes("pattern: Some(3)"),
            includes("formatted error: 10")
        );
    }
}
