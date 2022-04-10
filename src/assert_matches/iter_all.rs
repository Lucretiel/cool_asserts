#[doc(hidden)]
#[macro_export]
macro_rules! collect_or_drain {
    ($($bind:ident)+ = $iter:ident) => {
        let $($bind)+ = $crate::force_type!(Vec<_>: ::core::iter::Iterator::collect($iter));
    };

    ($iter:ident) => {
        ::core::iter::Iterator::for_each($iter, |_| ())
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! assert_matches_iter_all {
    // Base case without guard
    (
        block: $block:expr,
    ) => {
        $block
    };

    // Base case with guard
    //
    (
        if: $guard:expr,
        block: $block:expr,
    ) => {
        if $guard {
            $block
        } else {
            $crate::assertion_failure!(
                "iterator pattern failed guard",
                guard: stringify!($guard),
            )
        }
    };

    // Base case: no more patterns. Perform a final length check, then return
    // the collected values.
    (
        $(if: $guard:expr,)?
        block: $block:expr,
        iter: $iter:ident,
        index: $index:expr,
        expected_length: $target:expr,
        patterns: [],
    ) => {
        match ::core::iter::Iterator::next(&mut $iter) {
            ::core::option::Option::Some(overflow) => $crate::assertion_failure!(
                "iterable was too long",
                actual_length: $index + 1 + ::core::iter::Iterator::count($iter),
                expected_length: $index,
                first_overflow debug: overflow,
            ),
            ::core::option::Option::None => $crate::assert_matches_iter_all!(
                $(if: $guard,)?
                block: $block,
            ),
        }
    };

    // Rest pattern as the last pattern. We can just forward the iterator
    // directly in this case, potentially even returning it from assert matches.
    (
        $(if: $guard:expr,)?
        block: $block:expr,
        iter: $iter:ident,
        index: $index:expr,
        expected_length: $target:expr,
        patterns: [
            $($($bind:ident)+ @)? .. $(,)?
        ],
    ) => {{
        let _discard = $target;

        $(let $($bind)+ = $iter;)?

        $crate::assert_matches_iter_all!(
            $(if: $guard,)?
            block: $block,
        )
    }};

    // Rest pattern in a non-final position. We have to iterate all but the
    // last N items, and if there's a name binding, we have to collect them
    // into the vec
    (
        $(if: $guard:expr,)?
        block: $block:expr,
        iter: $iter:ident,
        index: $index:expr,
        expected_length: $target:expr,
        patterns: [
            $($($bind:ident)+ @)? .. ,
            $($tail:tt)+
        ],
    ) => {{
        // Create a buffer length N = the number of remaining patterns, and
        // fill it with the next N items from the iterator
        let buffer: [_; $crate::compute_target_length_no_slice!(0; $($tail)+)] = $crate::build_tail_array!(
            iter: $iter,
            index: $index,
            expected_length: $target,
            patterns: [$($tail)+]
        );

        // Use LoopBuffer to create an iterator that will yield all but the
        // last N items of the iterator. When this is done, the LoopBuffer will
        // contain those last N items of the iterator, which we will forward to
        // the remaining patterns.
        let mut buffer = $crate::iterators::LoopBuffer::new(buffer);
        let mut index = $index;

        let iter = buffer.bind_iterator($iter);
        let iter = ::core::iter::Iterator::inspect(iter, |_| {index += 1});
        let iter = ::core::iter::Iterator::fuse(iter);

        // Collect the contents of the iterator into a vector, if it exists;
        // otherwise simply drain it.
        $crate::collect_or_drain!($($($bind)+ = )? iter);

        let tail = buffer.into_array();
        let mut tail = ::core::iter::IntoIterator::into_iter(tail);

        $crate::assert_matches_iter_all!(
            $(if: $guard,)?
            block: $block,
            iter: tail,
            index: index,
            expected_length: $target,
            patterns: [$($tail)+],
        )
    }};

    // Ordinary case: a non-.. pattern. Attempt to match it, then recurse.
    // Recursion happens inline, because we need all the patterns to be scoped
    // together so that *all* bindings can be used in the final guard and block
    (
        $(if: $guard:expr,)?
        block: $block:expr,
        iter: $iter:ident,
        index: $index:expr,
        expected_length: $target:expr,
        patterns: [
            $pattern:pat $(, $($tail:tt)*)?
        ],
    ) => {
        match ::core::iter::Iterator::next(&mut $iter) {
            ::core::option::Option::Some(item) => $crate::assert_matches!(
                item,
                $pattern => $crate::assert_matches_iter_all!(
                    $(if $guard,)?
                    block: $block,
                    iter: $iter,
                    index: $index + 1,
                    expected_length: $target,
                    patterns: [
                        $($($tail)*)?
                    ],
                ),
            ),
            ::core::option::Option::None => $crate::assertion_failure!(
                "iterable was too short",
                actual_length: $index,
                expected_length: $target,
            ),
        }
    };
}

#[cfg(test)]
mod tests {
    use crate::assert_matches;

    fn get_data() -> impl IntoIterator<Item = Option<i32>> {
        [Some(0), Some(1), None, None, Some(2), Some(3), None]
    }

    #[test]
    fn basic_group_match() {
        let data = get_data();

        let value = assert_matches!(data, [
            Some(a),
            Some(b),
            None,
            _,
            _,
            Some(c),
            None
        ] => a + b + c);

        assert_eq!(value, 4);
    }
}
