#[macro_export]
macro_rules! assert_matches {
    ($value:expr, $pattern:pat $(if $guard:expr)? $(=> $block:expr)?) => ({
        let value = &$value;
        match value {
            $pattern $(if $guard)? => { $($block)? },
            _ => panic!("assertion failed: `(value does not match pattern)`
 expression: {},
      value: {:?},
    pattern: {}", stringify!($value), &*value, stringify!($pattern)),
        }
    });

    ($value:expr, $pattern:pat $(if $guard:expr)? $(=> $block:expr)?, ) => {
        assert_matches!($value, $pattern $(if $guard)? $(=> $block)?)
    };

    ($value:expr, $pattern:pat $(if $guard:expr)? $(=> $block:expr)? , $($fmt_arg:tt)+) => ({
        let value = &$value;
        match value {
            $pattern $(if $guard)? => { $($block)? },
            _ => panic!("assertion failed: `(value does not match pattern)`
 expression: `{}`,
      value: `{:?}`,
    pattern: `{}`: {}", stringify!($value), &*value, stringify!($pattern), format_args!($($fmt_arg)+);),
        }
    });
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
