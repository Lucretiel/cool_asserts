use std::fmt::{self, Display, Formatter};

/**
Count the number of patterns in the input. Compile error if there are any ..
patterns.
*/
#[doc(hidden)]
#[macro_export]
macro_rules! compute_target_length_no_slice {
    ($length:expr; ) => {
        $crate::force_type!(const usize: $length)
    };

    ($length:expr; $($name:ident@ )* .. $(if $guard:expr)? $(=> $block:expr)? $(, $($tail:tt)*)?) => {
        compile_error!("can't have more than one .. rest pattern in a [] slice pattern")
    };

    ($length:expr; $pattern:pat $(if $guard:expr)? $(=> $block:expr)? $(, $($tail:tt)*)?) => {
        $crate::compute_target_length_no_slice!($length + 1; $($($tail)*)?)
    };
}

/**
Count the number of patterns in the input. Returns a usize, or a LowerBound if
there was a .. pattern in the input (indicating that the length is at *least*
this value)
*/
#[doc(hidden)]
#[macro_export]
macro_rules! compute_target_length {
    ($length:expr; ) => {
        $crate::force_type!(const usize: $length)
    };

    ($length:expr; $($($bind:ident)+ @ )* .. $(if $guard:expr)? $(=> $block:expr)? $(, $($tail:tt)*)?) => {
        $crate::length::LowerBound(
            $crate::compute_target_length_no_slice!($length; $($($tail)*)?)
        )
    };

    ($length:expr; $pattern:pat $(if $guard:expr)? $(=> $block:expr)? $(, $($tail:tt)*)?) => {
        $crate::compute_target_length!($length + 1; $($($tail)*)?)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LowerBound(pub usize);

impl Display for LowerBound {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}..", self.0)
    }
}
