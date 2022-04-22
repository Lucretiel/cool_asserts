//! A collection of useful assertions and other utilities for writing
//! tests in Rust.

mod assert_matches;
mod assert_panics;
mod assertion_failure;

#[doc(hidden)]
pub mod iterators;

#[doc(hidden)]
pub mod length;

#[doc(hidden)]
pub use indent_write;

pub use assert_panics::get_panic_message;

#[doc(hidden)]
#[macro_export]
macro_rules! force_type {
    (const $type:ty: $value:expr) => {{
        const VALUE: $type = $value;
        VALUE
    }};

    ($type:ty: $value:expr) => {
        ::core::convert::identity::<$type>($value)
    };
}

#[doc(hidden)]
pub use core as secret_core;
