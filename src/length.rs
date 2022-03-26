pub trait Length {}

impl Length for usize {}

#[derive(Debug, Clone, Copy)]
pub struct LowerBound(pub usize);

impl Length for LowerBound {}
