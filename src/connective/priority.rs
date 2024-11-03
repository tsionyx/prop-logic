#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
/// Define the priority of operations.
pub struct Priority(pub(crate) u8);

impl Priority {
    /// Highest priority.
    pub const fn highest() -> Self {
        Self(u8::MAX)
    }

    /// Lowest priority.
    pub const fn lowest() -> Self {
        Self(u8::MIN)
    }

    /// Get higher priority off the given one.
    pub const fn higher(self) -> Self {
        Self(self.0.saturating_add(1))
    }

    /// Get lower priority off the given one.
    pub const fn lower(self) -> Self {
        Self(self.0.saturating_sub(1))
    }
}

/// Defines the priority for a [`Connective`][super::Connective].
pub trait Prioritized {
    /// Get the priority.
    fn priority(&self) -> Priority;
}
