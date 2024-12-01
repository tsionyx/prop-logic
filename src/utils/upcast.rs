//! Helper to implement upcasting dyn object of some trait to its supertrait.
//!
//! Usage:
//!
//! ```no-compile
//!
//! pub trait Super {}
//! pub trait Sub: Super + Upcast<dyn Super> {}
//!
//! // a necessary boilerplate
//! impl<'a, T: Super + 'a> UpcastFrom<T> for dyn Super + 'a {
//!     fn up_from(value: &T) -> &Self { value }
//!     fn up_from_mut(value: &mut T) -> &mut Self { value }
//! }
//! ```
//!
//! Now you can use the `Upcast::up(_mut)?: & (mut)? dyn Sub -> & (mut)? dyn Super`.
//!
//! Based on `https://crates.io/crates/upcast`.

/// Implement this trait explicitly for `dyn Super`.
pub trait UpcastFrom<T: ?Sized> {
    /// Convert the value of T: &dyn Subtrait into &dyn Supertrait.
    fn up_from(value: &T) -> &Self;

    /// Convert the value of T: &mut dyn Subtrait into &mut dyn Supertrait.
    fn up_from_mut(value: &mut T) -> &mut Self;
}

/// Require this in your subtrait: `trait Sub: Upcast<dyn Super>`.
pub trait Upcast<U: ?Sized> {
    /// Convert the value of T: &dyn Subtrait into &dyn Supertrait.
    ///
    /// Consider relying on the blanket implementation by implementing the [`UpcastFrom`] for `dyn Supertrait`.
    fn up(&self) -> &U;

    /// Convert the value of T: &mut dyn Subtrait into &mut dyn Supertrait.
    ///
    /// Consider relying on the blanket implementation by implementing the [`UpcastFrom`] for `dyn Supertrait`.
    fn up_mut(&mut self) -> &mut U;
}

impl<T, U> Upcast<U> for T
where
    T: ?Sized,
    U: UpcastFrom<T> + ?Sized,
{
    fn up(&self) -> &U {
        U::up_from(self)
    }

    fn up_mut(&mut self) -> &mut U {
        U::up_from_mut(self)
    }
}
