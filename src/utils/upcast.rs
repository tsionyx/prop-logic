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
//!     fn up_from(value: &T) -> &(dyn Super + 'a) { value }
//!     fn up_from_mut(value: &mut T) -> &mut (dyn Super + 'a) { value }
//! }
//! ```
//!
//! Now you can use the `Upcast::up(_mut)?: & (mut)? dyn Sub -> & (mut)? dyn Super`.
//!
//! Based on `https://crates.io/crates/upcast`.

/// Implement this trait explicitly for `dyn Super`.
pub trait UpcastFrom<T: ?Sized> {
    fn up_from(value: &T) -> &Self;

    fn up_from_mut(value: &mut T) -> &mut Self;
}

/// Require this in your subtrait: `trait Sub: Upcast<dyn Super>`.
pub trait Upcast<U: ?Sized> {
    fn up(&self) -> &U;

    #[allow(dead_code)]
    fn up_mut(&mut self) -> &mut U;
}

impl<T: ?Sized, U: ?Sized> Upcast<U> for T
where
    U: UpcastFrom<T>,
{
    fn up(&self) -> &U {
        U::up_from(self)
    }

    fn up_mut(&mut self) -> &mut U {
        U::up_from_mut(self)
    }
}
