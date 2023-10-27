// #![feature(trace_macros)]
// #![feature(log_syntax)]
// trace_macros!(true);

#[macro_export]
/// Converts two lists of items to their product
/// to later feed them to another (sub)macro.
///
/// <https://users.rust-lang.org/t/cartesian-product-using-macros/10763/24>
macro_rules! cartesian {
    // base case
    (@$out:tt [] $b:tt $init_b:tt $submacro:tt) => {
        // log_syntax!{1}
        // log_syntax!{$submacro $out}
         $submacro!{$out}
    };
    // when b empty, strip off an "a" and refill b from init_b
    (@$out:tt [$a:tt, $($at:tt)*] [] $init_b:tt $submacro:tt) => {
        // log_syntax!{2}
        cartesian!{@$out [$($at)*] $init_b $init_b $submacro}
    };
    // strip off a "b" and add a pair to $out that consists of the first "a" and first "b"
    (@[$($out:tt)*] [$a:tt, $($at:tt)*] [$b:tt, $($bt:tt)*] $init_b:tt $submacro:tt) => {
        // log_syntax!{$submacro}
        // log_syntax!{3}
        cartesian!{@[$($out)* ($a, $b),] [$a, $($at)*] [$($bt)*] $init_b $submacro}
    };
    // friendly public interface
    ([$($a:tt)*], [$($b:tt)*], $submacro:tt) => {
        cartesian!{@[] [$($a)*,] [$($b)*,] [$($b)*,] $submacro}
    };
}

#[macro_export]
/// Converts two lists of items to their product
/// to later feed them to another (sub)macro.
///
/// The upper, lower and diagonal elements could be handled independently.
///
/// <https://users.rust-lang.org/t/cartesian-product-using-macros/10763/26>
macro_rules! cartesian_diag {
    // recurse down diagonal
    (@[$($start_a:tt),*], [$mid_a:tt, $($end_a:tt),*], [$($start_b:tt),*], [$mid_b:tt, $($end_b:tt),*], $lower:tt, $diag:tt, $upper:tt) => {
        $($lower!($mid_a, $start_b);)*
        $diag!($mid_a, $mid_b);
        $($upper!($mid_a, $end_b);)*
        cartesian_diag!{@[$($start_a,)* $mid_a], [$($end_a),*], [$($start_b,)* $mid_b], [$($end_b),*], $lower, $diag, $upper}
    };
    // base case, last element on the diagonal
    (@[$($start_a:tt),*], [$last_a:tt], [$($start_b:tt),*], [$last_b:tt], $lower:tt, $diag:tt, $upper:tt) => {
        $($lower!($last_a, $start_b);)*
        $diag!($last_a, $last_b);
    };
    // friendly public interface
    ([$($a:tt)*], [$($b:tt)*], $lower:tt, $diag:tt, $upper:tt) => {
        cartesian_diag!{@[], [$($a)*], [], [$($b)*], $lower, $diag, $upper}
    };
    ([$($a:tt)*], [$($b:tt)*], $non_diag:tt, $diag:tt) => {
        cartesian_diag!{@[], [$($a)*], [], [$($b)*], $non_diag, $diag, $non_diag}
    };
    ([$($a:tt)*], [$($b:tt)*], $submacro:tt) => {
        cartesian_diag!{@[], [$($a)*], [], [$($b)*], $submacro, $submacro, $submacro}
    };
    (square [$($a:tt)*], $lower:tt, $diag:tt, $upper:tt) => {
        cartesian_diag!{[$($a)*], [$($a)*], $lower, $diag, $upper}
    };
    (square [$($a:tt)*], $non_diag:tt, $diag:tt) => {
        cartesian_diag!{[$($a)*], [$($a)*], $non_diag, $diag, $non_diag}
    };
    (square [$($a:tt)*], $submacro:tt) => {
        cartesian_diag!{[$($a)*], [$($a)*], $submacro, $submacro, $submacro}
    };
}

pub(crate) use cartesian_diag;
