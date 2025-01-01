//! Print truth tables for all the binary functions.
#![allow(clippy::wildcard_imports)]

use prop_logic::{connective::*, two_powers, CheckedArray};

fn print_stat<const ARITY: usize>(f: &dyn StoredBoolFn<ARITY>)
where
    TruthTable<ARITY>: std::fmt::Display,
    two_powers::D: CheckedArray<ARITY>,
{
    print!("{}", f.notation());
    let alternate: Vec<_> = f
        .alternate_notations()
        .into_iter()
        .flatten()
        .map(|n| n.to_string())
        .collect();
    if alternate.is_empty() {
        println!();
    } else {
        println!(" (aka {alternate:?})");
    }

    let table = f.get_truth_table();
    println!("{table}");
}

fn main() {
    println!("=== NULLARY ===");
    for f in NULLARY_FUNCTIONS.as_ref() {
        print_stat(f);
        println!();
    }

    println!("=== UNARY ===");
    for f in UNARY_FUNCTIONS.as_ref() {
        print_stat(f);
        println!();
    }

    println!("=== BINARY ===");
    for f in BINARY_FUNCTIONS.as_ref() {
        print_stat(f);
        println!();
    }
}
