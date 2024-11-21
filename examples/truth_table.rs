//! Print truth tables for all the binary functions.
#![allow(clippy::wildcard_imports)]

use prop_logic::connective::*;

fn main() {
    println!("=== NULLARY ===");
    for f in NULLARY_FUNCTIONS.iter() {
        let table = f.get_truth_table();
        println!("{table}");
        println!();
    }

    println!("=== UNARY ===");
    for f in UNARY_FUNCTIONS.iter() {
        let table = f.get_truth_table();
        println!("{table}");
        println!();
    }

    println!("=== BINARY ===");
    for f in BINARY_FUNCTIONS.iter() {
        let table = f.get_truth_table();
        println!("{table}");
        println!();
    }
}
