//! # Propositional logic concepts
//!
//! - boolean algebra;
//! - boolean operators (logical connectives);
//! - truth tables;
//! - propositional expressions;
//! - normal forms;
//! - rules of inference.

// ==== There goes the list of lints to better control the quality of code.
// ==== It is also recommended to use the command
// ==== `clippy --workspace --all-targets --all-features -- -W clippy::nursery -W clippy::pedantic`
// ==== to handle a lot of corner cases.
//
//
// do not warn on older Rust versions
#![allow(unknown_lints)]
//
// The following list was generated with the command
//   $ rustc -W help | grep ' allow ' | awk '{print $1}' | tr - _ | sort | xargs -I{} echo '#![warn({})]'
//
#![warn(absolute_paths_not_starting_with_crate)]
#![warn(anonymous_parameters)]
// use `Box` without fear
#![allow(box_pointers)]
#![warn(deprecated_in_future)]
#![warn(elided_lifetimes_in_paths)]
#![warn(explicit_outlives_requirements)]
#![warn(ffi_unwind_calls)]
#![warn(fuzzy_provenance_casts)]
#![warn(indirect_structural_match)]
#![warn(invalid_reference_casting)]
#![warn(keyword_idents)]
#![warn(let_underscore_drop)]
#![warn(lossy_provenance_casts)]
#![warn(macro_use_extern_crate)]
#![warn(meta_variable_misuse)]
#![warn(missing_abi)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(multiple_supertrait_upcastable)]
#![warn(must_not_suspend)]
#![warn(non_ascii_idents)]
#![warn(non_exhaustive_omitted_patterns)]
#![warn(noop_method_call)]
#![warn(pointer_structural_match)]
#![warn(private_bounds)]
#![warn(private_interfaces)]
#![warn(rust_2021_incompatible_closure_captures)]
#![warn(rust_2021_incompatible_or_patterns)]
#![warn(rust_2021_prefixes_incompatible_syntax)]
#![warn(rust_2021_prelude_collisions)]
#![warn(single_use_lifetimes)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unnameable_types)]
// conflicts with the `clippy::redundant_pub_crate`
#![allow(unreachable_pub)]
// !!! NO UNSAFE
#![forbid(unsafe_code)]
#![warn(unsafe_op_in_unsafe_fn)]
#![warn(unstable_features)]
#![warn(unused_crate_dependencies)]
#![warn(unused_extern_crates)]
#![warn(unused_import_braces)]
#![warn(unused_labels)]
#![warn(unused_lifetimes)]
#![warn(unused_macro_rules)]
#![warn(unused_qualifications)]
#![warn(unused_results)]
#![warn(unused_tuple_struct_fields)]
#![warn(variant_size_differences)]
//
// additional recommendations
#![deny(clippy::mem_forget)]
// suppress some pedantic warnings
#![allow(clippy::must_use_candidate, clippy::uninlined_format_args)]
// `use super::*` in tests
#![cfg_attr(test, allow(clippy::wildcard_imports))]
// ====
// ==== The end of the lint lists.
