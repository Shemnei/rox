#![allow(
	// To make moving things around easier/cheaper, everthing is boxed.
	clippy::borrowed_box,
	clippy::vec_box,

	dead_code,
	rustdoc::private_intra_doc_links
)]
#![deny(
    // Documentation
	// TODO: rustdoc::broken_intra_doc_links,
	// TODO: rustdoc::missing_crate_level_docs,
	// TODO: missing_docs,
	// TODO: clippy::missing_docs_in_private_items,

    // Other
	deprecated_in_future,
	exported_private_dependencies,
	future_incompatible,
	missing_copy_implementations,
	missing_debug_implementations,
	private_in_public,
	rust_2018_compatibility,
	rust_2018_idioms,
	trivial_casts,
	trivial_numeric_casts,
	unstable_features,
	unused_import_braces,
	unused_qualifications,

	// clippy attributes
	clippy::missing_const_for_fn,
	clippy::redundant_pub_crate,
	clippy::use_self
)]
#![cfg_attr(docsrs, feature(doc_cfg), feature(doc_alias))]

// TODO: all extra challenges.

pub mod env;
pub mod expr;
pub mod interpret;
pub mod lex;
pub mod parse;
pub mod rox;
pub mod source;
pub mod span;
pub mod stmt;
pub mod symbol;
pub mod token;

pub use crate::rox::*;
