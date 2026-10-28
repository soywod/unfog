//! # GTD
//!
//! The method over plain VTODOs, as pimdir's guide states it (GUIDE.md
//! §17) and the Pimalaya app applies it: `capture` writes to the Inbox,
//! `clarify` decides what each capture is, the views list the open tasks
//! of every account by their reserved categories, and `review` walks
//! them all. The convention itself is [`crate::shared::task::gtd`].

pub mod capture;
pub mod clarify;
pub mod cli;
pub mod review;
pub mod view;

#[cfg(test)]
mod tests;
