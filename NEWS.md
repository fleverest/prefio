# **prefio** 0.2.0

* 🎉🥳 Made **prefio** tidy! 🎊🎆
* Reimplemented `preferences` using `vctrs`.
* Implemented methods required for dplyr operations.
* Added a suite of tools for operating on `preferences`.
* Added two summary functions for `preferences`:
    1. instant-runoff voting algorithm
    2. covariance matrix based on rankings matrix representation
* Improved preflib interface to fail gracefully, which is why prefio 0.1.1 was taken down from CRAN.

# **prefio** 0.1.1

* Changed string formatting for preferences (now displays as `[A > B > C]`
rather than `"A > B > C"`).
* Fixed a bug when loading preferences from orderings format.
* Allows an empty preferences object (displays `preferences(0)`).
* Allow blank preferences (displays `[blank]`).

# **prefio** 0.1.0

* This is the initial release of **prefio**.
