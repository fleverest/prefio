# Helper function to make second column a dense ranking. Not exported.
.densify_col2 <- function(x) {
  x[, 2L] <- dplyr::dense_rank(x[, 2L])
  x
}

#' Check if a preference is blank.
#' @param x A vector of [`preferences`][preferences].
#' @return A logical vector indicating which preferences are blank, i.e., `[]`.
#' @examples
#' pref_blank(preferences(c("a > b > c", "", "b > c")))
#' @export
pref_blank <- function(x) {
  vapply(
    x,
    \(pref) dim(pref)[1L] == 0L,
    logical(1L)
  )
}

#' Check the length (number of rankings) of a preference.
#' @param x A vector of [`preferences`][preferences].
#' @return The number of items listed on each of the preferences.
#' @examples
#' pref_length(preferences(c("a > b > c", "", "b > c")))
#' @export
pref_length <- function(x) {
  vapply(
    x,
    \(pref) dim(pref)[1L],
    integer(1L)
  )
}

#' Get the rank assigned to a specific item in a set of preferences.
#' @param x A vector of [`preferences`][preferences].
#' @param item_name The name of the item to extract the rank for.
#' @return The rank of `item_name` for each of the preferences in `x`.
#' @examples
#' pref_get_rank(preferences(c("a > b > c", "b > c = a", "")), "a")
#' @export
pref_get_rank <- function(x, item_name) {
  idx <- match(item_name, levels(x))[1L]
  if (length(idx) != 1L) {
    stop("`item_name` must name exactly one item in the preferences.")
  }
  vapply(
    x,
    \(pref) {
      res <- pref[pref[, 1L] == idx, 2L]
      if (length(res) == 0L) {
        NA
      } else {
        res
      }
    },
    integer(1L)
  )
}

#' Get the name of the item(s) assigned a specific rank, e.g., first.
#' @param x A vector of [`preferences`][preferences].
#' @param rank A single integer, the rank which you would like to inspect.
#' @param drop When `FALSE` (default), blank preferences will remain. When
#' `TRUE`, blank preferences will be omitted.
#' @return A list containing the name(s) of the item(s) ranked `rank` in each of
#' the preferences in `x`.
#' @examples
#' # Get items ranked first
#' pref_get_items(preferences(c("a > b > c", "b = c > a")), rank = 1)
#' # Get items ranked second
#' pref_get_items(preferences(c("a > b > c", "b = c > a")), rank = 2)
#' # Get items ranked first, dropping blank preferences
#' pref_get_items(preferences(c("a > b > c", "", "b = c > a")), rank = 1, drop = TRUE)
#' @export
pref_get_items <- function(x, rank, drop = FALSE) {
  result <- vapply(
    x,
    \(pref) list(levels(x)[pref[which(pref[, 2L] == rank), 1L]]),
    list(1L)
  )

  if (drop) {
    # Identify preferences with zero items at the specified rank
    empty_prefs <- vapply(result, \(items) length(items) == 0L, logical(1L))
    # Return only non-empty preferences
    result[!empty_prefs]
  } else {
    result
  }
}

#' Complete preferences by adding unselected items as last place occurrances.
#' @param x A vector of [`preferences`][preferences].
#' @return A new vector of preferences, with each selection starting with the
#' corresponding selections made in `x`, but with all unranked items placed
#' last.
#' @examples
#' # Complete partial rankings by adding unranked items last
#' pref_add_unranked(preferences(c("a > b", "c > a", "b")))
#' @export
pref_add_unranked <- function(x) {
  if (pref_type(x) %in% c("soi", "toi")) {
    n_items <- nlevels(x)
    x |>
      vapply(
        \(pref) {
          if (nrow(pref) < n_items) {
            missing_items <- setdiff(seq(n_items), pref[, 1L])
            max_rank <- max(pref[, 2L])
            list(rbind(pref, cbind(missing_items, max_rank + 1L)))
          } else {
            list(pref)
          }
        },
        list(integer())
      ) |>
      .vctr_preferences(item_names = levels(x))
  } else {
    x
  }
}

#' Truncate preferences to a maximum number of ranks.
#' @param x A vector of [`preferences`][preferences].
#' @param n The maximum number of ranks to include (positive) or number of ranks to drop (negative). Must be an integer.
#' @param bottom If `FALSE` (default), operates on top ranks. If `TRUE`, operates on bottom ranks.
#' @return A vector of preferences with each selection truncated according to the parameters.
#' @examples
#' # Keep only the top 2 ranks
#' pref_trunc(preferences(c("a > b > c > d", "b > c > a")), n = 2)
#' # Keep only the bottom 2 ranks
#' pref_trunc(preferences(c("a > b > c > d", "b > c > a")), n = 2, bottom = TRUE)
#' # Drop the bottom 2 ranks (keep top ranks)
#' pref_trunc(preferences(c("a > b > c > d", "b > c > a")), n = -2)
#' # Drop the top 2 ranks (keep bottom ranks)
#' pref_trunc(preferences(c("a > b > c > d", "b > c > a")), n = -2, bottom = TRUE)
#' @export
pref_trunc <- function(x, n = 1L, bottom = FALSE) {
  n <- as.integer(n)
  if (length(n) != 1L) {
    stop("`n` must be a single integer.")
  }
  drop <- n < 0L
  n <- abs(n)
  x |>
    vapply(
      \(pref) {
        if (dim(pref)[1L] == 0L) {
          # Return empty preference unchanged
          list(pref)
        } else {
          max_rank <- max(pref[, 2L])

          if (drop && !bottom) {
            # Drop n ranks from bottom (keep top ranks)
            pref[pref[, 2L] <= max_rank - n, , drop = FALSE] |>
              list()
          } else if (drop && bottom) {
            # Drop n ranks from top (keep bottom ranks)
            pref[pref[, 2L] > n, , drop = FALSE] |>
              .densify_col2() |>
              list()
          } else if (!drop && bottom) {
            # Keep bottom n ranks
            pref[pref[, 2L] >= max_rank - n + 1L, , drop = FALSE] |>
              .densify_col2() |>
              list()
          } else {
            # Keep top n ranks (default)
            pref[pref[, 2L] <= n, , drop = FALSE] |>
              list()
          }
        }
      },
      list(integer())
    ) |>
    .vctr_preferences(item_names = levels(x))
}

#' Keep only specified items from preferences.
#' @param x A vector of [`preferences`][preferences].
#' @param items The names of the items which should be kept for preferences in `x`.
#' @return A new vector of preferences, but only containing `items` from each selection.
#' @examples
#' # Keep only 'a' and 'c'
#' pref_keep(preferences(c("a > b > c", "b > c > a")), c("a", "c"))
#' @export
pref_keep <- function(x, items) {
  idxs <- match(items, levels(x))
  x |>
    vapply(
      \(pref) {
        rows <- stats::na.omit(match(idxs, pref[, 1L]))
        if (length(rows) == 0L) {
          pref[integer(0L), , drop = FALSE] |>
            list()
        } else if (length(rows) > 0L) {
          pref[rows, , drop = FALSE] |>
            .densify_col2() |>
            list()
        } else {
          list(pref)
        }
      },
      list(integer())
    ) |>
    .vctr_preferences(item_names = levels(x))
}

#' Remove specified items from preferences.
#' @param x A vector of [`preferences`][preferences].
#' @param items The names of the items which should be removed from the preferences in `x`.
#' @return A new vector of preferences, but with `items` removed from each selection.
#' @examples
#' # Remove 'b'
#' pref_omit(preferences(c("a > b > c", "b > c > a")), "b")
#' # Remove 'b' and 'd'
#' pref_omit(preferences(c("a > b > c > d", "b > c > a > d")), c("b", "d"))
#' @export
pref_omit <- function(x, items) {
  idxs <- match(items, levels(x))
  x |>
    vapply(
      \(pref) {
        rows <- stats::na.omit(match(idxs, pref[, 1L]))
        if (length(rows) >= nrow(pref)) {
          pref[integer(0L), , drop = FALSE] |>
            list()
        } else if (length(rows) > 0L) {
          pref[-rows, , drop = FALSE] |>
            .densify_col2() |>
            list()
        } else {
          list(pref)
        }
      },
      list(integer())
    ) |>
    .vctr_preferences(item_names = levels(x))
}

#' Eliminate lowest (or highest) ranked items from preferences.
#' @param x A vector of [`preferences`][preferences].
#' @param n The number of times to remove the bottom rank.
#' @param lowest If `TRUE`, eliminates the lowest ranked item(s) for each
#' selection.
#' @param drop If `TRUE`, drops blank preferences from the output.
#' @return A new vector of preferences which is equal to `x` but with the least
#' preferred selection dropped for each selection.
#' @examples
#' # Remove the lowest ranked item from each preference
#' pref_pop(preferences(c("a > b > c", "b > c > a")))
#'
#' # Remove the 2 lowest ranked items
#' pref_pop(preferences(c("a > b > c > d", "b > c > a > d")), n = 2)
#'
#' # Remove the highest ranked item instead
#' pref_pop(preferences(c("a > b > c", "b > c > a")), lowest = FALSE)
#'
#' # Remove blank preferences that result from popping
#' pref_pop(preferences(c("a > b", "c", "")), drop = TRUE)
#' @export
pref_pop <- function(x, n = 1L, lowest = TRUE, drop = FALSE) {
  n <- as.integer(n)
  if (length(n) != 1L && is.integer(n) && n > 0L) {
    stop("`n` must be a single, positive integer.")
  }
  result <- x |>
    vapply(
      \(pref) {
        if (dim(pref)[1L] == 0L) {
          pref |>
            list()
        } else if (lowest) {
          pref[pref[, 2L] < max(pref[, 2L]) - n + 1L, , drop = FALSE] |>
            list()
        } else {
          pref[pref[, 2L] > n, , drop = FALSE] |>
            .densify_col2() |>
            list()
        }
      },
      list(integer())
    ) |>
    .vctr_preferences(item_names = levels(x))
  if (drop) {
    result[!pref_blank(result)]
  } else {
    result
  }
}

#' Reverse preference rankings
#' @param x A vector of [`preferences`][preferences].
#' @param ... Not used.
#' @return A vector of preferences with rankings reversed (first becomes last, etc.)
#' @examples
#' pref_rev(preferences(c("a > b > c", "b > c > a")))
#' @export
pref_rev <- function(x, ...) {
  x |>
    vapply(
      \(pref) {
        if (dim(pref)[1L] == 0L) {
          # Return empty preference
          list(pref)
        } else {
          # Reverse rankings by subtracting from max rank + 1
          max_rank <- max(pref[, 2L])
          new_pref <- pref
          new_pref[, 2L] <- max_rank + 1L - pref[, 2L]
          list(new_pref)
        }
      },
      list(integer())
    ) |>
    .vctr_preferences(item_names = levels(x))
}
