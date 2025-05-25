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
#' pref_rank_of_item(preferences(c("a > b > c", "b > c = a", "")), "a")
#' @export
pref_rank_of_item <- function(x, item_name) {
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
#' @return A list containing the name(s) of the item(s) ranked `rank` in each of
#' the preferences in `x`.
#' @examples
#' # Get items ranked first
#' pref_items_at_rank(preferences(c("a > b > c", "b = c > a")), rank = 1)
#' # Get items ranked second
#' pref_items_at_rank(preferences(c("a > b > c", "b = c > a")), rank = 2)
#' @export
pref_items_at_rank <- function(x, rank) {
  vapply(
    x,
    \(pref) list(levels(x)[pref[which(pref[, 2L] == rank), 1L]]),
    list(1L)
  )
}

#' Complete preferences by adding unselected items as last place occurrances.
#' @param x A vector of [`preferences`][preferences].
#' @return A new vector of preferences, with each selection starting with the
#' corresponding selections made in `x`, but with all unranked items placed
#' last.
#' @examples
#' # Complete partial rankings by adding unranked items last
#' pref_complete(preferences(c("a > b", "c > a", "b")))
#' @export
pref_complete <- function(x) {
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
#' @param n_ranks The number of rankings to include in the output.
#' @param top_ranks If `TRUE`, output the top `n_ranks`, otherwise the bottom
#' `n_ranks`.
#' @return A vector of preferences with each selection equal to `x` but omitting
#' the bottom (if `top_ranks` is `TRUE`, otherwise top) selections so that the
#' new vector has at most `n_ranks` selections.
#' @examples
#' # Keep only the top 2 ranked items
#' pref_trunc(preferences(c("a > b > c > d", "b > c > a")), n_ranks = 2)
#' # Keep only the bottom 2 ranked items
#' pref_trunc(preferences(c("a > b > c > d", "b > c > a")), n_ranks = 2, top_ranks = FALSE)
#' @export
pref_trunc <- function(x, n_ranks = 1L, top_ranks = TRUE) {
  x |>
    vapply(
      \(pref) {
        if (top_ranks) {
          pref[pref[, 2L] <= n_ranks, , drop = FALSE] |>
            list()
        } else {
          pref[pref[, 2L] >= max(pref[, 2L]) - n_ranks + 1L, , drop = FALSE] |>
            .densify_col2() |>
            list()
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
#' # Remove 'b' from preference rankings
#' pref_rm_items(preferences(c("a > b > c", "b > c > a")), "b")
#' # Remove multiple items
#' pref_rm_items(preferences(c("a > b > c > d", "b > c > a > d")), c("b", "d"))
#' @export
pref_rm_items <- function(x, items) {
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

#' Remove all but specified items from preferences.
#' @param x A vector of [`preferences`][preferences].
#' @param items The names of the items which should be kept from the preferences in `x`.
#' @return A new vector of preferences, but with any item not in `items`
#' removed from each selection.
#' @examples
#' # Keep only items 'a' and 'c' from the preferences
#' pref_project(preferences(c("a > b > c", "b > c > a")), c("a", "c"))
#' @export
pref_project <- function(x, items) {
  idxs <- match(items, levels(x))
  x |>
    vapply(
      \(pref) {
        pref[stats::na.omit(match(idxs, pref[, 1L])), , drop = FALSE] |>
          .densify_col2() |>
          list()
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

# Helper function to make second column a dense ranking. Not exported.
.densify_col2 <- function(x) {
  x[, 2L] <- dplyr::dense_rank(x[, 2L])
  x
}

#' Covariance matrix for preferences, calculated using the rankings matrix.
#'
#' @param x A vector of preferences.
#' @param preferences_col <[`tidy-select`][dplyr::dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the preferences.
#' @param frequency_col <[`tidy-select`][dplyr::dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the frequency of the preferences. If not
#' provided, each row is considered to be observed a single time.
#' @param ... Extra arguments to be passed to `stats::cov.wt`.
#' @return A covariance matrix containing covariances for the ranks assigned to
#' item pairs.
#' @examples
#' # Simple covariance on a vector of preferences
#' prefs <- preferences(c("a > b > c", "b > c > a", "c > a > b"))
#' pref_cov(prefs)
#'
#' # Weighted covariance by frequency
#' df <- tibble::tibble(
#'   prefs = preferences(c("a > b > c", "b > c > a")),
#'   freq = c(3, 2)
#' )
#' pref_cov(df, preferences_col = prefs, frequency_col = freq)
#' @export
pref_cov <- function(x,
                     preferences_col = NULL,
                     frequency_col = NULL,
                     ...) {
  x |>
    ranking_matrix({{ preferences_col }}, {{ frequency_col }}) |>
    apply(
      2L,
      \(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)
    ) |>
    stats::cov.wt(...)
}


#' Reverse preference rankings
#' @param x A vector of [`preferences`][preferences].
#' @param ... Not used.
#' @return A vector of preferences with rankings reversed (first becomes last, etc.)
#' @examples
#' pref_reverse(preferences(c("a > b > c", "b > c > a")))
#' @export
pref_reverse <- function(x, ...) {
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
