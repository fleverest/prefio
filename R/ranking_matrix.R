#' Compute the Rankings Matrix for a vector of preferences
#'
#' Convert a set of preferences to a rankings matrix, where each preference
#' defines a single row in the output. The columns in the rankings matrix
#' give the vector or ranks assigned to the corresponding candidate.
#'
#' For a `preferences` vector of length \eqn{N} with \eqn{M} items, the rankings
#' matrix is an \eqn{N} by \eqn{M} matrix, with element \eqn{(i, j)} being the
#' rank assigned to candidate \eqn{j} in the \eqn{i}th selection.
#'
#' @param x A [`preferences`][preferences] object or a `tibble` with a
#' `preferences`-typed column.
#' @param preferences_col <[`tidy-select`][dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the preferences.
#' @param ... Currently unused.
#'
#' @return An \eqn{N} by \eqn{M} matrix, where \eqn{N} is the number of
#' preferences, and \eqn{M} is the number of items.
#'
#' @examples
#' x <- tibble::tribble(
#'   ~voter_id, ~species, ~food, ~ranking,
#'   1, "Rabbit", "Apple", 1,
#'   1, "Rabbit", "Banana", 2,
#'   1, "Rabbit", "Carrot", 3,
#'   2, "Monkey", "Banana", 1,
#'   2, "Monkey", "Apple", 2,
#'   2, "Monkey", "Carrot", 3
#' ) |>
#'   long_preferences(
#'     food_preference,
#'     id_cols = voter_id,
#'     item_col = food,
#'     rank_col = ranking
#'   ) |>
#'   dplyr::pull(food_preference) |>
#'   rankings_matrix()
#'
#' @export
ranking_matrix <- function(x,
                           preferences_col = NULL,
                           ...) {
  x <- .validate_preferences_frequencies(
    x,
    {{ preferences_col }}
  )

  n_items <- nlevels(x$preferences)

  out <- x$preferences |>
    vapply(
      \(pref) {
        rankings <- rep(NA, n_items)
        rankings[pref[, 1L]] <- pref[, 2L]
        rankings |>
          list()
      },
      list(integer())
    ) |>
    rep(x$frequency) |>
    do.call(what = rbind)

  colnames(out) <- levels(x$preferences)
  out
}
