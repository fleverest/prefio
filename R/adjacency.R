#' Compute the Adjacency Matrix for a vector of preferences
#'
#' Convert a set of preferences to an adjacency matrix summarising wins
#' and losses between pairs of items.
#'
#' For a `preferences` object with \eqn{N} items, the adjacency
#' matrix is an \eqn{N} by \eqn{N} matrix, with element \eqn{(i, j)} being the
#' number of times item \eqn{i} wins over item \eqn{j}. For example, in the
#' preferences \{1\} > \{3, 4\} > \{2\}, item 1 wins over items 2, 3, and 4,
#' while items 3 and 4 win over item 2.
#'
#' If `weights` is specified, the values in the adjacency matrix are the
#' weighted counts.
#'
#' @param x A [`preferences`][preferences] object or a `tibble` with a
#' `preferences`-typed column.
#' @param preferences_col <[`tidy-select`][dplyr::dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the preferences.
#' @param frequency_col <[`tidy-select`][dplyr::dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the frequency of the preferences. If not
#' provided, each row is considered to be observed a single time.
#' @param ... Currently unused.
#'
#' @return An \eqn{N} by \eqn{N} matrix, where \eqn{N} is the number of
#' items.
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
#'   adjacency()
#'
#' @export
adjacency <- function(x,
                      preferences_col = NULL,
                      frequency_col = NULL,
                      ...) {
  # Process into a 2-column tibble with preferences and frequencies columns.
  x <- .validate_preferences_frequencies(
    x,
    {{ preferences_col }},
    {{ frequency_col }}
  ) |>
    dplyr::mutate( # Ensure preferences are complete to give matrice without NAs
      dplyr::across(
        dplyr::where(~ inherits(.x, "preferences")),
        pref_complete
      )
    )

  n <- nlevels(x$preferences)
  m <- x$preferences |>
    pref_length() |>
    max()
  nm <- levels(x$preferences)

  # Map each of the preferences to its own logical adjacency matrix, multiply
  # by weights, then sum them.
  adj <- x$preferences |>
    vapply( # Compute individual adjacencies
      \(pref) {
        rankings <- pref[order(pref[, 1L]), 2L]
        outer(rankings, rankings, `>`) |>
          list()
      },
      list(logical())
    ) |>
    mapply( # Weight by frequency
      FUN = `*`,
      x$frequency,
      SIMPLIFY = FALSE
    ) |>
    Reduce(f = `+`) # Take the sum

  rownames(adj) <- nm
  colnames(adj) <- nm
  structure(adj, class = c("adjacency", "matrix"))
}
