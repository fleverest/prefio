#' Create an Adjacency Matrix for a set of Preferences
#'
#' Convert a set of preferences to an adjacency matrix summarising wins
#' and losses between pairs of items
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
#' `preferences`-typed column to write to file.
#' @param preferences_col <[`tidy-select`][dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the preferences to be written to file.
#' If not provided and `x` is a `tibble`, then
#' @param frequency_col <[`tidy-select`][dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the frequency of the preferences. If not
#' provided, each row is considered to be observed a single time.
#' @param ... Currently unused.
#'
#' @return An \eqn{N} by \eqn{N} matrix, where \eqn{N} is the number of
#' items.
#'
#' @examples
#' x <- tribble(
#'   ~voter_id, ~species, ~food,   ~ranking,
#'   1,         "Rabbit", "Apple",  1,
#'   1,         "Rabbit", "Banana", 2,
#'   1,         "Rabbit", "Carrot", 3,
#'   2,         "Monkey", "Banana", 1,
#'   2,         "Monkey", "Apple",  2,
#'   2,         "Monkey", "Carrot", 3
#' ) |>
#'   long_preferences(food_preference,
#'                    id_cols = voter_id,
#'                    item_col = fruit,
#'                    rank_col = ranking) |>
#'   pull(food_preference) |>
#'   adjacency()
#'
#' @export
adjacency <- function(x,
                      preferences_col = NULL,
                      frequency_col = NULL,
                      ...) {
  # Process into a 2-column tibble with preferences and frequencies columns.
  if (inherits(x, "preferences")) {
    # Convert vector preferences into a tibble with columns `preferences`
    # and `frequency`.
    x <- tibble(preferences = x) |>
      group_by(preferences) |>
      summarise(frequency = n()) |>
      arrange(-frequency)
  } else if (inherits(x, "tbl_df")) {
    # Process tibble.
    # If `preferences_col` is passed, select the appropriate column. Otherwise
    # just look for a preferences-typed column.

    # Get preferences column
    preferences_col <- rlang::enquo(preferences_col)
    if (rlang::quo_is_null(preferences_col)) {
      preferences_col <- rlang::expr(where(~ inherits(.x, "preferences")))
    }
    x_preferences <- x |>
      select(!!preferences_col)
    # Ensure result has one column of "preferences" data.
    preferences_colnames <- x_preferences |>
      sapply(inherits, what = "preferences") |>
      which() |>
      names()
    if (length(preferences_colnames) == 0L) {
      stop(
        "Expected one column of \"preferences\" for ",
        "`write_preflib`, but got 0."
      )
    } else if (length(preferences_colnames) > 1L) {
      warning(
        "Expected one column of \"preferences\" for `write_preflib`, ",
        "but got ", length(preferences_colnames), ". Using `",
        preferences_colnames[1L], "`."
      )
    }
    x_preferences <- x_preferences |>
      select(preferences = preferences_colnames[1L])

    # Get frequency column
    frequency_col <- rlang::enquo(frequency_col)
    if (rlang::quo_is_null(frequency_col)) {
      frequency_col <- NULL
    }
    x_frequency <- x |>
      select(!!frequency_col)
    # Ensure result has one column of "numeric" data.
    if (!is.null(frequency_col)) {
      numeric_colnames <- x_frequency |>
        sapply(is.numeric) |>
        which() |>
        names()
      if (length(numeric_colnames) > 1L) {
        warning(
          "Expected only one column of frequency for `write_preflib`. ",
          "Using `", numeric_colnames[1L], "`."
        )
      }
      x_frequency <- x_frequency |>
        select(frequency = numeric_colnames[1L])
    } else {
      x_frequency <- 1L
    }

    x <- cbind(x_preferences, frequency = x_frequency) |>
      group_by(preferences) |>
      summarise(frequency = sum(frequency)) |>
      arrange(-frequency) |>
      mutate(preferences = pref_complete(preferences))
  }

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
