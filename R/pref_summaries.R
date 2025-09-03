#' Covariance matrix for preferences, calculated using the rankings matrix.
#'
#' @param x A vector of preferences, or a tibble with a column of preferences.
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

#' Compute the instant-runoff voting winner for a set of preferences.
#'
#' @description
#' A very rudimentary implementation of the IRV counting algorithm. It does not handle
#' ties elegantly, and should only be used for demonstration purposes. This implementation
#' eliminates all candidates with the fewest first-choice votes in each round until one
#' candidate has a majority or fewer than two candidates remain.
#'
#' @param x A vector of preferences, or a tibble with a column of preferences.
#' @param preferences_col <[`tidy-select`][dplyr::dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the preferences.
#' @param frequency_col <[`tidy-select`][dplyr::dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the frequency of the preferences. If not
#' provided, each row is considered to be observed a single time.
#' @return A list containing:
#' \describe{
#'   \item{winner}{The winning candidate(s) after IRV counting}
#'   \item{rounds}{A list of tibbles, each containing vote tallies for each round}
#'   \item{eliminated}{Character vector of eliminated candidates in order}
#' }
#' @examples
#' # Multi-round election with four candidates
#' prefs <- preferences(c(
#'   "alice > bob > charlie > david",
#'   "alice > bob > charlie > david",
#'   "alice > charlie > bob > david",
#'   "bob > alice > charlie > david",
#'   "bob > charlie > alice > david",
#'   "bob > charlie > alice > david",
#'   "charlie > david > alice > bob",
#'   "charlie > david > bob > alice",
#'   "david > charlie > bob > alice",
#'   "david > charlie > bob > alice"
#' ))
#' result <- pref_irv(prefs)
#' result$winner # Final winner after elimination rounds
#' result$rounds # Vote tallies for each round
#'
#' # Using aggregated data frame
#' df <- tibble::tibble(
#'   prefs = preferences(c(
#'     "alice > bob > charlie > david",
#'     "alice > charlie > bob > david",
#'     "bob > alice > charlie > david",
#'     "bob > charlie > alice > david",
#'     "charlie > david > alice > bob",
#'     "charlie > david > bob > alice",
#'     "david > charlie > bob > alice"
#'   )),
#'   freq = c(2, 1, 1, 2, 1, 1, 2)
#' )
#' pref_irv(df, prefs, freq)
#' @export
pref_irv <- function(x, preferences_col = NULL, frequency_col = NULL) {
  # Process input to construct votes tibble
  if (is.data.frame(x)) {
    preferences_col <- rlang::enquo(preferences_col)
    if (rlang::quo_is_null(preferences_col)) {
      stop("`preferences_col` must be specified when `x` is a data frame.")
    }
    frequency_col <- rlang::enquo(frequency_col)
    vote <- dplyr::pull(x, !!preferences_col)
    if (rlang::quo_is_null(frequency_col)) {
      weight <- rep(1L, length(vote))
    } else {
      weight <- as.integer(dplyr::pull(x, !!frequency_col))
    }
    votes <- tibble::tibble(
      vote,
      weight
    )
  } else {
    votes <- tibble::tibble(
      vote = x,
      weight = rep(1L, length(x))
    )
  }

  # Group by unique vote and sum the weights
  votes <- votes |>
    dplyr::group_by(vote) |>
    dplyr::summarise(weight = sum(weight))

  candidates <- unique(levels(votes$vote))

  rounds <- list()
  remaining <- candidates

  while (length(remaining) > 1L) {
    # Count first-choice votes for remaining candidates
    counts <- votes |>
      dplyr::group_by(candidate = pref_get_items(vote, 1L)) |>
      dplyr::summarise(value = sum(weight), .groups = "drop") |>
      tidyr::unnest("candidate")

    # Record the current round results
    rounds[[length(rounds) + 1L]] <- counts

    # Check if we have a winner (majority)
    if (max(counts$value) > sum(counts$value) / 2.0) {
      winner <- counts$candidate[counts$value == max(counts$value)]
      return(list(
        winner = winner,
        rounds = rounds,
        eliminated = setdiff(candidates, winner)
      ))
    }

    # Find candidate with fewest votes
    eliminated <- counts$candidate[counts$value == min(counts$value)]

    # Eliminate candidate with fewest votes
    remaining <- setdiff(remaining, eliminated)
    votes <- votes |>
      dplyr::mutate(vote = pref_keep(vote, remaining)) |>
      dplyr::group_by(vote) |>
      dplyr::summarise(weight = sum(weight), .groups = "drop")
  }

  # If one candidate remains, they win.
  # If no candidates remain (a tie), nobody wins.
  return(list(
    winner = remaining,
    rounds = rounds,
    eliminated = setdiff(candidates, remaining)
  ))
}
