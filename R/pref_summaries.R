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
#' @param x A vector of preferences, or a tibble with a column of preferences.
#' @param preferences_col <[`tidy-select`][dplyr::dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the preferences.
#' @param frequency_col <[`tidy-select`][dplyr::dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the frequency of the preferences. If not
#' provided, each row is considered to be observed a single time.
#' @return A summary of the results of the IRV contest:
#' ...
#' @examples
#' # Simple contest with five votes between three candidates a, b, and c.
#' pref_irv(preferences(c("a > b > c", "a > b > c", "b > c > a", "b > a > c", "b > a > c")))
#'
#' # Weighted covariance by frequency
#' df <- tibble::tibble(
#'   prefs = preferences(c("a > b > c", "b > c > a", "c > a > b")),
#'   freq = c(3, 2, 1)
#' )
#' pref_irv(df, preferences_col = prefs, frequency_col = freq)
#' @export
pref_irv <- function(x, preferences_col = NULL, frequency_col = NULL) {
  # TODO: implement irv
}
