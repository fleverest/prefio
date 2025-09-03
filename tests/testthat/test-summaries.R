x <- tibble::tibble(
  id = c(1, 1, 2, 2, 3, 3),
  item = c("A", "B", "A", "B", "B", "A"),
  rank = c(1, 2, 1, 2, 1, 2)
) |>
  long_preferences(
    pref,
    id_cols = id,
    rank_col = rank,
    item_col = item
  )

test_that("`pref_cov` doesn't return NA for simple example", {
  x |>
    _$pref |>
    pref_cov() |>
    _$cov |>
    is.na() |>
    any() |>
    expect_false()
})

test_that("pref_cov handles weighted calculations", {
  # Simple covariance on a vector of preferences
  prefs <- preferences(c("a > b > c", "b > c > a", "c > a > b"))
  result1 <- pref_cov(prefs)
  expect_true("cov" %in% names(result1))
  expect_equal(dim(result1$cov), c(3, 3))

  # Weighted covariance by frequency
  df <- tibble::tibble(
    prefs = preferences(c("a > b > c", "b > c > a")),
    freq = c(3, 2)
  )
  result2 <- pref_cov(df, preferences_col = prefs, frequency_col = freq)
  expect_true("cov" %in% names(result2))
  expect_equal(dim(result2$cov), c(3, 3))
})
