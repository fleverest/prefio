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

test_that("pref_irv returns expected structure", {
  # Simple 3-candidate election
  prefs <- preferences(c("a > b > c", "b > a > c", "c > a > b"))
  result <- pref_irv(prefs)

  # Check return structure
  expect_true(is.list(result))
  expect_true(all(c("winner", "rounds", "eliminated") %in% names(result)))
  expect_true(is.character(result$winner))
  expect_true(is.list(result$rounds))
  expect_true(is.character(result$eliminated))
})

test_that("pref_irv handles simple majority winner", {
  # Clear majority winner (no elimination needed)
  prefs <- preferences(c("a > b > c", "a > c > b", "a > b > c", "b > a > c", "c > b > a"))
  result <- pref_irv(prefs)

  expect_equal(result$winner, "a")
  expect_equal(length(result$rounds), 1) # Only one round needed
})

test_that("pref_irv handles elimination rounds", {
  # Election requiring multiple rounds
  prefs <- preferences(c(
    "a > c > b",
    "b > a > c",
    "b > a > c",
    "c > a > b",
    "c > a > b",
    "c > b > a"
  ))
  result <- pref_irv(prefs)

  expect_true(length(result$rounds) > 1) # Multiple rounds needed
  expect_true(length(result$winner) == 1) # Single winner
  expect_true(result$winner %in% c("a", "b", "c"))
})

test_that("pref_irv works with data frame input", {
  # Using data frame with frequency column
  df <- tibble::tibble(
    votes = preferences(c("a > b > c", "b > a > c", "c > a > b")),
    freq = c(3, 2, 1)
  )
  result <- pref_irv(df, preferences_col = votes, frequency_col = freq)

  expect_equal(result$winner, "a") # Should win with weighted votes
})

test_that("pref_irv handles single candidate", {
  # Edge case: only one candidate
  prefs <- preferences(c("a", "a", "a"))
  result <- pref_irv(prefs)

  expect_equal(result$winner, "a")
  expect_equal(length(result$rounds), 0)
  expect_equal(length(result$eliminated), 0)
})

test_that("pref_irv handles two candidates", {
  # Simple two-candidate race
  prefs <- preferences(c("a > b", "a > b", "b > a"))
  result <- pref_irv(prefs)

  expect_equal(result$winner, "a")
  expect_equal(length(result$rounds), 1)
  expect_equal(result$eliminated, "b")
})

test_that("pref_irv errors appropriately", {
  # Should error when data frame used without preferences_col
  df <- tibble::tibble(
    prefs = preferences(c("a > b > c", "b > a > c")),
    freq = c(1, 1)
  )

  expect_error(pref_irv(df), "`preferences_col` must be specified")
})
