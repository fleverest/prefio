syd <- "../data/sydney_2023.tsv" |>
  readr::read_tsv(show_col_types = FALSE) |>
  tidyr::drop_na() |>
  long_preferences(
    ballot_type,
    id_cols = BPNumber,
    item_col = CandidateName,
    rank_col = PrefCounted,
    unused_fn = list(PollingPlaceName = dplyr::first, District = dplyr::first)
  )

test_that("`ranking_matrix` is correct on simple example", {
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
    ) |>
    ranking_matrix()
  expect_equal(x[, "A"], c(1, 1, 2))
  expect_equal(x[, "B"], c(2, 2, 1))
})

test_that("`ranking_matrix` runs on Sydney 2023 dataset", {
  x <- syd |>
    ranking_matrix()
  expect_true(x[1, "GREENWICH Alex"] == 1)
  expect_true(is.na(x[1, "TITO Skye"]))
})
