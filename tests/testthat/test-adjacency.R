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

test_that("`adjacency` works on `preferences`", {
  expect_true(all(adjacency(syd) >= 0))
})

test_that("`adjacency` is consistent when applying after pre-aggregating", {
  adj1 <- adjacency(syd$ballot_type)
  adj2 <- adjacency(syd, preferences_col = ballot_type)
  adj3 <- syd |>
    dplyr::group_by(ballot_type) |>
    dplyr::summarise(frequency = dplyr::n()) |>
    adjacency(preferences_col = ballot_type, frequency_col = frequency)
  # Should be same
  expect_equal(adj1, adj2)
  expect_equal(adj2, adj3)
})
