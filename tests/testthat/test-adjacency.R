syd <- "../prefio_tidy/data//SG2301 LA Pref Data Sydney.txt" |>
  read_tsv() |>
  drop_na() |>
  long_preferences(
    ballot_type,
    id_cols = BPNumber,
    item_col = CandidateName,
    rank_col = PrefCounted,
    unused_fn = list(PollingPlaceName = first, District = first)
  )

test_that("`adjacency` works on `preferences`", {
  expect_success(adjacency(syd))
  expect_true(all(adjacency(syd) >= 0))
})

test_that("`adjacency` is consistent when aggregating", {
  adj1 <- adjacency(syd)
  adj2 <- syd |>
    group_by(ballot_type) |>
    summary(frequency = n()) |>
    adjacency(x = _, preferences_col = ballot_type, frequency_col = frequency)
  # Should be same
  expect_equal(adj1, adj2)
})
