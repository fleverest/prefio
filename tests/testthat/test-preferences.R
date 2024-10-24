test_that("`preferences` can be constructed from long format", {
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
  expect_true(
    syd$ballot_type |>
    inherits("preferences")
  )
})

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

test_that("`preferences` with altered `item_names` are not equal", {
  prefs <- syd$ballot_type
  prefs2 <- prefs
  levels(prefs2) <- LETTERS[1:5]
  expect_false(any(prefs == prefs2))
})

test_that("Equality and inequality work for `preferences`", {
  prefs <- syd$ballot_type
  prefs2 <- prefs
  expect_true(all(prefs == prefs2))
  levels(prefs2) <- LETTERS[1:5]
  expect_true(all(prefs != prefs2))
})

test_that("`print.preference` formats correctly", {
  prefs <- syd |>
    dplyr::group_by(ballot_type) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::mutate(ballot_type = pref_complete(ballot_type)) |>
    head(1)
  expect_output(print(prefs$ballot_type), "\\[GREENWICH Alex > STANTON Phyllisse = TITO Skye = WARD Nick = WHITTON Mark\\]")

  prefs <- prefs |>
    dplyr::mutate(ballot_type = pref_trunc(ballot_type, 0)) 
  expect_output(
    print(prefs$ballot_type),
    "\\[\\]"
  )
})

test_that("Some valid examples of `preferences` are not `na`", {
  expect_true(
    !any(is.na(
      read_preflib("../data/aspen00016-00000001.toc")$preferences
    )) &&
      !any(is.na(
        read_preflib("../data/glasgow00008-00000003.soi")$preferences
      )) &&
      !any(is.na(
        read_preflib("../data/netflix00004-00000101.soc")$preferences
      )) &&
      !any(is.na(
        read_preflib("../data/berkley00017-00000001.toi")$preferences
      ))
  )
})
