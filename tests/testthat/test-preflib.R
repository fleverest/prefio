test_that("`preferences` ordinal type is detected appropriately", {
  toc <- read_preflib("../data/aspen00016-00000001.toc")$preferences
  toi <- read_preflib("../data/berkley00017-00000001.toi")$preferences
  soc <- read_preflib("../data/netflix00004-00000101.soc")$preferences
  soi <- read_preflib("../data/glasgow00008-00000003.soi")$preferences
  expect_equal("toc", pref_type(toc))
  expect_equal("toi", pref_type(toi))
  expect_equal("soc", pref_type(soc))
  expect_equal("soi", pref_type(soi))
})

test_that("`read_preflib` throws error when 'ALT NAME X' is missing", {
  expect_error(read_preflib("../data/corrupt/missing_alt.soc"))
})

test_that("`read_preflib` throws error when 'N ALTS' is missing", {
  expect_error(read_preflib("../data/corrupt/missing_nalts.soc"))
})

test_that("`read_preflib` throws error when 'N VTRS' is missing", {
  expect_error(read_preflib("../data/corrupt/missing_nvoters.soc"))
})

test_that("`read_preflib` raises warning when 'N VTRS' differs from data", {
  expect_warning(read_preflib("../data/corrupt/incorrect_n_voters.soc"))
})

test_that("`read_preflib` raises warning when 'N UNQ ORDS' differs from data", {
  expect_warning(
    read_preflib("../data/corrupt/incorrect_n_unique_orders.soc")
  )
})

test_that("`read_preflib` throws error when 'N UNQ ORDS' is not integral", {
  expect_error(read_preflib("../data/corrupt/non_integer_n_unq_ords.soc"))
})

toc <- read_preflib("../data/aspen00016-00000001.toc")
toi <- read_preflib("../data/berkley00017-00000001.toi")
soc <- read_preflib("../data/netflix00004-00000101.soc")
soi <- read_preflib("../data/glasgow00008-00000003.soi")
today <- format(Sys.time(), "%Y-%m-%d")

test_that("`write_preflib` writes to stdout for all test datasets", {
  expect_output(
    write_preflib(toc,
      title = "test toc",
      modification_type = "imbued",
      publication_date = today,
      modification_date = today
    ),
    "# TITLE: test toc"
  )
  expect_output(
    write_preflib(toi,
      title = "test toi",
      modification_type = "imbued",
      publication_date = today,
      modification_date = today
    ),
    "# TITLE: test toi"
  )
  expect_output(
    write_preflib(soc,
      title = "test soc",
      modification_type = "imbued",
      publication_date = today,
      modification_date = today
    ),
    "# TITLE: test soc"
  )
  expect_output(
    write_preflib(soi,
      title = "test soi",
      modification_type = "imbued",
      publication_date = today,
      modification_date = today
    ),
    "# TITLE: test soi"
  )
})

test_that("`write_preflib` produces a file which reads to identical object", {
  pfile <- testthat::capture_output(
    write_preflib(toc,
      frequency_col = frequency,
      title = "test toc tempfile",
      modification_type = "imbued",
      publication_date = today,
      modification_date = today
    )
  )
  expect_true(all(read_preflib(textConnection(pfile)) == toc))
})
