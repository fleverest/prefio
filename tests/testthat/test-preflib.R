test_that("`preferences` ordinal type is detected appropriately", {
  toc <- read.preflib("../aspen00016-00000001.toc")$preferences
  toi <- read.preflib("../berkley00017-00000001.toi")$preferences
  soc <- read.preflib("../netflix00004-00000101.soc")$preferences
  soi <- read.preflib("../glasgow00008-00000003.soi")$preferences
  expect_equal("toc", attr(toc, "preftype"))
  expect_equal("toi", attr(toi, "preftype"))
  expect_equal("soc", attr(soc, "preftype"))
  expect_equal("soi", attr(soi, "preftype"))
})

test_that("`read.preflib` throws error when 'ALT NAME X' is missing", {
  expect_error(read.preflib("../corrupt_data/missing_alt.soc"))
})

test_that("`read.preflib` throws error when 'N ALTS' is missing", {
  expect_error(read.preflib("../corrupt_data/missing_nalts.soc"))
})

test_that("`read.preflib` throws error when 'N VTRS' is missing", {
  expect_error(read.preflib("../corrupt_data/missing_nvoters.soc"))
})

test_that("`read.preflib` raises warning when 'N VTRS' differs from data", {
  expect_warning(read.preflib("../corrupt_data/incorrect_n_voters.soc"))
})

test_that("`read.preflib` raises warning when 'N UNQ ORDS' differs from data", {
  expect_warning(read.preflib("../corrupt_data/incorrect_n_voters.soc"))
})
