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
