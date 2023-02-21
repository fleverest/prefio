test_that("`aggregate.preferences` is inverse to `as.preferences`", {
  netflix <- read.preflib("../netflix00004-00000101.soc")
  expect_true(all(netflix == aggregate(as.preferences(netflix))))
})
