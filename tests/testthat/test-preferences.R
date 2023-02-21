item_names <- c("A", "B", "C")

rankings <- matrix(c(
  1, 2, 3,
  3, 2, 1,
  2, 1, 3
  ),
  nrow = 3,
  byrow = TRUE
)
colnames(rankings) <- item_names

long <- data.frame(
  id = rep(1:3, each = 3),
  item = LETTERS[rep(1:3, 3)],
  rank = c(1, 2, 3, 3, 2, 1, 2, 1, 3)
)

ord <- as.data.frame(
  rbind(
    list("A", "B", "C"),
    list("C", "B", "A"),
    list("B", "A", "C")
  )
)

test_that("`preferences` can be constructed from matrices in rankings format", {
  prefs <- preferences(rankings, format = "ranking", item_names = item_names)
  expect_true(all(as.matrix(prefs) == rankings))
})

test_that("`preferences` can be constructed from data.frames in long format", {
  prefs <- preferences(long,
                       format = "long",
                       id = "id",
                       item = "item",
                       rank = "rank")
  expect_true(all(as.matrix(prefs) == rankings))
})

test_that("`preferences` can be constructed from orderings data.frames", {
  prefs <- preferences(ord,
                       format = "ordering")
  expect_true(all(as.matrix(prefs) == rankings))
})

test_that("`as.preferences` is equivalent to `preferences`", {
  prefs1 <- as.preferences(ord,
                           format = "ordering") # Without item_names
  prefs2 <- preferences(ord,
                        format = "ordering",
                        item_names = item_names)
  prefs3 <- as.preferences(long,
                           format = "long",
                           id = "id",
                           rank = "rank",
                           item = "item") # Without item_names
  prefs4 <- preferences(long,
                        format = "long",
                        id = "id",
                        rank = "rank",
                        item = "item",
                        item_names = item_names)
  prefs5 <- as.preferences(rankings,
                           format = "ranking") # Without item_names
  prefs6 <- preferences(rankings,
                        format = "ranking",
                        item_names = item_names)
  expect_true(identical(prefs1, prefs2, prefs3, prefs4, prefs5, prefs6))
})

test_that("`preferences` can be joined by rbind", {
  r2 <- rbind(rankings, rankings)
  p2 <- preferences(r2, format = "ranking")
  p1 <- preferences(rankings, format = "ranking")
  expect_true(all(rbind(p1, p1) == p2))
})

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
