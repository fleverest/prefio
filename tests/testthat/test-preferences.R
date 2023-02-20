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
    list(1, 2, 3),
    list(3, 2, 1),
    list(2, 1, 3)
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
                       format = "ordering",
                       item_names = item_names)
  expect_true(all(as.matrix(prefs) == rankings))
})

test_that("`preferences` can be joined by rbind", {
  r2 <- rbind(rankings, rankings)
  p2 <- preferences(r2, format = "ranking")
  p1 <- preferences(ranking, format = "ranking")
  expect_true(all(rbind(p1, p1) == p2))
})
