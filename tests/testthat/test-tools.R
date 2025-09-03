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
  )

test_that("Extracting first-preference works using `items_at_rank`", {
  x$pref |>
    pref_get_items(1) |>
    unlist() |>
    expect_equal(c("A", "A", "B"))
})

test_that("Extracting rank works using `rank_of_item`", {
  x |>
    _$pref |>
    pref_get_rank("A") |>
    expect_equal(c(1, 1, 2))
})

test_that("`pref_trunc` keeps only the top preferences by default.", {
  test_pref <- x |>
    _$pref |>
    pref_trunc(1)
  # All should have pref_length 1
  test_pref |>
    pref_length() |>
    expect_equal(c(1, 1, 1))
  # rank 1 item should be preserved
  test_pref |>
    pref_get_items(1) |>
    unlist() |>
    expect_equal(c("A", "A", "B"))
})

test_that("`pref_pop(pref)` achieves the same as `pref_trunc(pref, -1)`", {
  expect_equal(
    x$pref |>
      pref_trunc(-1),
    x$pref |>
      pref_pop()
  )
})

test_that("`rm_items` removes items by name.", {
  # Removing one of the items from the simple example makes all three
  # preferences identical (i.e., '[B]')
  x |>
    _$pref |>
    pref_omit("A") |>
    unique() |>
    length() |>
    expect_equal(1L)
  # Removing all items from the simple example makes all three
  # preferences identical and blank (i.e., '[]')
  x |>
    _$pref |>
    pref_omit(c("A", "B")) |>
    unique() |>
    length() |>
    expect_equal(1L)
})

test_that("`pref_keep` achieves the same as `rm_items`", {
  res1 <- x |>
    _$pref |>
    pref_omit("A")
  res2 <- x |>
    _$pref |>
    pref_keep("B")
  expect_equal(res1, res2)
})

test_that("Removing all items detected by `pref_blank`", {
  # Remove both A and B.
  x |>
    _$pref |>
    pref_omit(c("A", "B")) |>
    pref_blank() |>
    all() |>
    expect_true()
})

# Tests based on examples from pref_tools.R
test_that("pref_blank correctly identifies blank preferences", {
  preferences(c("a > b > c", "", "b > c")) |>
    pref_blank() |>
    expect_equal(c(FALSE, TRUE, FALSE))
  preferences("") |>
    pref_blank() |>
    expect_true()
})

test_that("pref_length correctly counts number of rankings", {
  result <- pref_length(preferences(c("a > b > c", "", "b > c")))
  expect_equal(result, c(3L, 0L, 2L))
})

test_that("pref_get_rank correctly extracts rank of specific item", {
  result <- pref_get_rank(preferences(c("a > b > c", "b > c = a", "")), "a")
  expect_equal(result, c(1L, 2L, NA_integer_))
})

test_that("pref_get_items correctly identifies items at specific rank", {
  # Get items ranked first
  result1 <- pref_get_items(preferences(c("a > b > c", "b = c > a")), rank = 1)
  expect_equal(result1[[1]], "a")
  expect_equal(sort(result1[[2]]), c("b", "c"))

  # Get items ranked second
  result2 <- pref_get_items(preferences(c("a > b > c", "b = c > a")), rank = 2)
  expect_equal(result2[[1]], "b")
  expect_equal(result2[[2]], "a")
})

test_that("pref_add_unranked correctly adds unselected items as last place", {
  result <- pref_add_unranked(preferences(c("a > b", "c > a", "b")))

  # Check that all preferences now have all items
  for (i in seq_along(result)) {
    expect_equal(pref_length(result[i]), 3L)
  }

  # Check specific rankings
  expect_equal(pref_get_rank(result, "c")[1], 3L) # c should be last in first pref
  expect_equal(pref_get_rank(result, "b")[2], 3L) # b should be last in second pref
  expect_equal(pref_get_rank(result, "a")[3], 2L) # a should be last in third pref
  expect_equal(pref_get_rank(result, "c")[3], 2L) # c should be last in third pref
})

test_that("pref_trunc correctly truncates preferences", {
  prefs <- preferences(c("a > b > c > d", "b > c > a"))
  # Keep only the top 2 ranked items
  result1 <- pref_trunc(prefs, n = 2)
  expect_equal(pref_length(result1), c(2L, 2L))
  expect_equal(pref_get_items(result1, 1)[[1]], "a")
  expect_equal(pref_get_items(result1, 2)[[1]], "b")
  expect_equal(pref_get_items(result1, 1)[[2]], "b")
  expect_equal(pref_get_items(result1, 2)[[2]], "c")

  # Keep only the bottom 2 ranked items
  result2 <- pref_trunc(prefs, n = 2, bottom = TRUE)
  expect_equal(pref_length(result2), c(2L, 2L))
  expect_equal(pref_get_items(result2, 1)[[1]], "c")
  expect_equal(pref_get_items(result2, 2)[[1]], "d")
  expect_equal(pref_get_items(result2, 1)[[2]], "c")
  expect_equal(pref_get_items(result2, 2)[[2]], "a")

  # Keep all but the bottom 2 ranked items
  result3 <- pref_trunc(prefs, n = -2)
  expect_equal(pref_length(result3), c(2L, 1L))
  expect_equal(pref_get_items(result3, 1)[[1]], "a")
  expect_equal(pref_get_items(result3, 2)[[1]], "b")
  expect_equal(pref_get_items(result3, 1)[[2]], "b")

  # Keep all but the top 2 ranked items
  result4 <- pref_trunc(prefs, n = -2, bottom = TRUE)
  expect_equal(pref_length(result4), c(2L, 1L))
  expect_equal(pref_get_items(result4, 1)[[1]], "c")
  expect_equal(pref_get_items(result4, 2)[[1]], "d")
  expect_equal(pref_get_items(result4, 1)[[2]], "a")
})

test_that("pref_omit correctly removes specified items", {
  # Remove 'b' from preference rankings
  result1 <- pref_omit(preferences(c("a > b > c", "b > c > a")), "b")
  expect_equal(pref_get_items(result1, 1)[[1]], "a")
  expect_equal(pref_get_items(result1, 1)[[2]], "c")

  # Remove multiple items
  result2 <- pref_omit(preferences(c("a > b > c > d", "b > c > a > d")), c("b", "d"))
  expect_equal(pref_length(result2), c(2L, 2L))
  expect_equal(
    sort(unlist(lapply(result2, function(p) levels(result2)[p[, 1]]))),
    c("a", "a", "c", "c")
  )
})

test_that("pref_keep correctly keeps only specified items", {
  result <- pref_keep(preferences(c("a > b > c", "b > c > a")), c("a", "c"))
  expect_equal(pref_length(result), c(2L, 2L))

  # Check that only a and c appear in the preferences
  all_items <- unlist(lapply(seq_along(result), function(i) {
    unlist(pref_get_items(result[i], 1:2))
  }))
  expect_true(all(all_items %in% c("a", "c")))
})

test_that("pref_pop correctly eliminates lowest ranked items", {
  # Remove the lowest ranked item from each preference
  result1 <- pref_pop(preferences(c("a > b > c", "b > c > a")))
  expect_equal(pref_length(result1), c(2L, 2L))
  expect_false("c" %in% unlist(pref_get_items(result1[1], 1:2)))
  expect_false("a" %in% unlist(pref_get_items(result1[2], 1:2)))

  # Remove the 2 lowest ranked items
  result2 <- pref_pop(preferences(c("a > b > c > d", "b > c > a > d")), n = 2)
  expect_equal(pref_length(result2), c(2L, 2L))

  # Remove the highest ranked item instead
  result3 <- pref_pop(preferences(c("a > b > c", "b > c > a")), lowest = FALSE)
  expect_equal(pref_length(result3), c(2L, 2L))
  expect_false("a" %in% unlist(pref_get_items(result3[1], 1:2)))
  expect_false("b" %in% unlist(pref_get_items(result3[2], 1:2)))

  # Remove blank preferences that result from popping
  result4 <- pref_pop(preferences(c("a > b", "c", "")), drop = TRUE)
  expect_equal(length(result4), 1L) # The blank ones should be gone
  expect_equal(pref_length(result4), 1L)
})

test_that("pref_rev correctly reverses preference rankings", {
  result <- pref_rev(preferences(c("a > b > c", "b > c > a")))

  # First preference should now be last
  expect_equal(pref_get_rank(result, "a")[1], 3L)
  expect_equal(pref_get_rank(result, "c")[1], 1L)

  expect_equal(pref_get_rank(result, "b")[2], 3L)
  expect_equal(pref_get_rank(result, "a")[2], 1L)

  # Test empty preference
  empty_result <- pref_rev(preferences(""))
  expect_true(pref_blank(empty_result))
})
