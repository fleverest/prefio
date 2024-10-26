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
  x |>
    _$pref |>
    items_at_rank(1) |>
    unlist() |>
    expect_equal(c("A", "A", "B"))
})

test_that("Extracting rank works using `rank_of_item`", {
  x |>
    _$pref |>
    rank_of_item("A") |>
    expect_equal(c(1, 1, 2))
})

test_that("`pref_trunc` keeps only the top preferences.", {
  test_pref <- x |>
    _$pref |>
    pref_trunc(1)
  # All should have pref_length 1
  test_pref |>
    pref_length() |>
    expect_equal(c(1, 1, 1))
  # rank 1 item should be preserved
  test_pref |>
    items_at_rank(1) |>
    unlist() |>
    expect_equal(c("A", "A", "B"))
})

test_that("`pref_pop` achieves the same as `pref_trunc` for a simple example", {
  test_pref <- x |>
    _$pref |>
    pref_trunc(1) |>
    expect_equal(
      x |>
        _$pref |>
        pref_pop()
    )
})

test_that("`rm_items` removes items by name.", {
  # Removing one of the items from the simple example makes all three
  # preferences identical (i.e., '[B]')
  x |>
    _$pref |>
    rm_items("A") |>
    unique() |>
    length() |>
    expect_equal(1L)
  # Removing all items from the simple example makes all three
  # preferences identical and blank (i.e., '[]')
  x |>
    _$pref |>
    rm_items(c("A", "B")) |>
    unique() |>
    length() |>
    expect_equal(1L)
})

test_that("`pref_project` achieves the same as `rm_items`", {
  x |>
    _$pref |>
    rm_items("A") |>
    expect_equal(
      x |>
        _$pref |>
        pref_project("B")
    )
})

test_that("Removing all items detected by `pref_blank`", {
  # Remove both A and B.
  x |>
    _$pref |>
    rm_items(c("A", "B")) |>
    pref_blank() |>
    all() |>
    expect_equal(TRUE)
})

test_that("`pref_cov` doesn't return NA for simple example", {
  x |>
    _$pref |>
    pref_cov() |>
    _$cov |>
    is.na() |>
    any() |>
    expect_false()
})
