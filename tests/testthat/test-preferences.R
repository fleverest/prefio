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

test_that("long_preferences handles basic conversion correctly", {
  # Create a test dataframe with long format preferences
  long_data <- tibble::tribble(
    ~voter_id, ~region, ~food, ~ranking,
    1, "North", "Apple", 2,
    1, "North", "Banana", 1,
    1, "North", "Carrot", 3,
    2, "South", "Apple", 2,
    2, "South", "Banana", 3,
    2, "South", "Carrot", 1,
    3, "East", "Apple", 1,
    3, "East", "Banana", 2,
    3, "East", "Carrot", 3
  )

  # Convert to preferences
  result <- long_preferences(
    long_data,
    col = food_pref,
    id_cols = c(voter_id, region),
    item_col = food,
    rank_col = ranking
  )

  # Test that the result has the right structure
  expect_true(
    result$food_pref |>
      inherits("preferences")
  )

  # Test that the ID columns are preserved
  expect_true(all(c("voter_id", "region") %in% colnames(result)))

  # Test that item_col and rank_col are removed
  expect_false(any(c("food", "ranking") %in% colnames(result)))

  # Test that the preference levels are set correctly
  expect_equal(
    levels(result$food_pref),
    c("Apple", "Banana", "Carrot")
  )

  # Test correct preference ordering for first voter
  first_pref <- format(result$food_pref[1])
  expect_match(first_pref, "Banana > Apple > Carrot", fixed = TRUE)
})

test_that("long_preferences handles NA values with drop_rows option", {
  # Create a test dataframe with some NA values
  long_data <- tibble::tribble(
    ~voter_id, ~food, ~ranking,
    1, "Apple", 2,
    1, "Banana", 1,
    1, "Carrot", 3,
    2, "Apple", NA, # NA ranking
    2, "Banana", 3,
    2, "Carrot", 1,
    3, "Apple", 1,
    3, NA, 2, # NA item
    3, "Carrot", 3
  )

  # Expect a message about dropping NA values with drop_rows (default)
  expect_message(
    result <- long_preferences(
      long_data,
      col = food_pref,
      id_cols = voter_id,
      item_col = food,
      rank_col = ranking,
      na_action = "drop_rows"
    ),
    "Found rows containing `NA`"
  )

  # Check that rows with NAs were dropped but other rows were kept
  expect_true(2 %in% result$voter_id) # Voter 2 still included with complete rows
  expect_true(3 %in% result$voter_id) # Voter 3 still included with complete rows

  # Check that the preference for voter 2 only includes Banana and Carrot
  second_pref <- format(result$food_pref[result$voter_id == 2])
  expect_match(second_pref, "Carrot > Banana", fixed = TRUE)
  expect_false(grepl("Apple", second_pref))

  # Check that the preference for voter 3 only includes Apple and Carrot
  third_pref <- format(result$food_pref[result$voter_id == 3])
  expect_match(third_pref, "Apple > Carrot", fixed = TRUE)
  expect_false(grepl("Banana", third_pref))
})

test_that("long_preferences handles NA values with drop_preferences option", {
  # Create a test dataframe with some NA values
  long_data <- tibble::tribble(
    ~voter_id, ~food, ~ranking,
    1, "Apple", 2,
    1, "Banana", 1,
    1, "Carrot", 3,
    2, "Apple", NA, # NA ranking
    2, "Banana", 3,
    2, "Carrot", 1,
    3, "Apple", 1,
    3, NA, 2, # NA item
    3, "Carrot", 3
  )

  # Expect a message about dropping preferences with NAs
  expect_message(
    result <- long_preferences(
      long_data,
      col = food_pref,
      id_cols = voter_id,
      item_col = food,
      rank_col = ranking,
      na_action = "drop_preferences"
    ),
    "Found rows containing `NA`"
  )

  # Check that only voter 1 is kept (the only one with no NAs)
  expect_equal(result$voter_id, 1)
  expect_equal(nrow(result), 1)

  # Check that the preference is complete
  first_pref <- format(result$food_pref[1])
  expect_match(first_pref, "Banana > Apple > Carrot", fixed = TRUE)
})

test_that("long_preferences handles duplicate items correctly", {
  # Create data with duplicate items
  long_data <- tibble::tribble(
    ~voter_id, ~food, ~ranking,
    1, "Apple", 2,
    1, "Apple", 1, # Duplicate item with different rank
    1, "Carrot", 3,
    2, "Apple", 2,
    2, "Banana", 1,
    2, "Carrot", 3
  )

  # Expect a message about duplicate items
  expect_message(
    result <- long_preferences(
      long_data,
      col = food_pref,
      id_cols = voter_id,
      item_col = food,
      rank_col = ranking
    ),
    "Duplicated rankings per item detected"
  )

  # First voter should use the highest rank (1) for Apple
  first_pref <- format(result$food_pref[1])
  expect_match(first_pref, "Apple > Carrot", fixed = TRUE)
})

test_that("long_preferences requires integer-valued rankings", {
  # Create data with non-integer rankings
  long_data <- tibble::tribble(
    ~voter_id, ~food, ~ranking,
    1, "Apple", 2,
    1, "Banana", 1.5, # Non-integer rank
    1, "Carrot", 3
  )

  # Expect an error about non-integer ranks
  expect_error(
    long_preferences(
      long_data,
      col = food_pref,
      id_cols = voter_id,
      item_col = food,
      rank_col = ranking
    ),
    "`rank` must be integer-valued"
  )
})

test_that("long_preferences handles ties through dense ranking", {
  # Create data with duplicate ranks (ties)
  long_data <- tibble::tribble(
    ~voter_id, ~food, ~ranking,
    1, "Apple", 1,
    1, "Banana", 1, # Same rank as Apple (tie)
    1, "Carrot", 2,
    2, "Apple", 2,
    2, "Banana", 1,
    2, "Carrot", 2 # Same rank as Apple (tie)
  )

  result <- long_preferences(
    long_data,
    col = food_pref,
    id_cols = voter_id,
    item_col = food,
    rank_col = ranking
  )

  # Check that ties are preserved in the formatted output
  first_pref <- format(result$food_pref[1])
  second_pref <- format(result$food_pref[2])

  expect_match(first_pref, "Apple = Banana > Carrot", fixed = TRUE)
  expect_match(second_pref, "Banana > Apple = Carrot", fixed = TRUE)
})

test_that("long_preferences handles item_names parameter correctly", {
  # Create data with numeric item identifiers
  long_data <- tibble::tribble(
    ~voter_id, ~item_id, ~ranking,
    1, 1, 2,
    1, 2, 1,
    1, 3, 3,
    2, 1, 2,
    2, 2, 3,
    2, 3, 1
  )

  # Define item names mapping
  item_names <- c("Apple", "Banana", "Carrot")

  result <- long_preferences(
    long_data,
    col = food_pref,
    id_cols = voter_id,
    item_col = item_id,
    rank_col = ranking,
    item_names = item_names
  )

  # Check that item names are correctly mapped
  expect_equal(levels(result$food_pref), item_names)

  # Check first preference ordering
  first_pref <- format(result$food_pref[1])
  expect_match(first_pref, "Banana > Apple > Carrot", fixed = TRUE)
})

test_that("long_preferences handles unused_fn parameter correctly", {
  # Create data with additional columns
  long_data <- tibble::tribble(
    ~voter_id, ~region, ~age, ~food, ~ranking,
    1, "North", 25, "Apple", 2,
    1, "North", 25, "Banana", 1,
    1, "North", 25, "Carrot", 3,
    2, "South", 30, "Apple", 2,
    2, "South", 30, "Banana", 3,
    2, "South", 30, "Carrot", 1
  )

  # Use unused_fn to keep additional columns
  result <- long_preferences(
    long_data,
    col = food_pref,
    id_cols = voter_id,
    item_col = food,
    rank_col = ranking,
    unused_fn = list(
      region = dplyr::first,
      age = dplyr::first
    )
  )
  # Check that unused columns are preserved
  expect_true(all(c("region", "age") %in% colnames(result)))

  # Check that values are correctly aggregated
  expect_equal(result$region, c("North", "South"))
  expect_equal(result$age, c(25, 30))
})

test_that("`preferences` can be constructed from wide format", {
  # Create a test dataframe with wide format preferences
  wide_data <- tibble::tibble(
    voter_id = c(1, 2, 3),
    region = c("North", "South", "East"),
    Apple = c(2, 2, 1),
    Banana = c(1, 3, 2),
    Carrot = c(3, 1, 3)
  )

  # Convert to preferences
  result <- wide_preferences(
    wide_data,
    col = food_pref,
    ranking_cols = c(Apple, Banana, Carrot)
  )

  # Test that the result has the right structure
  expect_true(
    result$food_pref |>
      inherits("preferences")
  )

  # Test that the original ID columns are preserved
  expect_true(all(c("voter_id", "region") %in% colnames(result)))

  # Test that the ranking columns are removed
  expect_false(any(c("Apple", "Banana", "Carrot") %in% colnames(result)))

  # Test that the levels are set correctly
  expect_equal(
    levels(result$food_pref),
    c("Apple", "Banana", "Carrot")
  )
})

test_that("wide_preferences handles NA values with keep_as_partial option", {
  # Create a test dataframe with some NA values
  wide_data <- tibble::tibble(
    voter_id = c(1, 2, 3),
    Apple = c(2, NA, 1),
    Banana = c(1, 3, 2),
    Carrot = c(3, 1, NA)
  )

  # Expect a message about NA values
  expect_message(
    result <- wide_preferences(
      wide_data,
      ranking_cols = c(Apple, Banana, Carrot),
      na_action = "keep_as_partial"
    ),
    "Found rows containing `NA`"
  )

  # Check that all rows are kept
  expect_equal(nrow(result), 3)
  expect_equal(result$voter_id, c(1, 2, 3))

  # Check that NAs are excluded from the preferences
  # Voter 1 should have complete ordering
  first_pref <- format(result$preferences[1])
  expect_match(first_pref, "Banana > Apple > Carrot", fixed = TRUE)

  # Voter 2 should only have Banana and Carrot (no Apple)
  second_pref <- format(result$preferences[2])
  expect_match(second_pref, "Carrot > Banana", fixed = TRUE)
  expect_false(grepl("Apple", second_pref))

  # Voter 3 should only have Apple and Banana (no Carrot)
  third_pref <- format(result$preferences[3])
  expect_match(third_pref, "Apple > Banana", fixed = TRUE)
  expect_false(grepl("Carrot", third_pref))
})

test_that("wide_preferences handles NA values with drop_preferences option", {
  # Create a test dataframe with some NA values
  wide_data <- tibble::tibble(
    voter_id = c(1, 2, 3),
    Apple = c(2, NA, 1),
    Banana = c(1, 3, 2),
    Carrot = c(3, 1, NA)
  )

  # Use expect_snapshot to capture both messages about NA handling
  expect_snapshot({
    result <- wide_preferences(
      wide_data,
      ranking_cols = c(Apple, Banana, Carrot),
      na_action = "drop_preferences"
    )
  })

  # Check that only voter 1 is kept (the only one with no NAs)
  expect_equal(nrow(result), 1)
  expect_equal(result$voter_id, 1)

  # Check that the preference is complete
  first_pref <- format(result$preferences[1])
  expect_match(first_pref, "Banana > Apple > Carrot", fixed = TRUE)
})

test_that("wide_preferences handles tie rankings correctly", {
  # Create data with tied rankings
  wide_data <- tibble::tibble(
    voter_id = c(1, 2),
    Apple = c(1, 2),
    Banana = c(1, 1), # Tied with Apple for voter 1
    Carrot = c(2, 3) # Tied with Apple for voter 1
  )

  # Expect a message about duplicate ranks
  expect_message(
    result <- wide_preferences(
      wide_data,
      ranking_cols = c(Apple, Banana, Carrot)
    ),
    "Duplicate ranks detected"
  )

  # First voter should have Apple and Banana tied at rank 1
  first_pref <- format(result$preferences[1])
  expect_match(first_pref, "Apple = Banana", fixed = TRUE)
})

test_that("wide_preferences requires integer-valued rankings", {
  # Create data with non-integer rankings
  wide_data <- tibble::tibble(
    voter_id = c(1, 2),
    Apple = c(1, 2.5), # Non-integer rank
    Banana = c(2, 1),
    Carrot = c(3, 3)
  )

  # Expect an error about non-integer ranks
  expect_error(
    wide_preferences(
      wide_data,
      ranking_cols = c(Apple, Banana, Carrot)
    ),
    "`rank` must be integer-valued"
  )
})

test_that("wide_preferences and long_preferences produce equivalent results", {
  # Create sample data in both formats
  wide_data <- tibble::tibble(
    voter_id = c(1, 2),
    Apple = c(3, 2),
    Banana = c(1, 1),
    Carrot = c(2, 3)
  )

  long_data <- tibble::tribble(
    ~voter_id, ~food, ~ranking,
    1, "Apple", 3,
    1, "Banana", 1,
    1, "Carrot", 2,
    2, "Apple", 2,
    2, "Banana", 1,
    2, "Carrot", 3
  )

  # Convert both to preferences
  wide_result <- wide_preferences(
    wide_data,
    col = food_pref,
    ranking_cols = c(Apple, Banana, Carrot)
  )

  long_result <- long_preferences(
    long_data,
    col = food_pref,
    id_cols = voter_id,
    item_col = food,
    rank_col = ranking
  )

  # Format the results for comparison
  wide_formatted <- format(wide_result$food_pref)
  long_formatted <- format(long_result$food_pref)

  # They should produce the same preference orderings
  expect_equal(wide_formatted[1], long_formatted[1])
  expect_equal(wide_formatted[2], long_formatted[2])
})

test_that("wide_preferences and long_preferences handle NAs consistently", {
  # Create data with NAs in both formats
  wide_data <- tibble::tibble(
    voter_id = c(1, 2),
    Apple = c(3, NA),
    Banana = c(1, 1),
    Carrot = c(2, 3)
  )

  long_data <- tibble::tribble(
    ~voter_id, ~food, ~ranking,
    1, "Apple", 3,
    1, "Banana", 1,
    1, "Carrot", 2,
    2, "Apple", NA, # NA ranking
    2, "Banana", 1,
    2, "Carrot", 3
  )

  # Convert both to preferences using the partial ordering approach
  wide_result <- wide_preferences(
    wide_data,
    col = food_pref,
    ranking_cols = c(Apple, Banana, Carrot),
    na_action = "keep_as_partial",
    verbose = FALSE
  )

  long_result <- long_preferences(
    long_data,
    col = food_pref,
    id_cols = voter_id,
    item_col = food,
    rank_col = ranking,
    na_action = "drop_rows",
    verbose = FALSE
  )

  # Format the results for comparison
  wide_formatted <- format(wide_result$food_pref)
  long_formatted <- format(long_result$food_pref)

  # Check that both approaches handle NAs consistently
  expect_equal(wide_formatted[2], long_formatted[2])
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
    dplyr::mutate(ballot_type = pref_add_unranked(ballot_type)) |>
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

test_that("as_preferences handles string input correctly", {
  # Basic test with default separator
  prefs <- as_preferences(c("A>B>C", "C>B>A", "B>A>C"))
  expect_equal(length(prefs), 3)
  expect_equal(levels(prefs), c("A", "B", "C"))

  # Test formatting
  expect_equal(format(prefs)[1], "[A > B > C]")
  expect_equal(format(prefs)[2], "[C > B > A]")
  expect_equal(format(prefs)[3], "[B > A > C]")
})

test_that("as_preferences handles custom separators", {
  prefs <- as_preferences(c("A|B|C", "C|B|A"), sep = "|")
  expect_equal(length(prefs), 2)
  expect_equal(format(prefs)[1], "[A > B > C]")
  expect_equal(format(prefs)[2], "[C > B > A]")
})

test_that("as_preferences handles equality between items", {
  prefs <- as_preferences(c("A=B>C", "C>B=A"))
  expect_equal(length(prefs), 2)
  expect_equal(format(prefs)[1], "[A = B > C]")
  expect_equal(format(prefs)[2], "[C > B = A]")

  # Custom equality symbol
  prefs <- as_preferences(c("A:B>C", "C>B:A"), equality = ":")
  expect_equal(format(prefs)[1], "[A = B > C]")
  expect_equal(format(prefs)[2], "[C > B = A]")
})

test_that("as_preferences handles descending parameter", {
  # When descending=FALSE, the order is reversed
  prefs_desc <- as_preferences(c("A>B>C", "C>B>A"), descending = TRUE)
  prefs_asc <- as_preferences(c("A>B>C", "C>B>A"), descending = FALSE)

  # The first preference should be opposite
  expect_equal(format(prefs_desc)[1], "[A > B > C]")
  expect_equal(format(prefs_asc)[1], "[C > B > A]")

  # The second preference should be opposite
  expect_equal(format(prefs_desc)[2], "[C > B > A]")
  expect_equal(format(prefs_asc)[2], "[A > B > C]")
})

test_that("as_preferences handles mixed separators and equality", {
  prefs <- as_preferences(c("A|B|C:D", "C|B|A"), sep = "|", equality = ":", descending = FALSE)
  expect_equal(length(prefs), 2)
  expect_equal(format(prefs)[1], "[C = D > B > A]") # Order reversed due to descending=FALSE
  expect_equal(format(prefs)[2], "[A > B > C]") # Order reversed due to descending=FALSE
})

test_that("preferences() function creates single preference", {
  pref <- preferences("A>B>C")
  expect_equal(length(pref), 1)
  expect_equal(format(pref), "[A > B > C]")
})

test_that("preferences() handles empty inputs", {
  # Empty string vector
  pref1 <- preferences()
  expect_equal(length(pref1), 0)
  expect_output(print(pref1), "preferences\\(0\\)")

  # Single empty string
  pref2 <- preferences("")
  expect_equal(length(pref2), 1)
  expect_output(print(pref2), "\\[\\]")
})

test_that("as_preferences handles empty and NA strings", {
  prefs <- as_preferences(c("A>B>C", "", NA, "B>A>C"))
  expect_equal(length(prefs), 4)

  # First and fourth elements should be parsed correctly
  expect_equal(format(prefs)[1], "[A > B > C]")
  expect_equal(format(prefs)[4], "[B > A > C]")

  # Second and third elements should be empty
  expect_equal(format(prefs)[2], "[]")
  expect_equal(format(prefs)[3], "[]")
})

test_that("as_preferences handles whitespace correctly", {
  prefs <- as_preferences(c("A > B > C", " C>B>A ", "B> A >C"))
  expect_equal(length(prefs), 3)
  expect_equal(format(prefs)[1], "[A > B > C]")
  expect_equal(format(prefs)[2], "[C > B > A]")
  expect_equal(format(prefs)[3], "[B > A > C]")
})
