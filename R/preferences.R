#' @title Preferences Objects
#' @name preferences
#'
#' @description
#' A tidy interface for working with ordinal preferences.
#'
#' @param data A `data.frame` or `tibble` to extract preferences from
#' @param format The format of the data: one of "ordering", "ranking", or
#' "long" (see above). By default, `data` is assumed to be in "long" format.
#' @param col The name of the new column, as a string or symbol.
#' @param ranking_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> The columns from which
#' to extract wide-format preferences.
#' @param id_cols <[`tidy-select`][dplyr::dplyr_tidy_select]> The columns by which to
#' group the dataset to extract a single preference selection.
#' @param item_col <[`tidy-select`][dplyr::dplyr_tidy_select]> For `data` in
#' long-format: the column representing the items by name or by index, in which
#' case the `item_names` parameter should also be passed.
#' @param rank_col <[`tidy-select`][dplyr::dplyr_tidy_select]> For `data` in
#' long-format: the column representing the rank for the associated item.
#' @param item_names The names of the full set of items. This is necessary when
#' the dataset specifies items by index rather than by name, or when there are
#' items which do not appear in any preference selection.
#' @param verbose If `TRUE`, diagnostic messages will be sent to stdout.
#' @param unused_fn When `format="long"`, passes parameter to
#' `dplyr::pivot_wider` to summarise values from unused columns. This can be
#' helpful to keep covariates from columns which are unused in processing
#' preferences, but are important for upstream tasks. See
#' <[`pivot_wider`][tidyr::pivot_wider]> for more details and other uses.
#' @param na_action Specifies how to handle NA values.
#' \describe{
#'   \item{`long_preferences`}{
#'     \describe{
#'       \item{`"drop_rows"`}{Removes individual rows containing NA values before processing}
#'       \item{`"drop_preferences"`}{Removes the entire preference selection that contains any NA}
#'     }
#'   }
#'   \item{`wide_preferences`}{
#'     \describe{
#'       \item{`"keep"`}{Interprets rows containing NAs as partial orderings}
#'       \item{`"drop"`}{Removes preferences with any NA ranks}
#'     }
#'   }
#' }
#' @param ... Unused.
#'
#' @examples
#' # Votes cast by two animals ranking a variety of fruits and vegetables.
#' # This is not real data, I made this up.
#' x <- tibble::tribble(
#'   ~voter_id, ~species, ~food, ~ranking,
#'   1, "Rabbit", "Apple", 1,
#'   1, "Rabbit", "Banana", 2,
#'   1, "Rabbit", "Carrot", 3,
#'   2, "Monkey", "Banana", 1,
#'   2, "Monkey", "Apple", 2,
#'   2, "Monkey", "Carrot", 3
#' )
#' # Process preferencial data into a single column.
#' x |>
#'   long_preferences(
#'     food_preference,
#'     id_cols = voter_id,
#'     item_col = food,
#'     rank_col = ranking
#'   )
#' # The same, but keep the species data.
#' x |>
#'   long_preferences(
#'     food_preference,
#'     id_cols = voter_id,
#'     item_col = food,
#'     rank_col = ranking,
#'     unused_col = list(species = dplyr::first)
#'   )
#' @importFrom rlang :=
#' @importFrom vctrs vec_ptype2 vec_cast
NULL

# Format a list of orderings as a "preferences" vctr with vctrs::list_of
.vctr_preferences <- function(orderings, item_names = NULL) {
  if (is.null(item_names)) {
    item_names <- attr(orderings, "item_names")
  }
  vctrs::as_list_of(
    orderings,
    .ptype = matrix(integer(), ncol = 2L)
  ) |>
    vctrs::new_vctr(
      item_names = item_names,
      class = "preferences"
    )
}

#' @rdname preferences
#' @export
long_preferences <- function(data,
                             col,
                             id_cols = NULL,
                             rank_col = NULL,
                             item_col = NULL,
                             item_names = NULL,
                             verbose = TRUE,
                             unused_fn = NULL,
                             na_action = c("drop_rows", "drop_preferences"),
                             ...) {
  col <- rlang::enquo(col)
  id_cols <- rlang::enquo(id_cols)
  item_col <- rlang::enquo(item_col)
  rank_col <- rlang::enquo(rank_col)
  na_action <- match.arg(na_action)

  if (rlang::quo_is_null(id_cols) ||
    rlang::quo_is_null(item_col) ||
    rlang::quo_is_null(rank_col)) {
    stop(
      "When creating \"preferences\" from long-format data, `id_cols`, ",
      "`item_col` and `rank_col` must all specify columns in `data`."
    )
  }
  preferences <- format_long(
    data,
    {{ col }},
    {{ id_cols }},
    {{ item_col }},
    {{ rank_col }},
    item_names,
    verbose,
    unused_fn,
    na_action
  )
  preferences |>
    dplyr::mutate(!!col := .vctr_preferences(!!col))
}

# A helper function to validate ordinal preferences in long format.
validate_long <- function(data,
                          id_cols,
                          rank_col,
                          item_col,
                          item_names,
                          verbose = TRUE) {
  # Show warning if any columns contain NA
  if (anyNA(data) && verbose) {
    message("Found rows containing `NA`. These rows may be ignored.")
  }

  # Extract symbols/expressions
  id_cols_quo <- rlang::enquo(id_cols)
  if (rlang::quo_is_call(id_cols_quo, "c")) {
    symbols <- rlang::call_args(rlang::quo_get_expr(id_cols_quo))
    # Convert to quosures with proper environment
    id_cols_quos <- purrr::map(symbols, rlang::new_quosure, env = rlang::quo_get_env(id_cols_quo))
  } else {
    id_cols_quos <- list(id_cols_quo)
  }

  # Find duplicated items
  if (
    (
      data |>
        dplyr::group_by(!!!id_cols_quos) |>
        dplyr::filter(anyDuplicated({{ item_col }}) != 0L) |>
        nrow() > 0L
    ) |>
      any() &&
      verbose
  ) {
    message(
      "Duplicated rankings per item detected: ",
      "only the highest ranks will be used."
    )
  }

  # Validate items
  if (is.null(item_names)) {
    item_names <- data |>
      dplyr::pull({{ item_col }}) |>
      unique() |>
      stats::na.omit()
  } else {
    # Check if the item column contains numeric values (potential indices)
    is_numeric_items <- is.numeric(data |> dplyr::pull({{ item_col }}))

    if (is_numeric_items) {
      # Check if any index is out of bounds
      max_index <- max(data |> dplyr::pull({{ item_col }}), na.rm = TRUE)
      if (max_index > length(item_names)) {
        stop(
          "`item` index out of bounds. `item_names` does not contain ",
          "enough elements to assign a unique name per item index."
        )
      }
      # For numeric items, we'll handle the conversion to item_names later
      # so we don't need additional validation here
    } else {
      # For non-numeric items, validate that all items exist in item_names
      unknown_items <- data |>
        dplyr::pull({{ item_col }}) |>
        unique() |>
        stats::na.omit() |>
        setdiff(item_names)

      if (length(unknown_items) > 0L) {
        stop(
          "Found item not in `item_names`. `item_names` parameter must ",
          "be a superset of items in preferences."
        )
      }
    }
  }

  # Validate rank
  orig_rank <- data |>
    dplyr::pull({{ rank_col }}) |>
    stats::na.omit()
  int_rank <- as.integer(orig_rank)
  if (anyNA(int_rank) || any(int_rank != orig_rank)) {
    stop("`rank` must be integer-valued.")
  }
}

# A helper function to convert long format data into an orderings format.
format_long <- function(data,
                        col,
                        id_cols,
                        item_col,
                        rank_col,
                        item_names = NULL,
                        verbose = TRUE,
                        unused_fn = NULL,
                        na_action = c("drop_rows", "drop_preferences")) {
  # Validate the data adequately describes preferences
  validate_long(
    data,
    {{ id_cols }},
    {{ rank_col }},
    {{ item_col }},
    item_names,
    verbose
  )

  # Replace item names if `item` column is numeric
  if (
    !is.null(item_names) &&
      data |>
        dplyr::pull({{ item_col }}) |>
        is.numeric()
  ) {
    data <- data |>
      dplyr::mutate(dplyr::across({{ item_col }}, ~ item_names[.x]))
  } else if (is.null(item_names)) {
    # Otherwise extract item_names from `item` column if `item_names` is null
    item_names <- data |>
      dplyr::pull({{ item_col }}) |>
      unique() |>
      stats::na.omit()
  }

  # Convert rank to integers
  data <- data |>
    dplyr::mutate(dplyr::across({{ rank_col }}, as.integer))

  # Extract symbols/expressions for id_cols
  id_cols_quo <- rlang::enquo(id_cols)
  if (rlang::quo_is_call(id_cols_quo, "c")) {
    symbols <- rlang::call_args(rlang::quo_get_expr(id_cols_quo))
    id_cols_quos <- purrr::map(symbols, rlang::new_quosure, env = rlang::quo_get_env(id_cols_quo))
  } else {
    id_cols_quos <- list(id_cols_quo)
  }

  # Handle NA values according to na_action parameter
  if (na_action == "drop_rows") {
    data <- data |> tidyr::drop_na()
  } else if (na_action == "drop_preferences") {
    .has_na <- NULL # NSE workaround
    na_groups <- data |>
      dplyr::group_by(!!!id_cols_quos) |>
      dplyr::summarise(
        .has_na = anyNA({{ item_col }}) || anyNA({{ rank_col }}),
        .groups = "drop"
      ) |>
      dplyr::filter(.has_na)

    if (nrow(na_groups) > 0L) {
      data <- data |>
        dplyr::anti_join(na_groups |> dplyr::select(-.has_na),
          by = gsub("~", "", as.character(id_cols_quos))
        )
    }
  }

  # Base R alternative to group_by + pivot_wider to speed things up
  col_name <- rlang::as_name(rlang::enquo(col))
  item_col_name <- rlang::as_name(rlang::enquo(item_col))
  rank_col_name <- rlang::as_name(rlang::enquo(rank_col))

  # Get id column names
  if (length(id_cols_quos) == 1L) {
    id_col_names <- rlang::as_name(id_cols_quos[[1L]])
  } else {
    id_col_names <- sapply(id_cols_quos, rlang::as_name)
  }

  # Extract vectors for maximum performance
  items <- data[[item_col_name]]
  ranks <- data[[rank_col_name]]
  unused_data <- data[, !names(data) %in% c(item_col_name, rank_col_name, id_col_names)]

  # Create group identifiers using base R
  # To support multiple ID columns, use interaction()
  group_data <- data[id_col_names]
  group_ids <- interaction(group_data, drop = TRUE, lex.order = TRUE)
  group_list <- split(seq_along(group_ids), group_ids)

  # Create item indices
  item_indices <- match(items, item_names)

  n_groups <- length(group_list)

  # Pre-allocate result list
  preferences_list <- vector("list", n_groups)

  # Process each group with optimized base R operations
  for (i in seq_len(n_groups)) {
    idx <- group_list[[i]]
    group_items <- item_indices[idx]
    group_ranks <- ranks[idx]

    # Handle duplicates using aggregate (faster than dplyr for small groups)
    if (anyDuplicated(group_items) > 0L) {
      # Use aggregate for deduplication - much faster than dplyr
      agg_result <- aggregate(
        group_ranks,
        by = list(item = group_items),
        FUN = min,
        na.rm = TRUE
      )
      group_items <- agg_result$item
      group_ranks <- agg_result$x
    }

    # Create dense ranks using base R (avoid expensive dplyr::dense_rank)
    unique_ranks <- sort(unique(group_ranks))
    dense_ranks <- match(group_ranks, unique_ranks)

    # Create ordering matrix
    sort_order <- order(dense_ranks)
    preferences_list[[i]] <- cbind(
      group_items[sort_order],
      dense_ranks[sort_order]
    )
  }

  # Create result data frame efficiently
  # Extract unique combinations from original data using group indices
  group_idx <- match(levels(group_ids), group_ids)
  result_df <- data[group_idx, id_col_names, drop = FALSE]

  # Add preferences column
  result_df[[col_name]] <- preferences_list

  # Convert to tibble if original was tibble
  if (inherits(data, "tbl_df")) {
    result_df <- tibble::as_tibble(result_df)
  }

  # Set item_names attribute
  attr(result_df[[col_name]], "item_names") <- item_names

  return(result_df)
}

#' @rdname preferences
#' @export
wide_preferences <- function(data,
                             col = NULL,
                             ranking_cols = NULL,
                             verbose = TRUE,
                             na_action = c("keep_as_partial", "drop_preferences"),
                             ...) {
  col <- rlang::enquo(col)
  ranking_cols <- rlang::enquo(ranking_cols)
  na_action <- match.arg(na_action)

  if (rlang::quo_is_null(ranking_cols)) {
    stop(
      "When creating \"preferences\" from wide-format data, `ranking_cols`, ",
      "must specify the columns in `data` which represent rankings."
    )
  }

  formatted <- format_wide(data, {{ ranking_cols }}, verbose, na_action, ...)

  if (rlang::quo_is_null(col)) {
    col_name <- "preferences"
  } else {
    col_name <- rlang::as_name(col)
  }

  # Convert to preferences vector with the proper class
  formatted |>
    dplyr::mutate(preferences = .vctr_preferences(preferences)) |>
    dplyr::rename(!!col_name := preferences)
}

validate_wide <- function(data,
                          cols,
                          na_action,
                          verbose = TRUE) {
  # Show warning if any columns contain NA
  if (anyNA(data) && verbose) {
    if (na_action == "drop_preferences") {
      message("Found rows containing `NA`. Any such preferences will be dropped.")
    } else if (na_action == "keep_as_partial") {
      message("Found rows containing `NA`. May result in incomplete preferences.")
    }
  }

  # Get the ranking columns
  ranking_cols <- data |>
    dplyr::select({{ cols }})

  # Check if any ranking values are non-integer
  for (col_name in colnames(ranking_cols)) {
    orig_rank <- data[[col_name]]
    int_rank <- as.integer(orig_rank)
    if (anyNA(int_rank) && !anyNA(orig_rank) || any(int_rank != orig_rank, na.rm = TRUE)) {
      stop("`rank` must be integer-valued.")
    }
  }

  # Check for inconsistent rankings (same rank for different items)
  for (i in seq_len(nrow(data))) {
    row_data <- ranking_cols[i, ]
    if (verbose && anyDuplicated(stats::na.omit(as.numeric(row_data)))) {
      message(
        "Duplicate ranks detected in row ", i,
        ": ties will be preserved in the preference ordering."
      )
    }
  }
}

format_wide <- function(data,
                        cols,
                        verbose = TRUE,
                        na_action = c("keep_as_partial", "drop_preferences"),
                        ...) {
  # Validate the data adequately describes preferences
  validate_wide(
    data,
    {{ cols }},
    na_action,
    verbose
  )

  # Extract item names from column names
  item_names <- data |>
    dplyr::select({{ cols }}) |>
    colnames()

  # Handle NA values according to na_action parameter
  if (na_action == "drop_preferences") {
    # Identify rows with any NA in the ranking columns
    ranking_data <- data |> dplyr::select({{ cols }})
    rows_with_na <- apply(ranking_data, 1L, anyNA)

    # Remove rows with any NA in the ranking columns
    if (any(rows_with_na)) {
      if (verbose) {
        message("Removing ", sum(rows_with_na), " row(s) containing NA values in ranking columns.")
      }
      # Actually filter out the rows with NAs
      data <- data[!rows_with_na, ]
    }
  }
  # For "keep_as_partial", we'll handle NAs in the ordering function below
  # by setting na.last = NA in order(), which will exclude NA items

  # Create a new column with orderings
  data$preferences <- data |>
    dplyr::select({{ cols }}) |>
    apply(
      1L,
      function(x) {
        # NAs are handled here: with na.last = NA, NAs are excluded from the result
        o <- order(x, na.last = NA)
        cbind(o, x[o])
      },
      simplify = FALSE
    )

  # Remove the original ranking columns
  result <- data |> dplyr::select(-{{ cols }})

  # Add item_names as an attribute
  attr(result$preferences, "item_names") <- item_names

  return(result)
}

#' @method Ops preferences
#' @export
Ops.preferences <- function(e1, e2) {
  op <- .Generic[[1L]] # nolint: object_usage_linter
  switch(op,
    `==` = {
      if (all(levels(e1) == levels(e2))) {
        vctrs::vec_equal(e1, e2)
      } else {
        rep(FALSE, max(length(e1), length(e2)))
      }
    },
    `!=` = {
      return(!e1 == e2)
    },
    stop("Undefined operation for \"preferences\".")
  )
}

#' pref_type
#'
#' Ordinal preferences can order every item, or they can order a subset. Some
#' ordinal preference datasets will contain ties between items at a given rank.
#' Hence, there are four distinct types of preferential data:
#' \describe{
#' \item{`soc`}{Strict Orders - Complete List}
#' \item{`soi`}{Strict Orders - Incomplete List}
#' \item{`toc`}{Orders with Ties - Complete List}
#' \item{`toi`}{Orders with Ties - Incomplete List}
#' }
#'
#' @param x A `preferences` object (or vector data representing preferences)
#' @param n_items The number of items, needed to assess whether a selection is
#' complete or not. Defaults to `nlevels(x)` if `x` has class `preferences`,
#' otherwise defaults to the length of the longest preference.
#' @export
pref_type <- function(x, n_items = NULL) {
  if (is.null(n_items)) {
    if (inherits(x, "preferences")) {
      n_items <- nlevels(x)
    } else {
      n_items <- max(vapply(vctrs::vec_data(x), function(p) max(p[, 1L]), integer(1L)))
    }
  }
  has_tie <- function(pref) as.logical(anyDuplicated(pref[, 2L]))
  ties <- any(vapply(vctrs::vec_data(x), has_tie, logical(1L)))
  is_complete <- function(pref) nrow(pref) == n_items
  complete <- all(vapply(vctrs::vec_data(x), is_complete, logical(1L)))
  if (complete) {
    if (ties) {
      "toc"
    } else {
      "soc"
    }
  } else {
    if (ties) {
      "toi"
    } else {
      "soi"
    }
  }
}

#' @method print preferences
#' @export
print.preferences <- function(x, ...) {
  if (length(x) == 0L) {
    cat("preferences(0)\n")
  } else {
    print.default(format(x, ...), quote = FALSE)
  }
}

#' @method format preferences
#' @rdname preferences
#' @param x A vector of preferences.
#' @export
format.preferences <- function(x, ...) {
  item_names <- levels(x)
  fmt_order <- function(pref) {
    pref[, 1L] <- item_names[pref[, 1L]]
    paste0(
      "[",
      split(pref[, 1L], pref[, 2L]) |>
        sapply(FUN = paste0, collapse = " = ") |>
        paste0(collapse = " > "),
      "]"
    )
  }
  vapply(vctrs::vec_data(x), fmt_order, character(1L))
}

#' @rdname preferences
#' @param strings A character vector of preference strings
#' @param sep Character separating the items in the string (default: ">")
#' @param equality Character representing equality between items (default: "=")
#' @param descending If TRUE, parse as descending order preferences.
#' @export
as_preferences <- function(strings, sep = ">", equality = "=", descending = TRUE) {
  if (length(strings) == 0L) {
    return(.vctr_preferences(list(), character(0L)))
  }

  validate_preferences(strings, sep, equality)

  # Get all unique items across all preference strings
  items <- character(0L)
  parsed_prefs <- vector("list", length(strings))

  for (i in seq_along(strings)) {
    if (is.na(strings[i]) || strings[i] == "") {
      parsed_prefs[[i]] <- list()
      next
    }

    # Split by rank using the separator
    ranks <- strsplit(strings[i], sep, fixed = TRUE)[[1L]]
    ranks <- trimws(ranks)

    # Process each rank group, handling equalities
    ranked_items <- vector("list", length(ranks))
    for (j in seq_along(ranks)) {
      # Split items at the same rank by equality character
      equal_items <- strsplit(ranks[j], equality, fixed = TRUE)[[1L]]
      equal_items <- trimws(equal_items)
      ranked_items[[j]] <- equal_items

      # Add to the overall item set
      items <- union(items, equal_items)
    }

    # Store the parsed preference
    parsed_prefs[[i]] <- ranked_items
  }

  # Create preference orderings
  orderings <- lapply(parsed_prefs, function(pref) {
    if (length(pref) == 0L) {
      return(matrix(integer(), ncol = 2L))
    }

    # Create a mapping of items to their position and rank
    item_positions <- integer(0L)
    item_ranks <- integer(0L)

    for (rank in seq_along(pref)) {
      items_at_rank <- pref[[rank]]
      for (item in items_at_rank) {
        item_positions <- c(item_positions, match(item, items))
        item_ranks <- c(item_ranks, rank)
      }
    }

    # For descending=FALSE, reverse the ranks
    if (!descending) {
      max_rank <- max(item_ranks)
      item_ranks <- max_rank - item_ranks + 1L
    }

    # Sort by rank
    order_idx <- order(item_ranks)
    result <- cbind(item_positions[order_idx], item_ranks[order_idx])
    return(result)
  })

  # Create the preferences object
  .vctr_preferences(orderings, items)
}

# Validation helper
validate_preferences <- function(x, sep, equality) {
  # Check for proper format (A>B>C pattern)
  pattern <- paste0("^[A-Za-z0-9_ .,]+([", sep, equality, "][A-Za-z0-9_ .,]+)*$")
  invalid <- !grepl(pattern, x) & !is.na(x) & x != ""

  if (any(invalid)) {
    stop(
      "Invalid preference format. Expected format like 'A", sep, "B", equality, "C', got: ",
      toString(x[invalid])
    )
  }

  invisible(x)
}

#' @rdname preferences
#' @export
preferences <- function(strings = character(0L), sep = ">", equality = "=", descending = TRUE) {
  if (length(strings) == 0L) {
    return(.vctr_preferences(list(), character(0L)))
  }
  as_preferences(strings, sep, equality, descending)
}

#' @method levels preferences
#' @rdname preferences
#' @param x A vector of preferences.
#' @export
levels.preferences <- function(x, ...) {
  attr(x, "item_names")
}

#' @method "levels<-" preferences
#' @export
`levels<-.preferences` <- function(x, value) {
  if (
    anyNA(value) ||
      !identical(unique(value), value) ||
      length(value) != nlevels(x)
  ) {
    warning(
      "No action taken: item names must be unique and with length ",
      "equal to the total number of items."
    )
    return(x)
  }
  attr(x, "item_names") <- value
  x
}

.validate_preferences_frequencies <- function(x,
                                              preferences_col = NULL,
                                              frequency_col = NULL) {
  # Resolve R CMD Check note
  preferences <- frequency <- NULL
  if (is.null(x)) {
    stop(
      "Expected `x` to be of class 'preferences' or a 'tibble' with a ",
      "'preferences' column."
    )
  }
  if (inherits(x, "preferences")) {
    # Convert vector preferences into a tibble with columns `preferences`
    # and `frequency`.
    x <- tibble::tibble(preferences = x) |>
      dplyr::group_by(preferences) |>
      dplyr::summarise(frequency = dplyr::n()) |>
      dplyr::arrange(-frequency)
  } else if (inherits(x, "tbl_df")) {
    # Process tibble.
    # If `preferences_col` is passed, select the appropriate column. Otherwise
    # just look for a preferences-typed column.

    # Get preferences column
    preferences_col <- rlang::enquo(preferences_col)
    if (rlang::quo_is_null(preferences_col)) {
      preferences_col <- rlang::expr(dplyr::where(~ inherits(.x, "preferences")))
    }
    # Get frequency column
    frequency_col <- rlang::enquo(frequency_col)
    if (rlang::quo_is_null(frequency_col)) {
      x <- x |>
        dplyr::select(preferences = !!preferences_col) |>
        dplyr::mutate(frequency = 1L)
    } else {
      x <- x |>
        dplyr::select(
          preferences = !!preferences_col,
          frequency = !!frequency_col
        )
    }
    # Ensure result has one column of "preferences" data.
    preferences_colnames <- x |>
      sapply(FUN = inherits, what = "preferences") |>
      which() |>
      names()
    if (length(preferences_colnames) == 0L) {
      stop(
        "Expected one column of \"preferences\" for ",
        "`write_preflib`, but got 0."
      )
    } else if (length(preferences_colnames) > 1L) {
      warning(
        "Expected one column of \"preferences\" for `write_preflib`, ",
        "but got ", length(preferences_colnames), ". Using `",
        preferences_colnames[1L], "`."
      )
    }
    # Ensure result has one column of "numeric" data.
    if (!rlang::quo_is_null(frequency_col)) {
      numeric_colnames <- x |>
        dplyr::select(frequency) |>
        sapply(FUN = is.numeric) |>
        which() |>
        names()
      if (length(numeric_colnames) > 1L) {
        warning(
          "Expected only one column of frequency for `write_preflib`. ",
          "Using `", numeric_colnames[1L], "`."
        )
      }
    }

    x <- x |>
      dplyr::group_by(preferences) |>
      dplyr::summarise(frequency = sum(frequency)) |>
      dplyr::arrange(-frequency)
  }
  return(x)
}

# Helper function to reindex preferences to match new item names
reindex_preferences <- function(prefs, old_items, new_items) {
  lapply(
    vctrs::vec_data(prefs),
    function(pref_matrix) {
      if (nrow(pref_matrix) == 0L) {
        return(pref_matrix)
      }
      # Map old indices to new indices
      old_item_names <- old_items[pref_matrix[, 1L]]
      new_indices <- match(old_item_names, new_items)

      # Return reindexed matrix
      cbind(new_indices, pref_matrix[, 2L])
    }
  )
}

#' @export
vec_ptype2.preferences.preferences <- function(x, y, ..., x_arg = "", y_arg = "") {
  # Get item names from both vectors
  x_items <- levels(x)
  y_items <- levels(y)

  # Combine item names by taking the union
  combined_items <- union(x_items, y_items)

  # Create an empty preferences vector with the combined item names
  .vctr_preferences(list(), combined_items)
}

#' @export
vec_cast.preferences.preferences <- function(x, to, ..., x_arg = "", to_arg = "") {
  # Get item names from both vectors
  x_items <- levels(x)
  to_items <- levels(to)

  # If item names are identical, no conversion needed
  if (identical(x_items, to_items)) {
    return(x)
  }

  # Check if all items in x are present in to
  missing_items <- setdiff(x_items, to_items)
  if (length(missing_items) > 0L) {
    vctrs::stop_incompatible_cast(
      x, to,
      x_arg = x_arg, to_arg = to_arg,
      details = paste0(
        "Can't cast preferences with items ",
        toString(missing_items),
        " to preferences that don't include these items."
      )
    )
  }

  # Reindex preferences to match target item names
  reindexed_orderings <- reindex_preferences(x, x_items, to_items)

  # Create new preferences vector with target item names
  .vctr_preferences(reindexed_orderings, to_items)
}
