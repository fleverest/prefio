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
#' @param id_cols <[`tidy-select`][dplyr_tidy_select]> The columns by which to
#' group the dataset to extract a single preference selection.
#' @param item_col <[`tidy-select`][dplyr_tidy_select]> For `data` in
#' long-format: the column representing the items by name or by index, in which
#' case the `item_names` parameter should also be passed.
#' @param rank_col <[`tidy-select`][dplyr_tidy_select]> For `data` in
#' long-format: the column representing the rank for the associated item.
#' @param frequency_col <[`tidy-select`][dplyr_tidy_select]> Optionally, for
#' `data` in ordering or ranking formats: column representing the frequency of
#' occurance for the associated preference selection.
#' @param item_names The names of the full set of items. This is necessary when
#' the dataset specifies items by index rather than by name, or when there are
#' items which do not appear in any preference selection.
#' @param verbose If `TRUE`, diagnostic messages will be sent to stdout.
#' @param unused_fn When `format="long"`, passes parameter to
#' `dplyr::pivot_wider` to summarise values from unused columns. This can be
#' helpful to keep covariates from columns which are unused in processing
#' preferences, but are important for upstream modelling. See
#' <[`pivot_wider`][pivot_wider]> for more details.
#' @param ... Unused.
#'
#' @examples
#' # Votes cast by two animals ranking a variety of fruits and vegetables.
#' # This is not real data, I made this up.
#' x <- tribble(
#'   ~voter_id, ~species, ~food,   ~ranking,
#'   1,         "Rabbit", "Apple",  1,
#'   1,         "Rabbit", "Banana", 2,
#'   1,         "Rabbit", "Carrot", 3,
#'   2,         "Monkey", "Banana", 1,
#'   2,         "Monkey", "Apple",  2,
#'   2,         "Monkey", "Carrot", 3
#' )
#' # Process preferencial data into a single column.
#' x |>
#'   long_preferences(food_preference,
#'                    id_cols = voter_id,
#'                    item_col = fruit,
#'                    rank_col = ranking)
#' # The same, but keep the species data.
#' x |>
#'   long_preferences(food_preference,
#'                    id_cols = voter_id,
#'                    item_col = fruit,
#'                    rank_col = ranking,
#'                    unused_col = list(species = first))
NULL

# Format a list of orderings as a "preferences" vctr with vctrs::list_of
vctr_preferences <- function(orderings, item_names = NULL) {
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
                             ...) {
  col <- rlang::enquo(col)
  id_cols <- rlang::enquo(id_cols)
  item_col <- rlang::enquo(item_col)
  rank_col <- rlang::enquo(rank_col)

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
    unused_fn
  )
  preferences |>
    mutate(across({{ col }}, vctr_preferences))
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
    message("Dropping rows containing `NA`.")
  }

  # Find duplicated items
  if (
    data |>
      group_by({{ id_cols }}) |>
      summarise(dupes = anyDuplicated({{ item_col }})) |>
      select(dupes) |>
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
      pull({{ item_col }}) |>
      unique()
  } else if (
    data |>
      pull({{ item_col }}) |>
      unique() |>
      na.omit() |>
      setdiff(item_names) |>
      length() > 0L
  ) {
    stop(
      "Found item not in `item_names`. `item_names` parameter must ",
      "be a superset of items in preferences."
    )
  } else if (
    data |> pull({{ item_col }}) |> is.numeric() &&
      any(data |> pull({{ item_col }}) > length(item_names))) {
    stop(
      "`item` index out of bounds. `item_names` does not contain ",
      "enough elements to assign a unique name per item index."
    )
  }

  # Validate rank
  orig_rank <- data |> pull({{ rank_col }})
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
                        unused_fn = NULL) {
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
      dplyr::mutate(across({{ item_col }}, ~ item_names[.x]))
  } else if (is.null(item_names)) {
    # Otherwise extract item_names from `item` column if `item_names` is null
    item_names <- data |>
      dplyr::pull({{ item_col }}) |>
      unique()
  }
  # Convert rank to integers
  data <- data |>
    dplyr::mutate(across({{ rank_col }}, as.integer))

  # Extract rankings
  data <- data |>
    # Filter rows containing NA
    tidyr::drop_na() |>
    # Filter duplicate rankings, keeping the highest ranking for an item.
    dplyr::group_by({{ id_cols }}, {{ item_col }}) |>
    dplyr::top_n(1L, -{{ rank_col }}) |>
    # Convert rankings to dense rankings
    dplyr::ungroup({{ item_col }}) |>
    dplyr::mutate(across({{ rank_col }}, dplyr::dense_rank)) |>
    # Convert long-format rankings to wide rankings
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = {{ id_cols }},
      names_from = {{ item_col }},
      values_from = {{ rank_col }},
      names_expand = TRUE,
      unused_fn = unused_fn
    )

  # Convert rankings into orderings format
  data[[rlang::as_name(col)]] <- data |>
    dplyr::select(item_names) |>
    apply(
      1L,
      function(x) {
        o <- order(x, na.last = NA)
        cbind(o, x[o])
      }
    )
  # Drop rankings
  data <- data |>
    dplyr::select(-item_names)

  # Add frequency as an attribute if necessary
  attr(data[[rlang::as_name(col)]], "item_names") <- item_names
  data
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
pref_type <- function(x, n_items = NULL, long = FALSE) {
  if (is.null(n_items)) {
    n_items <- max(vapply(vctrs::vec_data(x), function(p) max(p[, 1L]), integer(1L)))
  }
  has_tie <- function(pref) as.logical(anyDuplicated(pref[, 2L]))
  ties <- any(vapply(vctrs::vec_data(x), has_tie, logical(1L)))
  is_complete <- function(pref) nrow(pref) == n_items
  complete <- all(vapply(vctrs::vec_data(x), is_complete, logical(1L)))
  if (complete) {
    if (ties) {
      preftype <- "toc"
    } else {
      preftype <- "soc"
    }
  } else {
    if (ties) {
      preftype <- "toi"
    } else {
      preftype <- "soi"
    }
  }
  if (long) {
    paste0(preftype, "_preferences")
  } else {
    preftype
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
#' @export
format.preferences <- function(x, ...) {
  item_names <- levels(x)
  fmt_order <- function(pref) {
    pref[, 1L] <- item_names[pref[, 1L]]
    paste0(
      "[",
      split(pref[, 1], pref[, 2]) |>
        sapply(paste0, collapse = " = ") |>
        paste0(collapse = " > "),
      "]"
    )
  }
  vapply(vctrs::vec_data(x), fmt_order, character(1L))
}

#' @method levels preferences
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
    length(value) != length(levels(x))
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
