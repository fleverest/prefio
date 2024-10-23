#' @title Preferences Objects
#' @name preferences
#'
#' @description
#' TODO
#'
#' @param data A `list`, `matrix` or `tibble` in one of three formats:
#' \describe{
#' \item{`ordering`}{`list` elements represent a preference, with its' values
#'                        the item(s) (multiple in the case of a tie) assigned
#'                        to a rank by order of first to last preference, so
#'                        with no repetitions. If elements are `integer`s, then
#'                        the `item_names` parameter should be set, otherwise
#'                        the integers themselves will be treated as names.}
#' \item{`ranking`}{Columns are associated with a single item, with values
#'                        representing the rank assigned to the item for each
#'                        preference. Conventionally, ranks are integers in
#'                        increasing order (with larger values indicating lower
#'                        preference), but they can be any ordinal values. Any
#'                        rankings will be converted to 'dense' integer
#'                        rankings, as implemented by `dplyr::dense_rank()`.}
#' \item{`long`}{Like `ranking` but in a long format: three columns (`id`,
#'                        `item`, `rank`) are used to infer the `ranking`
#'                        structure.}
#' }
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
#' @param aggregate If `TRUE`, aggregates identical preference selections and
#' counts repetitions. When enabled, `new_preferences` will return a `tibble`.
#' @param ... Unused.
#'
#' @return A `preferences` object, or a `tibble` with
#' columns `preferences` and `frequency` when `aggregate=TRUE`.
#'
#' @examples
#' # create rankings from data in long format
#'
#' # Example long-form data
#' x <- data.frame(
#'   id = c(rep(1:4, each = 4), 5, 5, 5),
#'   item = c(
#'     LETTERS[c(1:3, 3, 1:4, 2:5, 1:2, 1)], NA,
#'     LETTERS[3:5]
#'   ),
#'   rank = c(4:1, rep(NA, 4), 3:4, NA, NA, 1, 3, 4, 2, 2, 2, 3)
#' )
#'
#' # * Set #1 has two different ranks for the same item (item C
#' # has rank 1 and 2). This item will be excluded from the preferences.
#' # * All ranks are missing in set #2, a technically valid partial ordering
#' # * Some ranks are missing in set #3, a perfectly valid partial ordering
#' # * Set #4 has inconsistent ranks for two items, and a rank with a
#' # missing item.
#' # * Set #5 is not a dense ranking. It will be converted to be dense and then
#' # inferred to be a regular partial ordering with ties.
#' split(x, x$rank)
#'
#' # Creating a preferences object with this data will attempt to resolve these
#' # issues automatically, sending warnings when assumptions need to be made.
#' new_preferences(x, id = "id", item = "item", rank = "rank")
#'
#' # Convert an existing matrix of rankings to a preferences object.
#' rnk <- matrix(c(
#'   1, 2, 0, 0,
#'   4, 1, 2, 3,
#'   2, 1, 1, 1,
#'   1, 2, 3, 0,
#'   2, 1, 1, 0,
#'   1, 0, 3, 2
#' ), nrow = 6, byrow = TRUE)
#' colnames(rnk) <- c("apple", "banana", "orange", "pear")
#'
#' rnk <- as_preferences(rnk, format = "ranking")
#'
#' # Convert an existing data frame of orderings to a preferences object.
#' e <- character() # short-hand for empty ranks
#' ord <- new_preferences(
#'   as.data.frame(
#'     rbind(
#'       list(1, 2, e, e), # apple, banana
#'       list("banana", "orange", "pear", "apple"),
#'       list(c("banana", "orange", "pear"), "apple", e, e),
#'       list("apple", "banana", "orange", e),
#'       list(c("banana", "orange"), "apple", e, e),
#'       list("apple", "pear", "orange", e)
#'     )
#'   ),
#'   format = "ordering",
#'   item_names = c("apple", "banana", "orange", "pear")
#' )
#'
#' # Access the first three sets of preferences
#' ord[1:3]
#'
#' # Truncate preferences to the top 2 ranks
#' ord[, 1:2, by_rank = TRUE]
#'
#' # Exclude pear from the rankings
#' ord[, -4]
#'
#' # Convert the preferences to a ranking matrix
#' as.matrix(ord)
#'
#' # Get the rank of apple in the third preference-set
#' as.matrix(ord)[3, 1]
#'
#' # Get all the ranks assigned to apple as a vector
#' as.matrix(ord)[, "apple"]
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
    aggregate,
    verbose,
    unused_fn
  )
  preferences |>
    mutate(across({{ col }}, vctr_preferences))
}

#' @rdname preferences
#' @export
new_preferences <- function(data,
                            col,
                            format = c("long", "ordering", "ranking"),
                            id_cols = NULL,
                            rank_col = NULL,
                            item_col = NULL,
                            frequency_col = NULL,
                            item_names = NULL,
                            aggregate = FALSE,
                            verbose = TRUE,
                            unused_fn = NULL,
                            ...) {
  fmt <- try(match.arg(format), silent = TRUE)
  id_cols <- rlang::enquo(id_cols)
  item_col <- rlang::enquo(item_col)
  rank_col <- rlang::enquo(rank_col)
  frequency_col <- rlang::enquo(frequency_col)
  # First we reformat the data into a matrix of rankings.
  if (inherits(fmt, "try-error")) {
    stop(
      "Format '", format, "' not implemented: Must be one ",
      "of 'ordering', 'ranking' or 'long'."
    )
  } else if (fmt == "long") {
    if (rlang::quo_is_null(id_cols) ||
      rlang::quo_is_null(item_col) ||
      rlang::quo_is_null(rank_col)) {
      stop(
        "When creating \"preferences\" from long-format data, `id_cols`, ",
        "`item_col` and `rank_col` must all specify columns in `data`."
      )
    }
    if (!rlang::quo_is_null(frequency_col)) {
      warning(
        "When creating \"preferences\" from long-format data, ",
        "`frequency_col` parameter is ignored."
      )
      frequency <- NULL
    }
    if (!is.null(unused_fn) && aggregate) {
      warning(
        "When creating \"preferences\" from long-format data, ",
        "cannot specify both `unused_fn` and `aggregate`. Ignoring `unused_fn`."
      )
      unused_fn <- NULL
    }
    ordering <- format_long(
      data,
      col,
      {{ id_cols }},
      {{ item_col }},
      {{ rank_col }},
      item_names,
      aggregate,
      verbose,
      unused_fn
    )
    return(ordering)
  } else if (fmt == "ranking") {
    ordering <- ranking_to_ordering(
      data
    )
  } else if (fmt == "ordering") {
    ordering <- validate_ordering(data, item_names, verbose)
  }
  # Obtain item_names from validation step if necessary
  if (is.null(item_names)) {
    item_names <- attr(ordering, "item_names")
  }
  x <- vctrs::as_list_of(
    ordering,
    .ptype = matrix(integer(), ncol = 2L)
  )
  x <- vctrs::new_vctr(
    x,
    item_names = item_names,
    class = "preferences"
  )
  if (aggregate) {
    if (is.null(frequency)) {
      warning("Did not aggregate preferences. Setting every `frequency` to 1.")
      frequency <- 1L
    }
    return(tibble(preferences = x, frequency = frequency) |> dplyr::arrange(-frequency))
  } else {
    x
  }
}

# A helper function to validate ordinal preferences in ordering format and clean them up.
validate_ordering <- function(x,
                              item_names,
                              verbose = TRUE) {
  # Ensure the object is formatted correctly as a list of lists
  if (!is.list(x) && !all(sapply(x, is.list))) {
    stop(
      "`data` in \"ordering\" format must be a `list` of `list`s"
    )
  }

  # Check there is no mixed types
  item_types <- unique(rapply(x, typeof))
  if (length(item_types) > 1L ||
    !any(c("character", "integer") %in% item_types)) {
    stop(
      "`data` in \"ordering\" format must be either all `character` vectors or ",
      "all `integer` vectors."
    )
  }

  # If items are `character` vectors, extract the item_names by factoring
  if (item_types == "character") {
    # Infer unique item_names if not given
    if (is.null(item_names)) {
      item_names <- x |>
        unlist() |>
        unique()
    } else {
      observed_items <- x |>
        unlist() |>
        unique()
      # When item_names is passed, make sure it's a superset of the items in `data`.
      if (!all(observed_items %in% item_names)) {
        stop("Expected all observed items to occur in `item_names`, but found missing values.")
      }
    }
    # Convert `character` items to index in `item_names`
    x <- lapply(
      x,
      lapply,
      FUN = match,
      table = item_names
    )
  }

  # If items are "integer", then warn user if `item_names` is not passed.
  if (item_types == "integer" && is.null(item_names)) {
    if (verbose) {
      warning("`item_names` missing on \"integer\"-valued items.")
    }
    item_names <- x |>
      unlist() |>
      unique() |>
      as.character()
  }

  # Convert list-of-lists format to list of integer-valued matrices
  x <- lapply(
    x,
    \(pref) {
      cbind(
        unlist(pref),
        rep(seq_along(pref),
          times = lengths(pref)
        )
      )
    }
  )

  # Return validated, cleaned data along with item_names
  structure(x, item_names = item_names)
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
                        aggregate = TRUE,
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
      vctrs::vec_equal(e1, e2)
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

#' @importFrom vctrs vec_ptype_abbr
#' @export
vctrs::vec_ptype_abbr

#' @method vec_ptype_abbr toi_preferences
#' @export
vec_ptype_abbr.toi_preferences <- function(x, ...) {
  "toi_pref"
}

#' @method vec_ptype_abbr toc_preferences
#' @export
vec_ptype_abbr.toc_preferences <- function(x, ...) {
  "toc_pref"
}

#' @method vec_ptype_abbr soi_preferences
#' @export
vec_ptype_abbr.soi_preferences <- function(x, ...) {
  "soi_pref"
}

#' @method vec_ptype_abbr soc_preferences
#' @export
vec_ptype_abbr.soc_preferences <- function(x, ...) {
  "soc_pref"
}

#' @method levels preferences
#' @export
levels.preferences <- function(x, ...) {
  attr(x, "item_names")
}

#' @method "levels<-" preferences
#' @export
`levels<-.preferences` <- function(x, value) {
  if (anyNA(value) ||
    !identical(unique(value), value) ||
    length(value) != length(names(x))) {
    warning(
      "No action taken: item names must be unique and with length ",
      "equal to the total number of items."
    )
    return(x)
  }
  attr(x, "item_names") <- value
  x
}
