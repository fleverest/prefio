#' Preferences Object
#'
#' Create a `"preferences"` object from Ordinal Preference data in either
#' `ordering` or `ranking` format.
#'
#' Ordinal preferences can order every alternative, or they can order a subset.
#' Some preferential datasets will contain ties between alternatives at a given
#' rank. Hence, there are four distinct types of preferential data:
#' \describe{
#' \item{`"soc"`}{Strict Orders - Complete List}
#' \item{`"soi"`}{Strict Orders - Incomplete List}
#' \item{`"toc"`}{Orders with Ties - Complete List}
#' \item{`"toi"`}{Orders with Ties - Incomplete List}
#' }
#' The data type is stored alongside the `"preferences"` as an attribute
#' `attr(preferences, "DATA TYPE")`. To convert between types, there are
#' four methods for doing so; namely `\link{as.soc}`, `\link{as.soi}`,
#' `\link{as.toc}` and `\link{as.toi}`. Canonically, all strict orders
#' can be converted into orders with ties, and all complete orderings can be
#' converted into incomplete orderings. Incomplete orders can also be converted
#' to complete orders with ties, where all unlisted alternatives appear tied
#' in last preference.
#'
#' A set of preferences can be represented either by `ranking` or by
#' `ordering`. These correspond to the two ways you can list a set of
#' preferences in a vector:
#' \describe{
#' \item{`ordering`}{The alternatives are listed in order of most preferred
#'                          to least preferred, allowing for pairs of
#'                          alternatives in the case of ties.}
#' \item{`ranking`}{The rank assigned to each alternative is listed in
#'                        order of some predefined index on the alternatives.
#'                        rankings should be dense, otherwise they will be
#'                        converted to dense rankings.}
#' }
#' When reading `"preferences"` from an `ordering` matrix, the index
#' on the alternatives is the order passed to the `alternative_names`
#' parameter. When reading from a `rankings` matrix, if no
#' `alternative_names` are provided, the order is inferred from the named
#' columns.
#'
#' A `"preferences"` object can also be read from a long-format matrix,
#' where there are three columns: `id`, `alternative` and
#' `rank`. The `id` variable groups the rows of the matrix which
#' correspond to a single set of preferences, which the \code{alternative:rank},
#' pairs indicate how each alternative is ranked. When reading a matrix from
#' this format and no `alternative_names` parameter is passed, the
#' order is determined automatically.
#'
#' @param data A `data.frame` or `matrix` in one of three formats:
#' \describe{
#' \item{"ordering"}{Orderings must be a `data.frame` with
#'                  `"list"`-valued columns. Each row represents an
#'                  ordering of the alternatives from first to last,
#'                  representing ties by a list of vectors corresponding to
#'                  the alternatives.}
#' \item{"ranking"}{Each row assigns a ranking to each alternative, each column
#'                  gives the rankings assigned to a single alterntive. Rankings
#'                  should be dense, otherwise they will be converted to dense
#'                  rankings.}
#' \item{"long"}{Three columns: an `id` column grouping the rows which
#'                correspond to a single set of preferences, a
#'                `alternative` column specifying (either by index or by
#'                name) the alternative each row refers to, and a `rank`
#'                column specifying the rank for the associated
#'                `alternative`.}
#' }
#' @param format The format of the data: one of "ordering", "ranking", or
#' "long". By default, `data` is assumed to be in `ordering` format.
#' @param id For `data` in long-format: the column representing the
#' preference set grouping.
#' @param alternative For `data` in long-format: the column representing
#' the alternatives by name or by index, in which case the
#' `alternative_names` parameter should also be passed, or the alternatives
#' will be named by the integers.
#' @param rank For `data` in long-format: the column representing the
#' rank for the associated alternative.
#' @param alternative_names The names of the full set of alternatives. When
#' loading data using integer-valued indices in place of alternative names,
#' the `alternative_names` character vector should be in the correct order.
#' @param aggregate If `TRUE`, aggregate the preferences via
#' `\link{aggregate.preferences}` before returning. This returns a
#' `\link{aggregated_preferences}` object.
#' @param frequency An optional integer vector containing the number of
#' occurences of each preference. If provided, the method will return a
#' `\link{aggregated_preferences}` object with the corresponding
#' frequencies.
#'
#' @return By default, a `"preferences"` object, which is a
#' `data.frame` with `"list"`-valued columns corresponding to
#' preferences on the alternatives. This may be an ordering on subsets of the
#' alternatives in the case of ties, or a potentially-partial strict ordering.
#' In the case of partial or tied preferences, some entries may be empty lists.
#'
#' @examples
#' # create rankings from data in long form
#'
#' # Example long-form data
#' x <- data.frame(id = c(rep(1:4, each = 4), 5, 5, 5),
#'                 alternative = c(LETTERS[c(1:3, 3, 1:4, 2:5, 1:2, 1)], NA,
#'                            LETTERS[3:5]),
#'                 rank = c(4:1, rep(NA, 4), 3:4, NA, NA, 1, 3, 4, 2, 2, 2, 3))
#'
#' # * Set #1 has two different ranks for the same alternative (alternative C
#' # has rank 1 and 2). This alternative will be excluded from the preferences.
#' # * All ranks are missing in set #2, a technically valid partial ordering
#' # * Some ranks are missing in set #3, a perfectly valid partial ordering
#' # * Set #4 has inconsistent ranks for two alternatives, and a rank with a
#' # missing alternative.
#' # * Set #5 is not a dense-ranking. It will be converted to be dense and then
#' # inferred to be a regular partial ordering with ties.
#' split(x, x$ranking)
#'
#' # Creating a preferences object with this data will attempt to resolve these
#' # issues automatically, sending warnings when assumptions need to be made.
#' preferences(x, id = "id", alternative = "alternative", rank = "rank")
#'
#' # Convert an existing matrix of rankings to a preferences object.
#' rnk <- matrix(c(1, 2, 0, 0,
#'                 4, 1, 2, 3,
#'                 2, 1, 1, 1,
#'                 1, 2, 3, 0,
#'                 2, 1, 1, 0,
#'                 1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(rnk) <- c("apple", "banana", "orange", "pear")
#'
#' rnk <- as.preferences(rnk, format = "ranking")
#'
#' # Convert an existing data.frame of orderings to a preferences object.
#' ord <- preferences(
#'   data.frame(
#'     rank1 = I(list()),
#'     rank2 = I(list()),
#'     rank3 = I(list()),
#'     rank4 = I(list())
#'   ),
#'   format = "ordering",
#'   alternative_names = c("apple", "banana", "orange", "pear")
#' )
#' ord[1,] <- list(1, 2) # apple, banana
#' ord[2,] <- list("banana", "orange", "pear", "apple")
#' ord[3,] <- list(c("banana", "orange", "pear"), "apple")
#' ord[4,] <- list("apple", "banana", "orange")
#' ord[5,] <- list(c("banana", "orange"), "apple")
#' ord[6,] <- list("apple", "pear", "orange")
#'
#' # Alternatively, we could have referred to each alternative by index and
#' # passed `alternative_names = c("apple", "banana", "orange", "pear")`.
#' ord <- as.preferences(ord)
#'
#' # Access the first three sets of preferences
#' ord[1:3, ]
#'
#' # Truncate preferences to the top 2 ranks
#' ord[, 1:2, by.ordering = TRUE]
#'
#' # Exclude pear from the rankings
#' ord[, -4]
#'
#' # Get the highest-ranked alternatives from the third preference-set
#' ord[3, 1, by.ordering = TRUE]
#'
#' # Get the rank of apple in the third preference-set
#' ord[3, 1]
#'
#' # Get all the ranks assiged to apple as a vector
#' ord[, 1]
#'
#' # Convert the preferences to a ranking matrix
#' as.matrix(ord, format = "ranking")
#'
#' # Convert the preferences to a ordering matrix
#' as.matrix(ord, format = "ordering")
#'
#' # Convert the preferences to a long-format matrix
#' as.matrix(ord, format = "long")
#'
#' @export
preferences <- function(data,
                        format = c("long", "ordering", "ranking"),
                        id = NULL,
                        rank = NULL,
                        alternative = NULL,
                        alternative_names = NULL,
                        frequency = NULL,
                        aggregate = FALSE,
                        verbose = TRUE,
                        ...) {
  format <- match.arg(format, c("long", "ordering", "ranking"))
  # First we reformat the data into a matrix of rankings.
  if (format == "long") {
    if (!missing(frequency)) {
      warning("When creating \"preferences\" from long-format data,",
              "`frequency` parameter is ignored.")
      frequency <- NULL
    }
    ranking <- long_to_ranking(data,
                               id,
                               alternative,
                               rank,
                               NULL,
                               verbose)

    prefs <- as.preferences.matrix(ranking,
                                   format = "ranking",
                                   ..., verbose = verbose)
  } else if (format == "ordering") {
    ranking <- ordering_to_ranking(data, alternative_names, verbose)
    prefs <- as.preferences.matrix(ranking,
                                   format = "ranking",
                                   ..., verbose = verbose)
  } else if (format == "ranking") {
    x <- data
    # Infer alterntive names
    if (missing(alternative_names) && colnames(x)) {
      alternative_names <- colnames(x)
    } else if (missing(alternative_names) && !colnames(x)) {
      message(paste0("Alternative names could not be inferred from ",
                     "ranked data. Defaulting to the integers 1-", dim(x)[2]))
    }
    prefs <- as.preferences.matrix(data,
                                   format = "ranking",
                                   alternative_names = alternative_names,
                                   ..., verbose = verbose)
  } else {
    stop("Format '", format, "' not implemented: Must be one ",
                "of 'ordering', 'ranking' or 'long'.")
  }
  # Aggregate if necessary.
  if (aggregate) {
    return(aggregate(prefs))
  } else {
    return(prefs)
  }
}

# A helper function to validate ordinal preferences in ordering format.
validate_ordering <- function(x,
                              alternative_names,
                              verbose = TRUE) {

  # Ensure the object is a data.frame with "list" columns
  if (!is.data.frame(data) || !all(unlist(lapply(data, typeof)) == "list")) {
    stop("`data` in \"ordering\" format must be a \"data.frame\" with ",
               "\"list\"-valued columns.")
  }

  # Ensure alternatives are either always character or always integer-valued
  alternative_types <- unique(rapply(data, typeof))
  if (length(alternative_types) != 1L ||
      !alternative_types %in% c("character", "numeric")) {
    stop("Alternatives must be listed by name everywhere ",
               "or by index everywhere.")
  }

  # Warn user if alternatives are listed by index without any alternative_names
  if (missing(alternative_names)) {
    alternative_names <- sort(unique(unlist(data)))
    if (is.numeric(alternative_names) && verbose) {
      message("Alternatives listed by index but no `alternative_names` passed ",
              "to method. Using numeric names.")
      alternative_names <- as.character(alternative_names)
    }
  }
}

# A helper function to convert ordering-form data into a rankings matrix.
ordering_to_ranking <- function(data,
                                alternative_names,
                                verbose = TRUE) {

  validate_ordering(data, alternative_names, verbose)

  if (missing(alternative_names)) {
    alternative_names <- as.character(sort(unique(unlist(data))))
  }
  # Convert the ordering data into a matrix of rankings.
  return(t(apply(
    data,
    1,
    function(x) {
      structure(
        rep(seq_along(x),
            sapply(x, length))[match(alternative_names, unlist(x))],
        names = alternative_names
      )
    }
  )))
}

# A helper function to validate ordinal preferences in long format.
validate_long <- function(data,
                          alternative_names,
                          verbose = TRUE) {

  # Raise error if `id`, `alternative` or `rank` are invalid.
  if (ncol(data) != 3L) {
    stop("When using long-format, `id`, `alternative` and `rank` ",
               "must all specify names of columns in `data`.")
  }

  # Show warning if any columns contain NA
  if (anyNA(data)) {
    if (verbose) {
      message("Dropping rows containing `NA`.")
    }
  }

  # Find duplicated items
  if (anyDuplicated(paste(data$id, data$alternative, sep = ":")) && verbose) {
    message("Duplicated rankings per alternative detected: ",
            "only the highest ranks will be used.")
  }

  # Validate alternatives
  if (is.null(alternative_names)) {
    alternative_names <- sort(unique(data$alternative))
  }
  if (is.character(data$alternative)) {
    if (is.null(setdiff(sort(unique(data$alternative)),
                        alternative_names))) {
      stop("Found `alternative` not in `alternative_names`.")
    }
  } else if (is.numeric(data$alternative)) {
      if (any(data$alternative > length(alternative_names))) {
        stop("`alternative` index out of bounds.")
      }
      data$alternative <- alternative_names[data$alternative]
  }

  # Validate rank
  orig_rank <- na.omit(data$rank)
  int_rank <- as.integer(orig_rank)
  if (anyNA(int_rank) || any(int_rank != orig_rank)) {
    stop("`rank` must be integer-valued.")
  }
}

# A helper function to convert long-form data into a rankings matrix.
long_to_ranking <- function(data,
                            id,
                            alternative,
                            rank,
                            alternative_names = NULL,
                            verbose = TRUE) {
  # Take only required columns
  data <- data[, c(id, alternative, rank)]
  colnames(data) <- c("id", "alternative", "rank")

  validate_long(data, alternative_names, verbose)

  data$rank <- as.integer(data$rank)

  # Process alternative_names
  if (is.null(alternative_names)) {
    alternative_names <- as.character(sort(unique(data$alternative)))
  }
  if (is.numeric(data$alternative)) {
    data$alternative <- alternative_names[data$alternative]
  }
  data$alternative <- factor(data$alternative)
  levels(data$alternative) <- alternative_names

  # Return the ranking matrix
  return(data %>%
    # Filter rows containing NA
    dplyr::filter(!is.na(rank) & !is.na(alternative) & !is.na(id)) %>%
    # Filter duplicate rankings, keeping the highest ranking for an alternative.
    dplyr::group_by(id, alternative) %>%
    dplyr::top_n(1, -rank) %>%
    # Convert rankings to dense rankings
    dplyr::ungroup(alternative) %>%
    dplyr::mutate(rank = dplyr::dense_rank(rank)) %>%
    # Convert long-format rankings to wide rankings
    tidyr::spread("alternative", "rank", drop = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(alternative_names) %>%
    as.matrix()
  )
}

# A helper to validate ordinal preferences in ranking format.
validate_ranking <- function(data, alternative_names, verbose) {
  if (!class(data) == "matrix") {

  }
}

# A helper to convert a matrix of rankings into ordering format.
ranking_to_ordering <- function(ranking) {
  max_rank <- max(na.omit(c(as.matrix(ranking))))
  out <- as.data.frame(t(apply(ranking,
        1,
        function(r) {
          sapply(seq_len(max_rank),
                 \(x) names(r)[which(r == x)])
        }
  )))
  colnames(out) <- paste0("Rank", seq_len(max_rank))
  out
}

#' @method identical preferences
#' @export
identical.preferences <- function(x1, x2, ...) {
  # If the alternatives are different return FALSE everywhere
  if (!all(dim(x1) == dim(x2))) {
    return(FALSE)
  }
  if (!identical(sort(attr(x1, "ALTERNATIVE NAMES")),
                 sort(attr(x2, "ALTERNATIVE NAMES")))) {
    return(FALSE)
  }
  # Sort the columns of x2 to be in the same order as x1
  x2 <- x2[, attr(x1, "ALTERNATIVE NAMES")]
  identical(unclass(x1), unclass(x2))
}

#' @method Ops preferences
#' @export
Ops.preferences <- function(x1, x2) {
  op <- .Generic[[1]]
  switch(op,
         `==` = {
           minlen <- min(nrow(x1), nrow(x2))
           maxlen <- max(nrow(x1), nrow(x2))
           cmp <- sapply(seq_len(minlen), function(i) identical(x1[i], x2[i]))
           if (minlen < maxlen) {
             warning("Objects being compared are not the same length.")
             cmp <- c(cmp, rep(FALSE, maxlen - minlen))
           }
           return(cmp)
         },
         `!=` = {
           minlen <- min(nrow(x1), nrow(x2))
           maxlen <- max(nrow(x1), nrow(x2))
           cmp <- sapply(seq_len(minlen), function(i) identical(x1[i], x2[i]))
           if (minlen < maxlen) {
             warning("Objects being compared are not the same length.")
             cmp <- c(cmp, rep(FALSE, maxlen - minlen))
           }
           return(!cmp)
         },
         stop("Undefined operation for \"preferences\"."))

}

#' @rdname preferences
#' @method [ preferences
#' @param i The index of the preference-set to access.
#' @param j The rank index to access, or if `by.ordering = TRUE` the
#' index or name of the alternative to obtain the ranking for.
#' @param by.ordering When `FALSE`, returns a `"preferences"` object:
#' internally rows \eqn{i} contain the ranking assigned to each alternative
#' in preference \eqn{p_i}. When `TRUE`, returns a `"data.frame"` where
#' rows group the the candidates in order of rank.
#' @param drop If `TRUE`, return single row/column matrices as a vector.
#' @param width The width in number of characters to format each preference,
#' truncating by "..." when they are too long.
#' @export
"[.preferences" <- function(x, i, j, ..., drop = FALSE, by.ordering = FALSE) {

  if (!missing(j) && is.numeric(j) && any(j > ncol(x) | j < -ncol(x))) {
    stop("Subscript `j` out of bounds.")
  }
  if (!missing(i) && is.numeric(i) && any(i > nrow(x) | i < -nrow(x))) {
    stop("Subscript `i` out of bounds.")
  }

  # Accessing orderings
  if (by.ordering) {
    if (drop) {
      message("Cannot drop when returning as orderings")
    }
    if (!missing(i)) {
      if (!is.numeric(i)) {
        stop("When accessing preferences as ordering lists, index must ",
             "be numeric.")
      }
      value <- .subset(x, i, TRUE, drop = FALSE)
    } else {
      value <- x
    }
    return(ranking_to_ordering(value)[, j])
  }

  # Accessing rankings
  if (missing(j)) {
    if (missing(i)) {
      value <- unclass(x)
    } else {
      j <- TRUE
      # always a vector if picking out elements of rankings matrix
      if (is.matrix(i)) {
        stop("Cannot subset by matrix.")
      }
      # else subset of rankings
      value <- .subset(x, i, j, drop = FALSE)
    }
  } else {
    # Subset items (never drop)
    if (missing(i)) {
      i <- TRUE
    }
    # Subset and convert to dense rank again.
    value <- do.call(rbind,
                     apply(.subset(x, i, j, drop = FALSE),
                           1L,
                           dplyr::dense_rank,
                           simplify = FALSE))
  }
  alternatives <- attr(x, "ALTERNATIVE NAMES")

  # Sort subset of alternatives so that they appear in the same order as the
  # original preferences' alternatives.
  if (!is.character(j)) {
    sub_alternatives <- alternatives[j]
  } else {
    sub_alternatives <- j
  }
  ord <- order(match(sub_alternatives, alternatives))
  value <- value[, ord, drop = drop]
  sub_alternatives <- sub_alternatives[ord]

  if (is.null(dim(value))) {
    if (length(sub_alternatives) > 1L) {
      names(value) <- sub_alternatives
    }
    value
  } else if (is.character(j)) {
    structure(value,
              dimnames = list(NULL, sub_alternatives),
               "ALTERNATIVE NAMES" = sub_alternatives,
              class = "preferences",
              "DATA TYPE" = preftype(value))
  } else {
    structure(value,
              dimnames = list(NULL, sub_alternatives),
               "ALTERNATIVE NAMES" = sub_alternatives,
              class = "preferences",
              "DATA TYPE" = preftype(value))
  }
}

# A helper function to determine the type of the preference data:
# "soc", "soi", "toc", or "toi".
preftype <- function(prefs) {
  x <- as.matrix(prefs)
  complete <- anyNA(x)
  ties <- FALSE
  for (i in seq_len(nrow(x))) {
    if (anyDuplicated(x[i,])) {
      ties <- TRUE
      break
    }
  }
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


#' @rdname preferences
#' @export
as.preferences <- function(x, ...) {
  UseMethod("as.preferences")
}

#' @rdname preferences
#' @export
as.preferences.default <- function(x,
                                   format,
                                   id = NULL,
                                   alternative = NULL,
                                   rank = NULL,
                                   alternative_names = NULL,
                                   verbose = TRUE) {
  # Convert orderings data.frames into ranking matrices.
  if ("data.frame" %in% class(x) && all(sapply(x, typeof) == "list")) {
    x <- ordering_to_ranking(x, alternative_names, verbose)
  }
  x <- as.matrix(x)
  as.preferences.matrix(x,
                        format,
                        id,
                        alternative,
                        rank,
                        alternative_names,
                        verbose)
}

#' @rdname preferences
#' @export
as.preferences.matrix <- function(x,
                                  format = c("ranking", "long"),
                                  id = NULL,
                                  alternative = NULL,
                                  rank = NULL,
                                  alternative_names = NULL,
                                  verbose = TRUE) {
  format <- match.arg(format, c("long", "ordering", "ranking"))
  # First we reformat the data into a matrix of rankings.
  if (format == "long") {
    prefs <- long_to_ranking(x,
                             id,
                             alternative,
                             rank,
                             alternative_names,
                             verbose)
  } else if (format == "ranking") {
    prefs <- x
  } else {
    stop("Not implemented.")
  }
  if (missing(alternative_names)) {
    alternative_names <- colnames(prefs)
  }
  class(prefs) <- c("preferences", class(prefs))
  attr(prefs, "ALTERNATIVE NAMES") <- alternative_names
  attr(prefs, "DATA TYPE") <- preftype(prefs)
  return(prefs)
}

#' @method length preferences
#' @export
length.preferences <- function(x) {
  nrow(x)
}

#' @method is.na preferences
#' @export
is.na.preferences <- function(x) {
  # Valid dense-rankings
  apply(x,
        1L,
        function(x) {
          x_sub <- na.omit(x)
          all(dplyr::dense_rank(x_sub) == x_sub)
        })
}

#' @method print preferences
#' @export
print.preferences <- function(x, ...) {
  print.default(format(x, ...))
}

#' @method format preferences
#' @rdname preferences
#' @export
format.preferences <- function(x, width = 40L, ...) {
  f <- function(i, alternatives) {
    keep <- !is.na(i) & i != 0L
    obj <- alternatives[keep]
    i <- i[keep]
    ord <- order(i)
    if (length(obj) > 1L) {
      op <- ifelse(diff(i[ord]) == 0L, " = ", " > ")
      paste(obj[ord], c(op, ""), sep = "", collapse = "")
    } else if (length(obj) == 1L) {
      obj
    } else {
      "blank"
    }
  }
  value <- apply(x, 1L, f,  alternatives = attr(x, "ALTERNATIVE NAMES"))
  nc <- nchar(value)
  trunc <- !is.na(nc) & nc > width
  value[trunc] <- paste(strtrim(value[trunc], width - 4), "...")
  value
}

#' @method rbind preferences
#' @export
rbind.preferences <- function(...) {
    # check contain the same items
    preflist <- list(...)
    nm <- lapply(preflist, colnames)
    ref <- nm[[1L]]
    ok <- vapply(nm, identical, TRUE, ref)
    if (any(!ok)) {
        alternative_names <- sort(unique(unlist(nm)))
        preflist <- lapply(
          preflist,
          function(x) {
            preflist <- matrix(0L,
                               nrow = nrow(x),
                               ncol = length(alternative_names),
                               dimnames = list(NULL, alternative_names))
            preflist[, colnames(x)] <- x
            preflist
          }
        )
    }
    # rbind all preferences matrices
    preflist <- do.call("rbind", lapply(preflist, unclass))
    structure(preflist, class = "preferences")
}

#' @method as.data.frame preferences
#' @export
as.data.frame.preferences <- function(x,
                                      row.names = NULL,
                                      optional = FALSE,
                                      ...,
                                      nm = paste(deparse(substitute(x),
                                                         width.cutoff = 20L),
                                                 collapse = " ")) {
  value <- list(x)
  if (!optional) {
    names(value) <- nm
  } else {
    names(value) <- make.names(nm)
  }
  if (is.null(row.names) && !is.null(rownames(x))) {
    row.names <- rownames(x)
  }
  if (is.null(row.names)) {
    row.names <- .set_row_names(nrow(x))
  } else {
    if (is.object(row.names) || !is.integer(row.names)) {
      row.names <- as.character(row.names)
    }
    if (anyNA(row.names)) {
      stop("row names contain missing values")
    }
    if (anyDuplicated(row.names)) {
      stop(paste("duplicate row.names: ",
           paste(unique(row.names[duplicated(row.names)]),
                       collapse = ", ")))
    }
  }
  attr(value, "row.names") <- row.names
  class(value) <- "data.frame"
  value
}

#' @method as.matrix preferences
#' @export
as.matrix.preferences <- function(x, ...) {
  m <- unclass(x)
  colnames(m) <- attr(x, "ALTERNATIVE NAMES")
  attr(m, "ALTERNATIVE NAMES") <- NULL
  return(m)
}

#' @importFrom utils str
#' @export
utils::str

#' @method str preferences
str.preferences <- function(object, ...) {
    str(unclass(object))
}
