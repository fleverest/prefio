#' Preferences Object
#'
#' Create a \code{"preferences"} object from Ordinal Preference data in either
#' \code{ordering} or \code{ranking} format.
#'
#' Ordinal preferences can order every alternative, or they can order a subset.
#' Some preferential datasets will contain ties between alternatives at a given
#' rank. Hence, there are four distinct types of preferential data:
#' \describe{
#' \item{\code{"soc"}}{Strict Orders - Complete List}
#' \item{\code{"soi"}}{Strict Orders - Incomplete List}
#' \item{\code{"toc"}}{Orders with Ties - Complete List}
#' \item{\code{"toi"}}{Orders with Ties - Incomplete List}
#' }
#' The data type is stored alongside the \code{"preferences"} as an attribute
#' \code{attr(preferences, "DATA TYPE")}. To convert between types, there are
#' four methods for doing so; namely \code{\link{as.soc}}, \code{\link{as.soi}},
#' \code{\link{as.toc}} and \code{\link{as.toi}}. Canonically, all strict orders
#' can be converted into orders with ties, and all complete orderings can be
#' converted into incomplete orderings. Incomplete orders can also be converted
#' to complete orders with ties, where all unlisted alternatives appear tied
#' in last preference.
#'
#' A set of preferences can be represented either by \code{ranking} or by
#' \code{ordering}. These correspond to the two ways you can list a set of
#' preferences in a vector:
#' \describe{
#' \item{\code{ordering}}{The alternatives are listed in order of most preferred
#'                          to least preferred, allowing for pairs of
#'                          alternatives in the case of ties.}
#' \item{\code{ranking}}{The rank assigned to each alternative is listed in
#'                        order of some predefined index on the alternatives.
#'                        rankings should be dense, otherwise they will be
#'                        converted to dense rankings.}
#' }
#' When reading \code{"preferences"} from an \code{ordering} matrix, the index
#' on the alternatives is the order passed to the \code{alternative_names}
#' parameter. When reading from a \code{rankings} matrix, if no
#' \code{alternative_names} are provided, the order is inferred from the named
#' columns.
#'
#' A \code{"preferences"} object can also be read from a long-format matrix,
#' where there are three columns: \code{id}, \code{alternative} and
#' \code{rank}. The \code{id} variable groups the rows of the matrix which
#' correspond to a single set of preferences, which the \code{alternative:rank},
#' pairs indicate how each alternative is ranked. When reading a matrix from
#' this format and no \code{alternative_names} parameter is passed, the
#' order is determined automatically.
#'
#' @param data A \code{data.frame} or \code{matrix} in one of three formats:
#' \describe{
#' \item{"ordering"}{Orderings must be a \code{data.frame} with
#'                  \code{"list"}-valued columns. Each row represents an
#'                  ordering of the alternatives from first to last,
#'                  representing ties by a list of vectors corresponding to
#'                  the alternatives.}
#' \item{"ranking"}{Each row assigns a ranking to each alternative, each column
#'                  gives the rankings assigned to a single alterntive. Rankings
#'                  should be dense, otherwise they will be converted to dense
#'                  rankings.}
#' \item{"long"}{Three columns: an \code{id} column grouping the rows which
#'                correspond to a single set of preferences, a
#'                \code{alternative} column specifying (either by index or by
#'                name) the alternative each row refers to, and a \code{rank}
#'                column specifying the rank for the associated
#'                \code{alternative}.}
#' }
#' @param format The format of the data: one of
#' \code{c("ordering", "ranking", "long")}. By default, \code{data} is assumed
#' to be in \code{ordering} format.
#' @param id For \code{data} in long-format: the column representing the
#' preference set grouping.
#' @param alternative For \code{data} in long-format: the column representing
#' the alternatives by name or by index, in which case the
#' \code{alternative_names} parameter should also be passed, or the alternatives
#' will be named by the integers.
#' @param rank For \code{data} in long-format: the column representing the
#' rank for the associated alternative.
#' @param alternative_names The names of the full set of alternatives. When
#' loading data using integer-valued indices in place of alternative names,
#' the \code{alternative_names} character vector should be in the correct order.
#' @param aggregate If \code{TRUE}, aggregate the preferences via
#' \code{\link{aggregate.preferences}} before returning. This returns a
#' \code{\link{aggregated_preferences}} object.
#' @param frequency An optional integer vector containing the number of
#' occurences of each preference. If provided, the method will return a
#' \code{\link{aggregated_preferences}} object with the corresponding
#' frequencies.
#'
#' @return By default, a \code{"preferences"} object, which is a
#' \code{data.frame} with \code{"list"}-valued columns corresponding to
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
#'               4, 1, 2, 3,
#'               2, 1, 1, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(rnk) <- c("apple", "banana", "orange", "pear")
#'
#' rnk <- as.preferences(rnk, format = "ranking")
#'
#' # Convert an existing data.frame of orderings to a preferences object.
#' ord <- preferences(data.frame(
#'   rank1 = I(list()),
#'   rank2 = I(list()),
#'   rank3 = I(list()),
#'   rank4 = I(list())
#' ))
#' ord[1,] <- list("apple", "banana")
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
#' # Truncate preferences to at most 2 ranks
#' ord[, -c(3:4)]
#'
#' # Exclude pear from the rankings
#' ord[, -4, by.rank = FALSE]
#'
#' # Get the highest-ranked alternatives from the third preference-set
#' ord[3, 1]
#'
#' # Get the rank of apple in the third preference-set
#' ord[3, 1, by.rank = FALSE]
#'
#' # Get all the ranks assiged to apple as a vector
#' ord[, 1, by.rank = FALSE]
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
                        aggregate = FALSE) {
  # First we reformat the data into a matrix of rankings.
  if (format == "long") {
    x <- data[, c(id, alternative, rank)]
    if (ncol(x) != 3L) {
      stop(paste("When using long format, 'id', 'alternative' and 'rank'",
                 "must all specify names of columns in 'data'."))
    }
    colnames(x) <- c("id", "alternative", "rank")

    # Show warning if any columns contain NA
    if (any(is.na(x))) {
      warning("Dropping rows containing `NA`.")
    }
    # Filter rows containing NA
    x <- x %>%  dplyr::filter(!is.na(rank) & !is.na(alternative))

    # Validate alternatives
    if (missing(alternative_names)) {
      alternative_names <- sort(unique(x$alternative))
    }
    if (is.character(x$alternative)) {
      if (length(setdiff(x$alternative, alternative_names)) > 0L) {
        stop(paste("Cannot coerce into `preferences`:",
                   "found `alternative` not in `alternative_names`."))
      }
    } else if (is.numeric(x$alternative)) {
        if (any(x$alternative > length(alternative_names))) {
          stop(paste("Cannot coerce into `preferences`:",
                     "`alternative` index out of bounds."))
        }
        x$alternative <- alternative_names[x$alternative]
    }
    x$alternative <- factor(x$alternative)
    levels(x$alternative) <- alternative_names

    # Validate rank
    old_rank <- x$rank
    x$rank <- as.integer(x$rank)
    if (any(is.na(x$rank)) || any(x$rank != old_rank)) {
      stop("Cannot coerce into `preferences`: `rank` must be integer-valued.")
    }

    # Convert into ranking matrix
    ranking <- x %>%
    # Filter duplicate rankings, keeping the highest ranking for an alternative.
      dplyr::group_by(id, alternative) %>%
      remove_duplicates() %>%
    # Convert rankings to dense rankings
      dplyr::ungroup(alternative) %>%
      dplyr::mutate(rank = as_dense(rank)) %>%
    # Convert long-format rankings to wide rankings
      tidyr::spread("alternative", "rank", drop = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(alternative_names) %>%
      as.matrix
    return(ranking)
  } else if (format == "ordering") {
    x <- data
    # Ensure the object is a data.frame with "list" columns
    if (!is.data.frame(data) || !all(unlist(lapply(data, typeof)) == "list")) {
      stop(paste("`data` in \"ordering\" format must be a `data.frame` with",
                 "`list`-valued columns."))
    }
    # Ensure alternatives are either always character or always integer-valued
    alternative_types <- unique(rapply(x, typeof))
    if (length(alternative_types) != 1L ||
        !alternative_types %in% c("character", "numeric")) {
      stop(paste("Alternatives must be listed by name everywhere",
                 "or by index everywhere."))
    }
    if (missing(alternative_names)) {
      alternative_names <- sort(unique(unlist(x)))
    }
    # Convert the ordering data into a matrix of rankings.
    ranking <- t(apply(
      x,
      1,
      function(x) {
        structure(
          rep(seq_along(x),
              sapply(x, length))[match(alternative_names, unlist(x))],
          names = alternative_names
        )
      }
    ))
    return(as.preferences.matrix(ranking))
  } else if (format == "ranking") {
    return(as.preferences.matrix(data))
  } else {
    stop(paste("Format '", format, "' not implemented. `format` must be one",
                "of 'ordering', 'ranking' or 'long'."))
  }
}

# A helper to detect duplicate
remove_duplicates <- function(x) {
  counts_gt1 <- dplyr::count(x)$n > 1
  if (any(counts_gt1)) {
    # TODO: show id:alternative pairs which contain duplicate entries.
    # via `x[counts_gt1, c("id", "alternative")]`
    warning(paste("Found duplicate rankings, only highest rank for each",
                  "alternative will be considered."))
  }
  return(dplyr::top_n(x, 1, -abs(rank)))
}

# An alternative to `dplyr::dense_rank` which raises a warning.
as_dense <- function(rank) {
  drank <- dplyr::dense_rank(rank)
  if (!all(drank == rank)) {
    warning("Converted ranking to dense ranking.")
  }
  drank
}

rankings_to_ordering_list <- function(ranking) {
  apply(ranking,
        1,
        function(ro) {
          sapply(1:max(ranking[!is.na(ranking)]),
                 \(x) names(ro)[which(ro == x)])
        }
  )
}


#' @rdname preferences
#' @method [ preferences
#' @param i The index of the preference-set to access.
#' @param j The rank index to access, or if \code{by.rank = TRUE} the
#' index or name of the alternative to obtain the ranking for.
#' @param by.rank When \code{FALSE}, columns \eqn{j} contain the alternatives
#' in rank \eqn{j}. By default, columns \eqn{j} represent the rank for the
#' in rank \eqn{j}.
#' @param drop If \code{TRUE}, return single row/column matrices as a vector.
#' @param width The width in number of characters to format each preference,
#' truncating by "..." when they are too long.
#' @export
"[.preferences" <- function(x, i, j, ..., drop = TRUE, by.rank = FALSE) {
  stop("Not implemented")
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
                                   frequency = NULL,
                                   aggregate = FALSE) {
  preferences(x,
              format = format,
              id = id,
              alternative = alternative,
              rank = rank,
              alternative_names = alternative_names,
              frequency = frequency,
              aggregate = aggregate)
}

#' @rdname preferences
#' @export
as.preferences.matrix <- function(x,
                                  format,
                                  frequency = NULL,
                                  aggregate = FALSE) {
  if (mode(x) != "numeric" && format == "ranking") {
      stop("Values should be numeric ranks for 'ranking' format.")
  }

}

#' @method length preferences
#' @export
length.preferences <- function(x) {
  nrow(x)
}

#' @method is.na preferences
#' @export
is.na.preferences <- function(x) {
  # Alternatives are only listed once
  apply(x,
        1L,
        function(x) {
          y <- unlist(x)
          identical(unique(y), unname(y))
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
    } else {
      NA
    }
  }
  value <- apply(x, 1L, f, alternatives = attr(x, "alternatives"))
  nc <- nchar(value)
  trunc <- !is.na(nc) & nc > width
  value[trunc] <- paste(strtrim(value[trunc], width - 4), "...")
  value
}

#' @method rbind preferences
#' @export
rbind.preferences <- function(...) {
  stop("Not implemented")
}

#' @method as.data.frame preferences
#' @export
as.data.frame.preferences <- function(x, ...) {
  stop("Not implemented")
}

#' @method as.matrix preferences
#' @export
as.matrix.preferences <- function(x, ...) {
  unclass(x)
}
