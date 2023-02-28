#' Group Preferences
#'
#' Create an object of class `grouped_preferences` which associates a
#' group index with an object of class `preferences`. This allows the
#' preferences to be linked to covariates with group-specific values.
#'
#' @param index A numeric vector of length equal to the number of preferences
#' specifying the subject for each set.
#' @param x A [`preferences`][preferences] object for `group()`; an object
#' that can be coerced to a `grouped_preferences` object with
#' `as.grouped_preferences()`; otherwise a `grouped_preferences` object.
#' @param i Indices specifying groups to extract, may be any data type accepted
#' by `\link{[}`.
#' @param j Indices specifying items to extract, as for `\link{[}`.
#' @param as.grouped_preferences If `TRUE` return a `grouped_preferences`
#' object, otherwise return a matrix/vector.
#' @param max The maximum number of preferences to format per subject.
#' @param width The maximum width in number of characters to format the
#' preferences.
#' @param ... Additional arguments passed on to `\link{as.preferences}`
#' by `grouped_preferences` or `as.grouped_preferences`; unused by
#' `format`.
#' @return An object of class `grouped_preferences`, which is a vector of
#' of group IDs with the following attributes:
#' \item{preferences}{ The `preferences` object.}
#' \item{index}{ An index matching each preference set to each group ID.}
#' @examples
#'
#' # ungrouped preferences (5 preference sets, 4 items)
#' R <- as.preferences(matrix(c(1, 2, 0, 0,
#'                              0, 2, 1, 0,
#'                              0, 0, 1, 2,
#'                              2, 1, 0, 0,
#'                              0, 1, 2, 3), ncol = 4, byrow = TRUE))
#' length(R)
#' R
#'
#' # group preferences (first three in group 1, next two in group 2)
#' G <- group(R, c(1, 1, 1, 2, 2))
#' length(G)
#'
#' ## by default up to 2 preference sets are shown per group, "..." indicates if
#' ## there are further preferences
#' G
#' print(G, max = 1)
#'
#' ## select preferences from group 1
#' G[1,]
#'
#' ## exclude item 3 from preferences
#' G[, -3]
#'
#' ## preferences from group 2, excluding item 3
#' ## - note group 2 becomes the first group
#' G[2, -3]
#' @name group
#' @export
NULL

#' @rdname group
#' @export
group <- function(x, ...) {
  UseMethod("group")
}

#' @rdname group
#' @method group preferences
#' @export
group.preferences <- function(x, index, ...) {
  if (!(is.vector(index) && length(index) == nrow(x))) {
    stop("index must be a vector with length equal to preferences")
  }
  index <- as.numeric(index)
  do.call("structure",
          c(list(seq_len(max(index)), preferences = x, index = index),
            list(class = "grouped_preferences")))
}

#' @rdname group
#' @method [ grouped_preferences
#' @export
"[.grouped_preferences" <- function(x, i, j, ..., drop = TRUE) {
  if (!missing(i)) {
    if (missing(j)) {
      j <- TRUE
    }
    # always a vector if picking out elements of preferences matrix
    if (is.matrix(i)) {
        r <- split(seq_along(attr(x, "index")), attr(x, "index"))
        i1 <- unlist(r[i[, 1L]])
        i2 <- rep(i[, 2L], lengths(r))
        return(.subset(attr(x, "preferences"), cbind(i1, i2)))
    }
    # convert index of groups to index of preferences
    g <- .subset(x, i)
    # create index for preferences matrix
    i <- which(attr(x, "index") %in% g)
    groups <- split(i, attr(x, "index")[i])[as.character(g)]
    i <- unlist(groups)
    # update value and index to remove omitted groups
    value <- seq_along(groups)
    index <- rep(value, lengths(groups))
  } else {
    if (missing(j)) {
      return(x)
    }
    value <- x
    i <- TRUE
    index <- attr(x, "index")
  }
  # now subset preferences matrix
  preferences <- attr(x, "preferences")[i, j]
  # convert preferences matrix to grouped_preferences
  group(as.preferences(preferences), index)
}



#' @method as.data.frame grouped_preferences
#' @export
as.data.frame.grouped_preferences <-
  function(x, row.names = NULL, optional = FALSE, ...,
           nm = paste(deparse(substitute(x), width.cutoff = 20L),
                      collapse = " ")) {
  value <- list(x)
  if (!optional) {
    names(value) <- nm
  }
  if (is.null(row.names) && !is.null(rownames(x))) {
    row.names <- rownames(x)
  }
  if (is.null(row.names)) {
    row.names <- .set_row_names(length(x))
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

#' @method print grouped_preferences
#' @export
print.grouped_preferences <- function(x, max = 2L, width = 20L, ...) {
  print.default(format(x, max = max, width = width, ...))
}

#' @rdname group
#' @method format grouped_preferences
#' @export
format.grouped_preferences <- function(x, max = 2L, width = 20L, ...) {
  tab <- tabulate(attr(x, "index"))
  rep <- numeric(length(attr(x, "index")))
  rep[order(attr(x, "index"))] <- sequence(tab)
  if (ncol(attr(x, "preferences")) > 0) {
    j <- TRUE
  } else {
    j <- NULL
  }
  R <- attr(x, "preferences")[rep <= max, j]
  char <- format.preferences(R, width = width)
  value <- vapply(split(char, attr(x, "index")[rep <= max]),
                  function(x) {
                    if (all(is.na(x))) return(NA_character_)
                    paste(x, collapse = ", ")
                  },
                  "a")
  # add ... if more than max preferences
  trunc <- tab > max & !is.na(value)
  value[trunc] <- paste0(value[trunc], ", ...")
  value
}

#' @method na.omit grouped_preferences
#' @importFrom stats na.omit
na.omit.grouped_preferences <- function(object, ...) {
  omit <- seq_along(
    attr(
      object,
      "preferences"
    )
  )[is.na(attr(object, "preferences"))]
  if (length(omit) == 0L) {
    return(object)
  }
  nm <- names(object)
  index <- attr(object, "index")[-omit]
  index <- match(index, unique(index))
  names(omit) <- nm[omit]
  attr(omit, "class") <- "omit"
  structure(unique(index),
            preferences = attr(object, "preferences")[-omit, , drop = FALSE],
            index = index,
            na.action = omit,
            class = "grouped_preferences")
}

#' @method na.exclude grouped_preferences
#' @importFrom stats na.exclude
na.exclude.grouped_preferences <- function(object, ...) {
  out  <- na.omit(object)
  class(attr(out, "na.action")) <- "na.exclude"
  out
}

#' @method is.na grouped_preferences
#' @export
is.na.grouped_preferences <- function(x) {
  out <- tapply(attr(x, "preferences"), attr(x, "index"), sum) == 0
  names(out) <- names(x)
  out
}
