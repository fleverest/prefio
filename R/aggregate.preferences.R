#' Aggregate Preferences
#'
#' Aggregate `preferences`, returning an `aggregated_preferences` object of
#' the unique preferences and their frequencies. The frequencies can be
#' accessed via the function `frequencies()`.
#'
#' @param x A [`preferences`][preferences] object for `aggregate()`; an
#' object that can be coerced to an `aggregated_preferences` object for
#' `as.aggregated_preferences()`, otherwise an `aggregated_preferences`
#' object.
#' @param frequencies A vector of frequencies for preferences that have been
#' previously aggregated.
#' @param i indices specifying preferences to extract, as for `\link{[}`.
#' @param j indices specifying items to extract, as for `\link{[}`.
#' @param drop if `TRUE` return single row/column matrices as a vector.
#' @param as.aggregated_preferences if `TRUE` create an
#' `aggregated_preferences` object from the indexed preferences Otherwise
#' index the underlying matrix of ranks and return in a data frame with the
#' corresponding frequencies.
#' @param ... Additional arguments, currently unused.
#' @return A data frame of class `aggregated_preferences`, with columns:
#' \describe{
#' \item{preferences}{A [`preferences`][preferences] object of the unique
#' preferences}
#' \item{frequencies}{The corresponding frequencies.}
#' }
#' Methods are available for [`rbind()`] and [`as.matrix()`].
#' @seealso [preflib()] for an object that can be coerced to an
#' `aggregated_preferences` object.
#' @examples
#' # create a preferences object with duplicated preferences
#' R <- matrix(c(1, 2, 0, 0,
#'               0, 1, 2, 3,
#'               2, 1, 1, 0,
#'               1, 2, 0, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- c("apple", "banana", "orange", "pear")
#' R <- as.preferences(R)
#'
#' # aggregate the preferences
#' A <- aggregate(R)
#'
#' # subsetting applies to the preferences, e.g. first two unique preferences
#' A[1:2]
#'
#' # (partial) preferences projected to items 2-4 only
#' A[, 2:4]
#'
#' # convert to a matrix
#' as.matrix(A)
#' @name aggregate
NULL

#' @method aggregate preferences
#' @rdname aggregate
#' @export
aggregate.preferences <- function(x, frequencies = NULL, ...) {
    if (getRversion() < "3.6.0") {
        r <- lapply(seq_len(nrow(x)), function(i) x[i, ])
    } else {
      r <- asplit(unclass(x), 1L)
    }
    dup <- duplicated(r)
    if (any(dup)) {
        r_new <- r[!dup]
        r_id <- match(r, r_new)
        if (!is.null(frequencies)) {
            frequencies <- as.vector(rowsum(frequencies, r_id))
        } else {
          frequencies <- tabulate(r_id)
        }
        x <- do.call("rbind", r_new)
        rownames(x) <- NULL
        res <- data.frame(preferences = as.preferences(x, format = "ranking"),
                          frequencies = frequencies)
        colnames(res) <- c("preferences", "frequencies")
    } else {
        if (is.null(frequencies)) frequencies <- rep.int(1, length(r))
        res <- data.frame(preferences = x,
                          frequencies = frequencies)
        colnames(res) <- c("preferences", "frequencies")
    }
    structure(res, class = c("aggregated_preferences", class(res)))
}

#' @export
aggregate.aggregated_preferences <- function(x, ...) {
    aggregate(x$preferences, x$frequencies)
}

#' @rdname aggregate
#' @export
as.aggregated_preferences <- function(x, ...) {
    UseMethod("as.aggregated_preferences")
}

#' @method as.aggregated_preferences preferences
#' @export
as.aggregated_preferences.preferences <- function(x, ...) {
  return(aggregate(x, ...))
}

#' @rdname aggregate
#' @method [ aggregated_preferences
#' @export
"[.aggregated_preferences" <- function(x, i, j, ..., drop = FALSE,
                                    as.aggregated_preferences = TRUE) {
    preferences <-  x$preferences[i, j, ..., drop = drop,
                          as.preferences = as.aggregated_preferences]
    if (as.aggregated_preferences) {
        aggregate(preferences, frequencies = x$frequencies[i])
    } else {
        if (!is.null(dim(i))) i <- i[, 1]
        data.frame(preferences = preferences, frequencies = x$frequencies[i])
        colnames(res) <- c("preferences", "frequencies")
    }
}

#' @rdname aggregate
#' @export
frequencies <- function(x) {
    if (is.list(x)) {
      x[["frequencies"]]
    } else {
      NULL
    }
}

#' @method as.matrix aggregated_preferences
#' @export
as.matrix.aggregated_preferences <- function(x, ...) {
    res <- cbind(x$preferences, frequencies = x$frequencies)
    rownames(res) <- NULL
    res
}

#' @method rbind aggregated_preferences
#' @export
rbind.aggregated_preferences <- function(...) {
    x <- list(...)
    aggregate(do.call("rbind", lapply(x, `[[`, "preferences")),
              unlist(lapply(x, `[[`, "frequencies")))
}
