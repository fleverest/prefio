#' Group Preferences
#'
#' Create an object of class \code{"grouped_preferences"} which associates a
#' group index with an object of class \code{"preferences"}. This allows the
#' preferences to be linked to covariates with group-specific values as the basis
#' for model-based recursive partitioning, see \code{\link{pltree}}.
#'
#' @param index a numeric vector of length equal to the number of preferences
#' specifying the subject for each ranking.
#' @param x a [`"preferences"`][preferences] object for `group()`; an
#' object that can be coerced to a \code{"grouped_preferences"} object for
#' \code{as.grouped_preferences()}, otherwise a \code{"grouped_preferences"} object.
#' @param i indices specifying groups to extract, may be any data type accepted
#' by \code{\link{[}}.
#' @param j indices specifying items to extract, as for \code{\link{[}}.
#' @param drop if \code{TRUE} return single row/column matrices as a vector.
#' @param as.grouped_preferences if \code{TRUE} return a grouped_preferences object,
#' otherwise return a matrix/vector.
#' @param max the maximum number of preferences to format per subject.
#' @param width the maximum width in number of characters to format each
#' ranking.
#' @param ... additional arguments passed on to \code{\link{as.preferences}}
#' by \code{grouped_preferences} or \code{as.grouped_preferences}; unused by
#' \code{format}.
#' @return An object of class \code{"grouped_preferences"}, which is a vector of
#' of group IDs with the following attributes:
#' \item{preferences}{ The \code{"preferences"} object.}
#' \item{index}{ An index match each ranking to each group ID.}
#' \item{R}{ A matrix with items ordered from last to first place, for each
#' ranking. }
#' \item{S}{ The preferences matrix with the ranks replaced by the size of the
#' chosen set for free choices and zero for forced choices. }
#' \item{id}{ A list with elements of the adjacency matrix that are incremented
#' by each ranking. }
#' @seealso \code{\link{pltree}}
#' @examples
#'
#' # ungrouped preferences (5 preference sets, 4 alternatives)
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
#' ## exclude item 3 from ranking
#' G[, -3]
#'
#' ## preferences from group 2, excluding item 3
#' ## - note group 2 becomes the first group
#' G[2, -3]
#'
#' ## index underlying preferences without creating new grouped_preferences
#' ## object
#' G[2, -3, as.grouped_preferences = FALSE]
#' @export
group <- function(x, index, ...){
    UseMethod("group")
}

#' @method group preferences
#' @export
group.preferences <- function(x, index, ...){
    if (!(is.vector(index) & length(index) == nrow(x)))
        stop("index must be a vector with length equal to preferences")
    index <- as.numeric(index)
    do.call("structure",
            c(list(seq_len(max(index)), preferences = x, index = index),
              ranking_stats(x),
              list(class = "grouped_preferences")))
}


# ranking stats - summaries used in model fitting, compute once for all
ranking_stats <- function(preferences){
    preferences <- unclass(preferences)
    nr <- nrow(preferences)
    nc <- ncol(preferences)
    R <- S <- matrix(0L, nr, nc)
    id <- list()
    for (i in seq_len(nr)){
        x <- preferences[i, ]
        ind <- which(as.logical(x))
        if (length(ind) < 2L) next # no contribution to modelling
        ord <- order(x[ind], decreasing = TRUE)
        j <- seq_along(ind)
        # items ranked from last to 1st place
        R[i, j] <- as.integer(ind[ord])
        # 1 in column s if ranking includes choice from set of size |s|
        x <- x[R[i, j]]
        # size of chosen set at each rank (ignore "choice" of one from one)
        size <- tabulate(x)[x]
        if (size[1L] == 1L) size[1L] <- 0L
        S[i, j] <- size
        # contribution to adjacency matrix
        add <- list()
        for (s in seq_len(x[1L] - 1L)){
            one <- which(preferences[i, ] == s)
            # > gives rest; == s + 1 gives next best
            two <- which(preferences[i, ] > s)
            add[[s]] <- kronecker(one, (two - 1L)*nc, "+")
        }
        id[[i]] <- unlist(add)
    }
    list(R = R, S = S, id = id)
}

#' @rdname group
#' @export
as.grouped_preferences <- function(x, ...){
    UseMethod("as.grouped_preferences")
}

#' @rdname group
#' @method as.grouped_preferences paircomp
#' @export
as.grouped_preferences.paircomp <- function(x, ...){
    if (attr(x, "mscale")[1L] < -1L) {
        warning("strength of preference ignored")
        x <- sign(x)
    }
    id <- which(!is.na(as.matrix(x)), arr.ind = TRUE)
    ncomp <- nrow(id)
    nobj <- length(attr(x, "labels"))
    pairs <- which(upper.tri(diag(nobj)), arr.ind = TRUE)
    preferences <- matrix(0L, nrow = ncomp, ncol = nobj,
                       dimnames = list(NULL, attr(x, "labels")))
    x <- as.matrix(x)[id]
    preferences[cbind(seq_len(ncomp), pairs[,1L][id[,2L]])] <-
        ifelse(x == -1L, 2L, 1L)
    preferences[cbind(seq_len(ncomp), pairs[,2L][id[,2L]])] <-
        ifelse(x == 1L, 2L, 1L)
    preferences <- structure(preferences, class = "preferences")
    do.call("structure",
            c(list(seq_len(max(id[,1L])), preferences = preferences, index = id[,1L]),
              ranking_stats(preferences),
              list(class = "grouped_preferences")))
}

#' @rdname group
#' @method [ grouped_preferences
#' @export
"[.grouped_preferences" <- function(x, i, j, ..., drop = TRUE,
                                 as.grouped_preferences = TRUE) {
    if (!missing(i)) {
        if (missing(j)) j <- TRUE
        # always a vector if picking out elements of preferences matrix
        if (is.matrix(i)) {
            r <- split(seq_along(attr(x, "index")), attr(x, "index"))
            i1 <- unlist(r[i[,1L]])
            i2 <- rep(i[,2L], lengths(r))
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
        if (missing(j)) return(x)
        value <- x
        i <- TRUE
        index <- attr(x, "index")
    }
    # now subset preferences matrix
    preferences <- .subset(attr(x, "preferences"), i, j, drop = FALSE)
    if (!as.grouped_preferences) {
        if (drop) return(drop(preferences))
        return(preferences)
    }
    if (ncol(preferences) == ncol(attr(x, "preferences"))) {
        # subset attributes to match selected preferences
        structure(value,
                  preferences = structure(preferences, class = "preferences"),
                  index = index,
                  R = attr(x, "R")[i, , drop = FALSE],
                  S = attr(x, "S")[i, , drop = FALSE],
                  id = attr(x, "id")[i],
                  class = "grouped_preferences")
    } else {
        # convert preferences matrix to grouped_preferences
        # (will recode as necessary, omit redundant preferences, create R, S, id)
        group(as.preferences(preferences), index)
    }
}



#' @method as.data.frame grouped_preferences
#' @export
as.data.frame.grouped_preferences <-
    function(x, row.names = NULL, optional = FALSE, ...,
             nm = paste(deparse(substitute(x), width.cutoff = 20L),
                        collapse = " ")){
    value <- list(x)
    if (!optional) {
        names(value) <- nm
    }
    if (is.null(row.names) & !is.null(rownames(x))) row.names <- rownames(x)
    if (is.null(row.names)) {
        row.names <- .set_row_names(length(x))
    } else {
        if (is.object(row.names) || !is.integer(row.names))
            row.names <- as.character(row.names)
        if (anyNA(row.names))
            stop("row names contain missing values")
        if (anyDuplicated(row.names))
            stop(paste("duplicate row.names: ",
                       paste(unique(row.names[duplicated(row.names)]),
                             collapse = ", ")))
    }
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

#' @method print grouped_preferences
#' @export
print.grouped_preferences <- function(x, max = 2L, width = 20L, ...){
    print.default(format(x, max = max, width = width, ...))
}

#' @rdname group
#' @method format grouped_preferences
#' @export
format.grouped_preferences <- function(x, max = 2L, width = 20L, ...){
    tab <- tabulate(attr(x, "index"))
    rep <- numeric(length(attr(x, "index")))
    rep[order(attr(x, "index"))] <- sequence(tab)
    R <- attr(x, "preferences")[rep <= max, ]
    char <- format.preferences(R, width = width)
    value <- vapply(split(char, attr(x, "index")[rep <= max]),
                    function(x) {
                        if (all(is.na(x))) return(NA_character_)
                        paste(x, collapse = ", ")
                        }, "a")
    # add ... if more than max preferences
    trunc <- tab > max & !is.na(value)
    value[trunc] <- paste0(value[trunc], ", ...")
    value
}

#' @method na.omit grouped_preferences
#' @importFrom stats na.omit
na.omit.grouped_preferences <- function(object, ...) {
    omit <- seq_along(attr(object, "preferences"))[is.na(attr(object, "preferences"))]
    if (length(omit) == 0L)
        return(object)
    nm <- names(object)
    index <- attr(object, "index")[-omit]
    index <- match(index, unique(index))
    names(omit) <- nm[omit]
    attr(omit, "class") <- "omit"
    structure(unique(index),
              preferences = attr(object, "preferences")[-omit, , drop = FALSE],
              index = index,
              R = attr(object, "R")[-omit, , drop = FALSE],
              S = attr(object, "S")[-omit, , drop = FALSE],
              id = attr(object, "id")[-omit],
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