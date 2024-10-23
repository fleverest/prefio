#' Check if a preference is blank.
#' @param x A vector of preferences.
#' @return A logical vector indicating which preferences are blank, i.e., `[]`.
#' @export
pref_blank <- function(x) {
  vapply(
    x,
    \(pref) dim(pref)[1L] == 0L,
    logical(1L)
  )
}

#' Check the length (number of rankings) of a preference.
#' @param x A vector of preferences.
#' @return The number of items listed on each of the preferences.
#' @export
pref_length <- function(x) {
  vapply(
    x,
    \(pref) dim(pref)[1L],
    integer(1L)
  )
}

#' Get the name of each first preference in a vector of preferences.
#' @param x A vector of preferences.
#' @return The name(s) of the item(s) assigned first preference.
#' @export
first_pref <- function(x) {
  nms <- levels(x)
  vapply(
    x,
    \(pref) {
      res <- nms[pref[which(pref[, 2L] == 1L), 1L]]
      if (length(res) == 0) {
        NA
      } else {
        res
      }
    },
    character(1L)
  )
}

#' Get the rank assigned to a specific item in a set of preferences.
#' @param x A vector of preferences.
#' @param item_name The name of the item to extract the rank for.
#' @return The rank of `item_name` for each of the preferences in `x`.
#' @export
get_rank <- function(x, item_name) {
  idx <- match(item_name, levels(x))[1L]
  if (length(idx) != 1L) {
    stop("`item_name` must name exactly one item in the preferences.")
  }
  vapply(
    x,
    \(pref) {
      res <- pref[pref[, 1L] == idx, 2L]
      if (length(res) == 0L) {
        NA
      } else {
        res
      }
    },
    integer(1L)
  )
}

#' Get the name of the item(s) assigned a specific rank, e.g., first.
#' @param x A vector of preferences.
#' @param rank A single integer, the rank which you would like to inspect.
#' @return A list containing the name(s) of the item(s) ranked `rank` in each of
#' the preferences in `x`.
#' @export
get_items <- function(x, rank) {
  vapply(
    x,
    \(pref) list(rownames(pref)[pref[which(pref[, 2L] == rank), 2L]]),
    list(1L)
  )
}

#' Complete preferences by adding unselected items as last place occurrances.
#' @param x A vector of preferences.
#' @return A new vector of preferences, with each selection starting with the
#' corresponding selections made in `x`, but with all unranked items placed
#' last.
#' @export
pref_complete <- function(x) {
  if (pref_type(x) %in% c("soi", "toi")) {
    n_items <- nlevels(x)
    preferences <- x |>
      vapply(
        \(pref) {
          if (nrow(pref) < n_items) {
            missing_items <- setdiff(seq(n_items), pref[, 1L])
            max_rank <- max(pref[, 2L])
            list(rbind(pref, cbind(missing_items, max_rank + 1L)))
          } else {
            list(pref)
          }
        },
        list(integer())
      ) |>
      vctr_preferences(item_names = levels(x))
  } else {
    x
  }
}

#' Truncate preferences to a maximum number of ranks.
#' @param x A vector of preferences.
#' @param n_ranks The number of rankings to include in the output.
#' @param top_ranks If `TRUE`, output the top `n_ranks`, otherwise the bottom
#' `n_ranks`.
#' @return A vector of preferences with each selection equal to `x` but omitting
#' the top (if `top_ranks` is `TRUE`, otherwise bottom) selections so that the
#' new vector has at most `n_ranks` selections.
#' @export
pref_trunc <- function(x, n_ranks = 1L, top_ranks = TRUE) {
  x |>
    vapply(
      \(pref) {
        if (top_ranks) {
          pref[pref[, 2L] <= max_rank, , drop = FALSE] |>
            list()
        } else {
          pref[pref[, 2L] >= max(pref[, 2L]) - max_rank + 1L, , drop = FALSE] |>
            .densify_col2() |>
            list()
        }
      },
      list(integer())
    ) |>
    vctr_preferences(item_names = levels(x))
}

#' Remove specified items from preferences.
#' @param x A vector of preferences.
#' @param items The names of the items which should be removed from the preferences in `x`.
#' @return A new vector of preferences, but with `items` removed from each selection.
#' @export
pref_remove <- function(x, items) {
  idxs <- match(items, levels(x))
  x |>
    vapply(
      \(pref) {
        rows <- na.omit(match(idxs, pref[, 1L]))
        if (length(rows) >= nrow(pref)) {
          pref[integer(0L), , drop = FALSE] |>
            list()
        } else if (length(rows) > 0L) {
          pref[-rows, , drop = FALSE] |>
            .densify_col2() |>
            list()
        } else {
          list(pref)
        }
      },
      list(integer())
    ) |>
    vctr_preferences(item_names = levels(x))
}

#' Eliminate items from preference selections.
#' @param x A vector of preferences.
#' @param lowest If `TRUE`, eliminates the lowest ranked items from each
#' selection.
#' @param drop If `TRUE`, drops all blank preferences.
#' @return A new vector of preferences which is equal to `x` but with the least
#' preferred selection dropped for each selection.
#' @export
pref_elim <- function(x, lowest = TRUE, drop = FALSE) {
  result <- x |>
    vapply(
      \(pref) {
        if (pref_length(pref) == 0L) {
          pref
        } else if (lowest) {
          pref[pref[, 2L] != max(pref[, 2L]), , drop = FALSE] |>
            list()
        } else {
          pref[pref[, 2L] != 1L, , drop = FALSE] |>
            .densify_col2() |>
            list()
        }
      },
      list(integer())
    ) |>
    vctr_preferences(item_names = levels(x))
  if (drop) {
    result[!pref_blank(result)]
  } else {
    result
  }
}


# Helper function to make second column a dense ranking. Not exported.
.densify_col2 <- function(x) {
  x[, 2L] <- dplyr::dense_rank(x[, 2L])
  x
}
