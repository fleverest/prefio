#' Check if a preference is blank.
#' @export
pref_isblank <- function(x) {
  vapply(
    x,
    \(pref) dim(pref)[1L] == 0L,
    logical(1L)
  )
}

#' Check the length (number of rankings) of a preference.
#' @export
pref_length <- function(x) {
  vapply(
    x,
    \(pref) dim(pref)[1L],
    integer(1L)
  )
}

#' Get the name of the first preference for each selection.
#' @export
pref_first <- function(x) {
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
#' @export
pref_get_rank <- function(x, item_name) {
  idx <- match(item_name, levels(x))[1L]
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

#' Get the items assigned to a specific rank.
#' @export
pref_get_items <- function(x, rank) {
  vapply(
    x,
    \(pref) list(rownames(pref)[pref[which(pref[, 2L] == rank), 2L]]),
    list(1L)
  )
}

#' Complete preferences by adding unselected items as last place occurrances.
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
#' @export
pref_trunc <- function(x, max_rank = 1L, top_ranks = TRUE) {
  x |>
    vapply(
      \(pref) {
        if (top_ranks) {
          pref[pref[, 2L] <= max_rank, , drop = FALSE] |>
            list()
        } else {
          pref[pref[, 2L] >= max(pref[, 2L]) - max_rank + 1L, , drop = FALSE] |>
            densify_col2() |>
            list()
        }
      },
      list(integer())
    ) |>
    vctr_preferences(item_names = levels(x))
}

#' Remove specified items from preferences.
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
            densify_col2() |>
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
#' @export
pref_elim <- function(x, lowest = TRUE, drop = TRUE) {
  x |>
    vapply(
      \(pref) {
        if (pref_length(pref) == 0L) {
          pref
        } else if (lowest) {
          pref[pref[, 2L] != max(pref[, 2L]), , drop = FALSE] |>
            list()
        } else {
          pref[pref[, 2L] != 1L, , drop = FALSE] |>
            densify_col2() |>
            list()
        }
      },
      list(integer())
    ) |>
    vctr_preferences(item_names = levels(x))
}


# Helper function to make second column a dense ranking.
densify_col2 <- function(x) {
  x[, 2L] <- dplyr::dense_rank(x[, 2L])
  x
}
