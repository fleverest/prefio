#' Get the name of the first preference for each selection.
#' @export
pref_first <- function(x) {
  nms <- levels(x)
  vapply(
    x,
    \(pref) nms[pref[which(pref[, 2L] == 1L), 1L]],
    character(1L)
  )
}

#' Get the ranks assigned to a specific item in a set of preferences.
#' @export
pref_get_rank <- function(x, item_name) {
  idx <- match(item_name, levels(x))[1L]
  vapply(
    x,
    \(pref) pref[pref[, 1L] == idx, 2L][0L:1L],
    integer(1L)
  )
}

#' Complete preferences by adding unselected items as last place occurrances.
#' @export
pref_complete <- function(x) {
  if (pref_type(x) %in% c("soi", "toi")) {
    n_items <- length(levels(x))
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
          pref[pref[, 2L] <= max_rank, ] |>
            list()
        } else {
          pref[pref[, 2L] >= max(pref[, 2L]) - max_rank + 1L, ] |>
            densify_col2() |>
            list()
        }
      },
      list(integer())
    ) |>
    vctr_preferences(item_names = levels(x))
}

#' Eliminate items from preference selections.
#' @export 
pref_elim <- function(x, lowest = TRUE) {
  x |>
    vapply(
      \(pref) {
        if (lowest) {
          pref[pref[, 2L] != max(pref[, 2L]), ] |>
            list()
        } else {
          pref[pref[, 2L] != 1L, ] |>
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