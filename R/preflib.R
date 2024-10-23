#' Read Ordinal Preference Data From PrefLib
#'
#' Read orderings from `.soc`, `.soi`, `.toc` or `.toi` files storing
#' ordinal preference data format as defined by
#' \href{https://www.preflib.org/}{\{PrefLib\}: A Library for Preferences}
#' into a `preferences` object.
#'
#' Note that PrefLib refers to the items being ordered by "alternatives".
#'
#' The file types supported are
#' \describe{
#' \item{.soc}{Strict Orders - Complete List}
#' \item{.soi}{Strict Orders - Incomplete List}
#' \item{.toc}{Orders with Ties - Complete List}
#' \item{.toi}{Orders with Ties - Incomplete List}
#' }
#'
#' The numerically coded orderings and their frequencies are read into a
#' tibble, storing all original metadata in a "preflib" attribute.
#'
#' A PrefLib file may be corrupt, in the sense that the ordered alternatives do
#' not match their names. In this case, the file will still be read, but with a
#' warning.
#'
#' @return A tibble with two columns: `preferences` and `frequency`. The
#' `preferences` column contains all the preferential orderings in the file, and
#' the `frequency` column the relative frequency of this selection.
#'
#' @param file A preferential data file, conventionally with extension `.soc`,
#' `.soi`, `.toc` or `.toi` according to data type.
#' @param from_preflib A logical which, when `TRUE` will attempt to source
#' the file from PrefLib by adding the database `HTTP` prefix.
#' @param preflib_url The URL which will be preprended to `file`, if
#' `from_preflib` is `TRUE`.
#' @note The Netflix and cities datasets used in the examples are from
#' Caragiannis et al (2017) and Bennet and Lanning (2007) respectively. These
#' data sets require a citation for re-use.
#' @references
#' Mattei, N. and Walsh, T. (2013) PrefLib: A Library of Preference Data.
#' \emph{Proceedings of Third International Conference on Algorithmic Decision
#' Theory (ADT 2013)}. Lecture Notes in Artificial Intelligence, Springer.
#'
#' Bennett, J. and Lanning, S. (2007) The Netflix Prize.
#' \emph{Proceedings of The KDD Cup and Workshops}.
#'
#' @examples
#'
#' # Can take a little while depending on speed of internet connection
#'
#' \donttest{
#' # strict complete orderings of four films on Netflix
#' netflix <- read_preflib("netflix/00004-00000138.soc", from_preflib = TRUE)
#' head(netflix)
#' levels(netflix$preferences)
#'
#' # strict incomplete orderings of 6 random cities from 36 in total
#' cities <- read_preflib("cities/00034-00000001.soi", from_preflib = TRUE)
#' }
#' @importFrom utils read.csv
#' @importFrom tibble tibble
#' @export
read_preflib <- function(file,
                         from_preflib = FALSE,
                         preflib_url = "https://www.preflib.org/static/data") {
  if (from_preflib) {
    file <- file.path(preflib_url, file)
  }
  # Read the file into memory
  lines <- readLines(file)

  # Filter the header lines and convert them to key:value attributes
  header_lines <- sub("# ", "", grep("^#", lines, value = TRUE))
  metadata <- regmatches(
    header_lines,
    regexpr(": ", header_lines),
    invert = TRUE
  )
  preflib_attributes <- lapply(metadata, function(x) x[2L])
  names(preflib_attributes) <- sapply(metadata, function(x) x[1L])

  # Assert that the minimum required attributes are present
  required_attributes <- c(
    "NUMBER UNIQUE ORDERS",
    "NUMBER VOTERS",
    "NUMBER ALTERNATIVES"
  )

  if (suppressWarnings(
    anyNA(as.integer(unlist(preflib_attributes[required_attributes])))
  )) {
    stop(
      "PrefLib datafile is corrupt: 'NUMBER UNIQUE ORDERS', ",
      "'NUMBER VOTERS' and 'NUMBER ALTERNATIVES' must be able to be ",
      "coerced into integers."
    )
  }
  if (!"NUMBER ALTERNATIVES" %in% names(preflib_attributes)) {
    stop(
      "PrefLib datafile is corrupt: ",
      "missing 'NUMBER ALTERNATIVES' metadata."
    )
  }
  n_alternatives <- as.integer(preflib_attributes[["NUMBER ALTERNATIVES"]])
  required_attributes <- c(
    required_attributes,
    paste(
      "ALTERNATIVE NAME",
      seq_len(n_alternatives)
    )
  )
  if (!all(required_attributes %in% names(preflib_attributes))) {
    missing_attributes <- paste(
      required_attributes[
        which(!required_attributes %in% names(preflib_attributes))
      ],
      sep = ", "
    )
    stop(
      "PrefLib datafile is corrupt: ",
      "missing required metadata (",
      missing_attributes,
      ")."
    )
  }

  # Concatenate all the 'ALTERNATIVE NAME X' into a list of names
  alternative_names <- unname(unlist(
    preflib_attributes[
      paste(
        "ALTERNATIVE NAME",
        seq_len(n_alternatives)
      )
    ]
  ))

  # Filter the data lines and 'encourage' them into a csv format
  data_lines <- grep("^[^#]", lines, value = TRUE)
  data_lines <- chartr("{}", "''", data_lines)
  csv_string <- paste(
    sub(
      "^([0-9]+): ",
      "\\1,",
      data_lines
    ),
    collapse = "\n"
  )
  preferences <- read.csv(
    text = csv_string,
    header = FALSE,
    stringsAsFactors = FALSE,
    strip.white = TRUE,
    quote = "'",
    col.names = paste0("V", 1L:(n_alternatives + 1L)),
    colClasses = rep("character", n_alternatives)
  )
  # Replace character columns with lists
  preferences[, seq(2L, ncol(preferences))] <- as.data.frame(
    do.call(
      rbind,
      apply(
        preferences[, seq(2L, ncol(preferences))],
        1L,
        strsplit,
        split = ","
      )
    )
  )
  # Convert all data in the output to integer-valued vectors
  preferences <- rapply(
    preferences,
    as.integer,
    how = "replace"
  )
  frequency <- preferences[, 1L]
  # Convert orderings to vctrs format
  preferences <- preferences[, -1L] |>
    apply(
      1L,
      \(ord) {
        lapply(
          seq_along(ord),
          \(r) cbind(ord[[r]], rep(r, length(ord[[r]])))
        ) |>
          do.call(what = rbind)
      },
      simplify = FALSE
    ) |>
    vctr_preferences(item_names = alternative_names)

  output_tibble <- tibble(preferences = preferences, frequency = frequency)
  attr(output_tibble, "preflib") <- preflib_attributes

  # Ensure we have the expected number of unique and total orderings.
  n_unique_orders <- as.integer(preflib_attributes[["NUMBER UNIQUE ORDERS"]])
  n_voters <- as.integer(preflib_attributes[["NUMBER VOTERS"]])
  if (length(preferences) != n_unique_orders) {
    warning(
      "Expected ",
      n_unique_orders,
      " unique orderings but only ",
      length(preferences),
      " were recovered when reading the PrefLib datafile.",
      " The file may be corrupt."
    )
  }
  if (sum(frequency) != n_voters) {
    warning(
      "Expected ",
      n_voters,
      " total orderings but only ",
      sum(frequency),
      " were recovered when reading the PrefLib datafile.",
      " The file may be corrupt."
    )
  }

  return(output_tibble)
}

#' Write Ordinal Preference Data to PrefLib Formats
#'
#' Write `preferences` to `.soc`, `.soi`, `.toc` or `.toi` file types, as
#' defined by the PrefLib specification:
#' \href{https://www.preflib.org/}{\{PrefLib\}: A Library for Preferences}.
#'
#' @details The file types supported are
#' \describe{
#' \item{.soc}{Strict Orders - Complete List}
#' \item{.soi}{Strict Orders - Incomplete List}
#' \item{.toc}{Orders with Ties - Complete List}
#' \item{.toi}{Orders with Ties - Incomplete List}
#' }
#'
#' The PrefLib format specification requires some additional metadata. Note
#' that the additional metadata required for the PrefLib specification is not
#' necessarily required for the `write_preflib` method; any missing fields
#' required by the PrefLib format will simply show "NA".
#' \describe{
#' \item{TITLE (required)}{
#'   The title of the data file, for instance the year of the election
#'   represented in the data file.
#' }
#' \item{DESCRIPTION (optional)}{
#'   A description of the data file, providing additional information about it.
#' }
#' \item{RELATES TO (optional)}{
#'   The name of the data file that the current file relates to, typically the
#'   source file in case the current file has been derived from another one.
#' }
#' \item{RELATED FILES (optional)}{
#'   The list of all the data files related to this one, comma separated.
#' }
#' \item{PUBLICATION DATE (required)}{
#'   The date at which the data file was published for the first time.
#' }
#' \item{MODIFICATION TYPE (required)}{
#'   The modification type of the data. One of:
#'   \describe{
#'   \item{original}{Data that has only been converted into a PrefLib format.}
#'   \item{induced}{Data that has been induced from another context. For
#'                  example, computing a pairwise relation from a set of strict
#'                  total orders. No assumptions have been made to create these
#'                  files, just a change in the expression language.}
#'   \item{imbued}{Data that has been imbued with extra information. For
#'                 example, extending an incomplete partial order by placing
#'                 all unranked candidates tied at the end.}
#'   \item{synthetic}{Data that has been generated artificially.}
#'   }
#' }
#' \item{MODIFICATION DATE (optional)}{
#'   The last time the data was modified.
#' }
#' }
#'
#' In addition to these fields, some required PrefLib fields will be generated
#' automatically depending on arguments to `write_preflib()` and the attributes
#' of the `aggregated_preferences` object being written to file:
#' \describe{
#' \item{FILE NAME}{The name of the output file.}
#' \item{DATA TYPE}{The data type (one of `soc`, `soi`, `toc` or `toi`).}
#' \item{NUMBER ALTERNATIVES}{The number of items.}
#' \item{ALTERNATIVE NAME `X`}{The name of each item, where `X` ranges from
#'                             `0` to `length(items)`.}
#' \item{NUMBER VOTERS}{The total number of orderings.}
#' \item{NUMBER UNIQUE ORDERS}{The number of distinct orderings.}
#' }
#'
#' Note that PrefLib refers to the items as "alternatives".
#' The "alternatives" in the output file will be the same as the "items" in the
#' `aggregated_preferences` object.
#'
#' @param x A `preferences` object or a `tibble` with a `preferences`-typed
#' column to write to file.
#' @param file Either a character string naming the a file or a writeable,
#' open connection. The empty string `""` will write to stdout.
#' @param preferences_col <[`tidy-select`][dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the preferences to be written to file.
#' If not provided and `x` is a `tibble`, then
#' @param frequency_col <[`tidy-select`][dplyr_tidy_select]> When `x` is a
#' `tibble`, the column containing the frequency of the preferences. If not
#' provided, each row is considered to be observed a single time.
#' @param title The title of the data file, for instance the name of the
#' election represented in the data file. If not provided, we check for the
#' presence of `attr(x, "preflib")`, and if it exists we check for `TITLE`.
#' @param description A description of the data file, providing additional
#' information about it. If not provided, we check for the presence of
#' `attr(x, "preflib")`, and if it exists we check for `DESCRIPTION`.
#' @param publication_date The date at which the data file was published for the
#' first time. If not provided, we check for the presence of
#' `attr(x, "preflib")`, and if it exists we check for `PUBLICATION DATE`.
#' @param modification_type The modification type of the data: one of
#' `original`, `induced`, `imbued` or `synthetic` (see `Details`). If not
#' provided, we check for the presence of `attr(x, "preflib")`, and if it exists
#' we check for `MODIFICATION TYPE`.
#' @param modification_date The last time the data was modified. If not
#' provided, we check for the presence of `attr(x, "preflib")`, and if it exists
#' we check for `MODIFICATION DATE`.
#' @param relates_to The name of the data file that the current file relates to,
#' typically the source file in case the current file has been derived from
#' another one. If not provided, we check for the presence of
#' `attr(x, "preflib")`, and if it exists we check for `RELATES TO`.
#' @param related_files The list of all the data files related to this one,
#' comma separated. If not provided, we check for the presence of
#' `attr(x, "preflib")`, and if it exists we check for `RELATED FILES`.
#' @return No return value. Output will be written to file or stdout.
#'
#' @export
write_preflib <- function(x, # nolint: cyclocomp_linter
                          file = "",
                          preferences_col = NULL,
                          frequency_col = NULL,
                          title = NULL,
                          publication_date = NULL,
                          modification_type = NULL,
                          modification_date = NULL,
                          description = NULL,
                          relates_to = NULL,
                          related_files = NULL) {
  pl_attr <- NULL
  if ("preflib" %in% names(attributes(x))) {
    pl_attr <- attributes(x)$preflib
  }

  if (missing(title)) {
    if (is.list(pl_attr) && "TITLE" %in% names(pl_attr)) {
      title <- pl_attr[["TITLE"]]
      message(
        "`title` inferred from attributes of `x`: ",
        title
      )
    } else {
      title <- NA
      warning(
        "Missing `title`: the PrefLib format requires a ",
        "title to be specified. Using `NA`."
      )
    }
  }

  if (missing(publication_date)) {
    if (is.list(pl_attr) && "PUBLICATION DATE" %in% names(pl_attr)) {
      publication_date <- pl_attr[["PUBLICATION DATE"]]
      message(
        "`publication_date` inferred from attributes of `x`: ",
        publication_date
      )
    } else {
      publication_date <- format(Sys.time(), "%Y-%m-%d")
      warning(
        "Missing `publication_date`, using today's date(",
        publication_date,
        ")."
      )
    }
  }

  if (missing(modification_date)) {
    if (is.list(pl_attr) && "MODIFICATION DATE" %in% names(pl_attr)) {
      modification_date <- pl_attr[["MODIFICATION DATE"]]
      message(
        "`modification_date` inferred from attributes of `x`: ",
        modification_date
      )
    } else {
      modification_date <- format(Sys.time(), "%Y-%m-%d")
      warning(
        "Missing `modification_date`, using today's date(",
        modification_date,
        ")."
      )
    }
  }

  if (missing(modification_type)) {
    if (is.list(pl_attr) && "MODIFICATION TYPE" %in% names(pl_attr)) {
      modification_type <- pl_attr[["MODIFICATION TYPE"]]
      message(
        "`modification_type` inferred from attributes of `x`: ",
        modification_type
      )
    } else {
      modification_type <- NA
      warning(
        "Missing `modification_type`: the PrefLib format requires ",
        "this to be specified. Using `NA`."
      )
    }
  } else {
    modification_type <- try(
      match.arg(
        modification_type,
        c(
          "original",
          "induced",
          "imbued",
          "synthetic"
        )
      ),
      silent = TRUE
    )
    if (inherits(modification_type, "try-error")) {
      stop(
        "`modification_type` must be one of \"original\", \"induced\",",
        "\"imbued\" or \"synthetic\"."
      )
    }
  }

  if (is.character(file) && file == "") {
    file <- stdout()
    file_name <- "NA"
  } else if (is.character(file)) {
    file_name <- basename(file)
    file <- file(file, "w")
  } else {
    file_name <- "NA"
    warning("File name could not be determined. Check output.")
  }

  if (!inherits(file, "connection")) {
    stop("'file' must be a character string.")
  }

  if (inherits(x, "preferences")) {
    # Convert vector preferences into a tibble with columns `preferences`
    # and `frequency`.
    x <- tibble(preferences = x) |>
      group_by(preferences) |>
      summarise(frequency = n()) |>
      arrange(-frequency)
  } else if (inherits(x, "tbl_df")) {
    # Process tibble.
    # If `preferences_col` is passed, select the appropriate column. Otherwise
    # just look for a preferences-typed column.

    # Get preferences column
    preferences_col <- rlang::enquo(preferences_col)
    if (rlang::quo_is_null(preferences_col)) {
      preferences_col <- rlang::expr(where(~ inherits(.x, "preferences")))
    }
    x_preferences <- x |>
      select(!!preferences_col)
    # Ensure result has one column of "preferences" data.
    preferences_colnames <- x_preferences |>
      sapply(inherits, what = "preferences") |>
      which() |>
      names()
    if (length(preferences_colnames) == 0L) {
      stop(
        "Expected one column of \"preferences\" for ",
        "`write_preflib`, but got 0."
      )
    } else if (length(preferences_colnames) > 1L) {
      warning(
        "Expected one column of \"preferences\" for `write_preflib`, ",
        "but got ", length(preferences_colnames), ". Using `",
        preferences_colnames[1L], "`."
      )
    }
    x_preferences <- x_preferences |>
      select(preferences = preferences_colnames[1L])

    # Get frequency column
    frequency_col <- rlang::enquo(frequency_col)
    if (rlang::quo_is_null(frequency_col)) {
      frequency_col <- NULL
    }
    x_frequency <- x |>
      select(!!frequency_col)
    # Ensure result has one column of "numeric" data.
    if (!is.null(frequency_col)) {
      numeric_colnames <- x_frequency |>
        sapply(is.numeric) |>
        which() |>
        names()
      if (length(numeric_colnames) > 1L) {
        warning(
          "Expected only one column of frequency for `write_preflib`. ",
          "Using `", numeric_colnames[1L], "`."
        )
      }
      x_frequency <- x_frequency |>
        select(frequency = numeric_colnames[1L])
    } else {
      x_frequency <- 1L
    }

    x <- cbind(x_preferences, frequency = x_frequency) |>
      group_by(preferences) |>
      summarise(frequency = sum(frequency)) |>
      arrange(-frequency)
  }

  # Prepare lines for file
  lines <- c(
    # Required metadata
    paste("#", "FILE NAME:", file_name),
    paste("#", "TITLE:", title),
    paste("#", "DESCRIPTION:", description),
    paste("#", "DATA TYPE:", pref_type(x$preferences)),
    paste("#", "MODIFICATION TYPE:", modification_type),
    paste("#", "RELATES TO:", paste(relates_to, collapse = ",")),
    paste("#", "RELATED FILES:", paste(related_files, collapse = ",")),
    paste("#", "PUBLICATION DATE:", publication_date),
    paste("#", "MODIFICATION DATE:", modification_date),
    # Derived metadata
    paste("#", "NUMBER ALTERNATIVES:", nlevels(x$preferences)),
    paste("#", "NUMBER VOTERS:", sum(x$frequency)),
    paste("#", "NUMBER UNIQUE ORDERS:", length(x$preferences))
  )

  items <- attr(x$preferences, "item_names")
  lines <- c(
    lines,
    sapply(
      seq_along(items),
      function(i) paste0("# ALTERNATIVE NAME ", i, ": ", items[i])
    )
  )

  # Format orders as strings

  ordering_str <- vctrs::vec_data(x$preferences) |>
    sapply(
      \(o) paste0(
        sapply(
          split(o[, 1L], o[, 2L]),
          fmt_eql_items # Formats ties ~ {1,2,3,...}
        ),
        collapse = ","
      )
    )
  lines <- c(lines, paste0(x$frequency, ": ", ordering_str))

  writeLines(lines, file, sep = "\n")
  if (file_name != "NA") {
    close(file)
  }
}

# Helper function for formatting list of items at equal rank ~ {1,2,3,...}
fmt_eql_items <- function(items) {
  if (length(items) > 1L) {
    paste0("{", paste0(items, collapse = ","), "}")
  } else {
    items
  }
}
