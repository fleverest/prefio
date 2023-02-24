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
#' data frame, storing the item names as an attribute. The
#' `as.aggregated_preferences` method converts these to an
#' [`aggregated_preferences`][aggregate.preferences] object with the
#' items labelled by name.
#'
#' A Preflib file may be corrupt, in the sense that the ordered alternatives do
#' not match their names. In this case, the file can be read in as a data
#' frame (with a warning), but `as.aggregated_preferences` will throw an error.
#'
#' @return An [`aggregated_preferences`][aggregate.preferences] object
#' containing the PrefLib data.
#'
#' @param file A preferential data file, conventionally with extension `.soc`,
#' `.soi`, `.toc` or `.toi` according to data type.
#' @param x An object of class `preflib`.
#' @param as.aggregated_preferences When `TRUE`, returns an object of class
#' [`aggregated_preferences`][aggregate.preferences]. When `FALSE`, returns
#' a `data.frame` containing the raw PrefLib data.
#' @param ... Additional arguments passed to [as.aggregated_preferences()]:
#' `frequencies`, `format` or `item_names` will be ignored as they are
#' set automatically.
#' @note The Netflix and cities datasets used in the examples are from
#' Caragiannis et al (2017) and Bennet and Lanning (2007) respectively. These
#' data sets require a citation for re-use.
#' @references
#' Mattei, N. and Walsh, T. (2013) PrefLib: A Library of Preference Data.
#' \emph{Proceedings of Third International Conference on Algorithmic Decision
#' Theory (ADT 2013)}. Lecture Notes in Artificial Intelligence, Springer.
#'
#' Caragiannis, I., Chatzigeorgiou, X, Krimpas, G. A., and Voudouris, A. A.
#' (2017) Optimizing positional scoring rules for rank aggregation.
#' In \emph{Proceedings of the 31st AAAI Conference on Artificial Intelligence}.
#'
#' Bennett, J. and Lanning, S. (2007) The Netflix Prize.
#' \emph{Proceedings of The KDD Cup and Workshops}.
#'
#' @examples
#'
#' # Can take a little while depending on speed of internet connection
#'
#' \dontrun{
#' # url for preflib data in the "Election Data" category
#' preflib <- "https://www.preflib.org/static/data/"
#'
#' # strict complete orderings of four films on Netflix
#' netflix <- read.preflib(file.path(preflib, "netflix/00004-00000101.soc"))
#' head(netflix)
#' attr(netflix, "item_names")
#'
#' # strict incomplete orderings of 6 random cities from 36 in total
#' cities <- read.preflib(file.path(preflib, "cities/00034-00000001.soi"))
#' }
#' @importFrom utils read.csv
#' @name preflib
NULL

#' @rdname preflib
#' @export
read.preflib <- function(file) {
    # Read the file into memory
    lines <- readLines(file)

    # Filter the header lines and convert them to key:value attributes
    header_lines <- sub("# ", "", grep("^#", lines, value = TRUE))
    metadata <- regmatches(
      header_lines,
      regexpr(": ", header_lines),
      invert = TRUE
    )
    preflib_attributes <- lapply(metadata, function(x) x[2])
    names(preflib_attributes) <- sapply(metadata, function(x) x[1])

    # Assert that the minimum required attributes are present
    required_attributes <- c("NUMBER UNIQUE ORDERS",
                             "NUMBER VOTERS",
                             "NUMBER ALTERNATIVES")

    if (suppressWarnings(
          anyNA(as.integer(unlist(preflib_attributes[required_attributes]))))) {
      stop("PrefLib datafile is corrupt: 'NUMBER UNIQUE ORDERS', ",
           "'NUMBER VOTERS' and 'NUMBER ALTERNATIVES' must be able to be ",
           "coerced into integers.")
    }
    if (!"NUMBER ALTERNATIVES" %in% names(preflib_attributes)) {
      stop(paste0("PrefLib datafile is corrupt: ",
                  "missing 'NUMBER ALTERNATIVES' metadata."))
    }
    n_alternatives <- as.integer(preflib_attributes[["NUMBER ALTERNATIVES"]])
    required_attributes <- c(required_attributes,
                             paste("ALTERNATIVE NAME",
                                   seq_len(n_alternatives)))
    if (any(!required_attributes %in% names(preflib_attributes))) {
      stop(paste0("PrefLib datafile is corrupt: ",
                  "missing required metadata (",
                  paste(required_attributes[
                      which(!required_attributes %in% names(preflib_attributes))
                    ]
                  ),
                  ")."))
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
    preferences <- read.csv(text = csv_string,
                            header = FALSE,
                            stringsAsFactors = FALSE,
                            strip.white = TRUE,
                            quote = "'",
                            col.names = paste0("V", 1:(n_alternatives + 1)),
                            colClasses = rep("character", n_alternatives))
    # Replace NA with blank ties
    preferences[is.na(preferences)] <- ""
    # Replace character columns with lists
    preferences[, seq(2, ncol(preferences))] <- as.data.frame(
      sapply(
        preferences[, seq(2, ncol(preferences))],
        strsplit,
        split = ","
      )
    )
    # Convert all data in the output to integer-valued vectors
    preferences <- rapply(
      preferences,
      as.integer,
      how = "replace"
    )
    names(preferences) <- c(
      "Frequency",
      paste("Rank", seq_len(ncol(preferences)  - 1))
    )
    frequencies <- preferences[, 1]
    preferences <- preferences[, -1]
    preferences <- as.preferences(
      preferences,
      "ordering",
      item_names = alternative_names
    )
    aggregated_prefs <- aggregate(preferences, frequencies = frequencies)
    attr(aggregated_prefs, "preflib") <- preflib_attributes

    # Ensure we have the expected number of unique and total orderings.
    n_unique_orders <- as.integer(preflib_attributes[["NUMBER UNIQUE ORDERS"]])
    n_voters <- as.integer(preflib_attributes[["NUMBER VOTERS"]])
    if (length(aggregated_prefs$preferences) != n_unique_orders) {
      warning(paste0("Expected ",
                     n_unique_orders,
                     " unique orderings but only ",
                     length(aggregated_prefs$preferences),
                     " were recovered when reading the PrefLib datafile.",
                     " The file may be corrupt."
                      ))
    }
    if (sum(aggregated_prefs$frequencies) != n_voters) {
      warning(paste0("Expected ",
                     n_voters,
                     " total orderings but only ",
                     sum(aggregated_prefs$frequencies),
                     " were recovered when reading the PrefLib datafile.",
                     " The file may be corrupt."
                      ))
    }

    return(aggregated_prefs)
}

#' Write Ordinal Preference Data to PrefLib Formats
#'
#' Write `preferences` to `.soc`, `.soi`, `.toc` or `.toi` file types, as
#' defined by the Preflib specification:
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
#' Writing to PrefLib format requires the following additional metadata:
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
#' On top of these fields, some required PrefLib fields will be generated
#' automatically depending on arguments to `write.preflib` and the attributes
#' of the `preferences` object being written to file:
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
#' Note that PrefLib refers to the items being ordered by "alternatives",
#' so the "alternatives" in the output file are the same as "items" in your
#' `preferences` object.
#'
#' @param x Some `aggregated_preferences` to write to file. If `x` is of a
#' class, it attempts to coerce `x` into an `aggregated_preferences` object via
#' `as.aggregated_preferences`.
#' @param file Either a character string naming the a file or a writeable,
#' open connection. The empty string `""` will write to stdout.
#' @param as.toc A logical indicating whether or not incomplete orderings
#' should be converted to complete orderings (with ties) by adding the
#' unordered items tied as last-place. Both `toi` and `soi` ordinal preferences
#' can be converted to this representation, but this operation does not make
#' sence for complete orderings. If `x` is of type `toc` or `soc`, this will
#' be ignored.
#'
#' @export
write.preflib <- function(x, file = "", as.toc = FALSE) {
  stop("Not implemented.")
}
