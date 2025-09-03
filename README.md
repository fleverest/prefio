
# [prefio](https://fleverest.github.io/prefio/) <img src="man/figures/prefio.svg" width="160" align="right" alt="prefio hex sticker" />

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/prefio)](https://cran.r-project.org/package=prefio)
[![R-CMD-check](https://github.com/fleverest/prefio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fleverest/prefio/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/fleverest/prefio/branch/main/graph/badge.svg)](https://app.codecov.io/gh/fleverest/prefio?branch=main)
<!-- badges: end -->

## Overview

Preferential datasets are used by many research communities including,
but not limited to, those who work with elections, recommender systems,
computational social choice, and combinatorial optimization.

**prefio** provides a tidy format for dealing with preferences, along
with a set of functions which enable users to perform a wide range of
analyses.

## Installation

The package may be installed from CRAN via

``` r
install.packages("prefio")
```

The development version can be installed via

``` r
# install.packages("remotes")
remotes::install_github("fleverest/prefio")
```

## Usage

**prefio** provides a tidy interface for processing data from tabular
formats as well as sourcing data from one of the unified [PrefLib
formats](https://preflib.org/format), including a convenient method for
downloading data files directly from PrefLib to your R session.

#### Casting from character vectors

The easiest way to try things out is to write preferences as strings,
then cast to preferences. For example:

``` r
preferences(c("Apple > Banana > Carrot", "Carrot > Banana = Apple"))
```

    ## [1] [Apple > Banana > Carrot] [Carrot > Banana = Apple]

#### Processing long-format data

Preferential datasets can come in many forms. A very common way for
preferential datasets to be stored is in a long-format with item/rank
columns. For example, consider a dataset of votes

|  ID | VoterLocation | Candidate | Rank |
|----:|:--------------|:----------|-----:|
|   1 | Melbourne     | Allie     |    1 |
|   1 | Melbourne     | Beatriz   |    2 |
|   1 | Melbourne     | Charles   |    3 |
|   2 | Wangaratta    | Allie     |    3 |
|   2 | Wangaratta    | Beatriz   |    2 |
|   2 | Wangaratta    | Charles   |    1 |
|   3 | Geelong       | Allie     |    2 |
|   3 | Geelong       | Beatriz   |    1 |
|   3 | Geelong       | Charles   |    3 |

Three preferential votes, ranking three candidates in long-format.

Here, we summarise the votes in a new column of type `preferences`.

Note that, since we are just gathering preferential data from across
multiple rows here, the syntax is quite similar to `dplyr::pivot_wider`.
Indeed, the function is based on this, and extra arguments will be
passed directly to `dplyr::pivot_wider` via `...`.

``` r
long <- tribble(
  ~ID, ~VoterLocation, ~Candidate, ~Rank,
  1, "Melbourne", "Allie", 1,
  1, "Melbourne", "Beatriz", 2,
  1, "Melbourne", "Charles", 3,
  2, "Wangaratta", "Allie", 3,
  2, "Wangaratta", "Beatriz", 2,
  2, "Wangaratta", "Charles", 1,
  3, "Geelong", "Allie", 2,
  3, "Geelong", "Beatriz", 1,
  3, "Geelong", "Charles", 3
)

long |>
  long_preferences(
    vote,
    id_cols = c(ID, VoterLocation),
    rank_col = Rank,
    item_col = Candidate
  )
```

    ## # A tibble: 3 × 3
    ##      ID VoterLocation                        vote
    ##   <dbl> <chr>                          <prefrncs>
    ## 1     1 Melbourne     [Allie > Beatriz > Charles]
    ## 2     2 Wangaratta    [Charles > Beatriz > Allie]
    ## 3     3 Geelong       [Beatriz > Allie > Charles]

#### Processing wide-format data

Another common way to store preferential data is in wide-format, where
each column represents a candidate/item and the values represent the
rank assigned. Let’s recreate our previous example but in wide-format:

|  ID | VoterLocation | Allie | Beatriz | Charles |
|----:|:--------------|------:|--------:|--------:|
|   1 | Melbourne     |     1 |       2 |       3 |
|   2 | Wangaratta    |     3 |       2 |       1 |
|   3 | Geelong       |     2 |       1 |       3 |

Three preferential votes, ranking three candidates in wide-format.

Here, we summarise the votes in a new column of type `preferences`.

``` r
wide <- tribble(
  ~ID, ~VoterLocation, ~Allie, ~Beatriz, ~Charles,
  1, "Melbourne", 1, 2, 3,
  2, "Wangaratta", 3, 2, 1,
  3, "Geelong", 2, 1, 3
)

wide |>
  wide_preferences(vote, Allie:Charles)
```

    ## # A tibble: 3 × 3
    ##      ID VoterLocation                        vote
    ##   <dbl> <chr>                          <prefrncs>
    ## 1     1 Melbourne     [Allie > Beatriz > Charles]
    ## 2     2 Wangaratta    [Charles > Beatriz > Allie]
    ## 3     3 Geelong       [Beatriz > Allie > Charles]

#### Reading from PrefLib

The [Netflix Prize](https://en.wikipedia.org/wiki/Netflix_Prize) was a
competition devised by Netflix to improve the accuracy of its
recommendation system. To facilitate this they released ratings about
movies from the users of the system that have been transformed to
preference data and are available from
[PrefLib](https://www.preflib.org/data/ED/00004/), (Bennett and Lanning
2007). Each data set comprises rankings of a set of 3 or 4 movies
selected at random. Here we consider rankings for just one set of movies
to illustrate the functionality of **prefio**.

PrefLib datafiles such as these can be downloaded on-the-fly by
specifying the argument `from_preflib = TRUE` in the `read_preflib`
function:

``` r
netflix <- read_preflib("00004 - netflix/00004-00000138.soc", from_preflib = TRUE)
head(netflix)
```

    ## # A tibble: 6 × 2
    ##                                                                     preferences
    ##                                                                      <prefrncs>
    ## 1 [Beverly Hills Cop > Mean Girls > Mission: Impossible II > The Mummy Returns]
    ## 2 [Mean Girls > Beverly Hills Cop > Mission: Impossible II > The Mummy Returns]
    ## 3 [Beverly Hills Cop > Mean Girls > The Mummy Returns > Mission: Impossible II]
    ## 4 [Mean Girls > Beverly Hills Cop > The Mummy Returns > Mission: Impossible II]
    ## 5 [Beverly Hills Cop > Mission: Impossible II > Mean Girls > The Mummy Returns]
    ## 6 [The Mummy Returns > Beverly Hills Cop > Mean Girls > Mission: Impossible II]
    ## # ℹ 1 more variable: frequency <int>

Each row corresponds to a unique ordering of the four movies in the
dataset. The number of Netflix users that assigned each ordering is
given in the `frequency` column. In this case, the most common ordering
(with 68 voters specifying the same preferences) is the following:

``` r
netflix$preferences[1]
```

    ## [1] [Beverly Hills Cop > Mean Girls > Mission: Impossible II > The Mummy Returns]

#### Writing to Preflib formats

**prefio** provides a convenient interface for writing preferential
datasets to PrefLib formats. To aid the user, the `preferences()`
function automatically calculates metrics of the dataset which are
required for producing valid PrefLib files. For example, we can write
our example from earlier to a PrefLib format:

``` r
long |>
  long_preferences(
    vote,
    id_cols = ID,
    rank_col = Rank,
    item_col = Candidate,
    unused_fn = list(VoterLocation = dplyr::first)
  ) |>
  write_preflib(preferences_col = vote)
```

    ## Warning in write_preflib(long_preferences(long, vote, id_cols = ID, rank_col =
    ## Rank, : Missing `title`: the PrefLib format requires a title to be specified.
    ## Using `NA`.

    ## Warning in write_preflib(long_preferences(long, vote, id_cols = ID, rank_col =
    ## Rank, : Missing `publication_date`, using today's date(2025-09-03).

    ## Warning in write_preflib(long_preferences(long, vote, id_cols = ID, rank_col =
    ## Rank, : Missing `modification_date`, using today's date(2025-09-03).

    ## Warning in write_preflib(long_preferences(long, vote, id_cols = ID, rank_col =
    ## Rank, : Missing `modification_type`: the PrefLib format requires this to be
    ## specified. Using `NA`.

    ## # FILE NAME: NA
    ## # TITLE: NA
    ## # DESCRIPTION: 
    ## # DATA TYPE: soc
    ## # MODIFICATION TYPE: NA
    ## # RELATES TO: 
    ## # RELATED FILES: 
    ## # PUBLICATION DATE: 2025-09-03
    ## # MODIFICATION DATE: 2025-09-03
    ## # NUMBER ALTERNATIVES: 3
    ## # NUMBER VOTERS: 3
    ## # NUMBER UNIQUE ORDERS: 3
    ## # ALTERNATIVE NAME 1: Allie
    ## # ALTERNATIVE NAME 2: Beatriz
    ## # ALTERNATIVE NAME 3: Charles
    ## 1: 1,2,3
    ## 1: 3,2,1
    ## 1: 2,1,3

Note that this produces four warnings. Each warning corresponds to a
field which is required by the official PrefLib format, but may not be
necessary for internal use-cases. If your goal is to publish some data
to PrefLib, these warnings must be resolved.

## Projects using **prefio**

The [PrefLib formatter for New South Wales Legislative Assembly
Elections](https://github.com/fleverest/nswla_preflib) uses **prefio**
to process the public election datasets into PrefLib formats.

The R package
[elections.dtree](https://github.com/fleverest/elections.dtree) uses
**prefio** for tracking ballots observed by the Dirichlet-tree model.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Bennett2007" class="csl-entry">

Bennett, J., and S. Lanning. 2007. “The Netflix Prize.” In
*<span class="nocase">Proceedings of the KDD Cup Workshop 2007</span>*,
3–6. ACM.

</div>

</div>
