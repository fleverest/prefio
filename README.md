
# prefio

<!--
Package website: https://fleverest.github.io/prefio/.
-->

## Overview

Ordinal Preference datasets are used by many research communities
including, but not limited to, those who work with recommender systems,
computational social choice, voting systems and combinatorial
optimization.

The **prefio** R package provides a set of functions which enable users
to perform a wide range of preference analysis tasks, including
preference aggregation, pairwise comparison summaries and convenient IO
operations. This makes it easier for researchers and other professionals
to perform common data analysis and preprocessing tasks with such
datasets.

## Installation

<!--
The package may be installed from CRAN via
&#10;
```r
install.packages("prefio")
```
-->

The development version can be installed via

``` r
# install.packages("remotes")
remotes::install_github("fleverest/prefio")
```

## Usage

**prefio** provides a convenient interface for processing data from
tabular formats as well as sourcing data from one of the unified
[PrefLib formats](https://www.preflib.org/format/), including a
convenient method for downloading data files directly from PrefLib to
your R session.

#### Processing tabular data

Preference data can come in many forms. Commonly preference data will be
either represented in either *long*-format with each row corresponding
to a particular *ranking* chosen for a single *item*:, e.g:

|  ID | ItemName | Rank |
|----:|:---------|-----:|
|   1 | A        |    1 |
|   1 | B        |    2 |
|   1 | C        |    3 |
|   2 | A        |    3 |
|   2 | B        |    2 |
|   2 | C        |    1 |
|   3 | A        |    2 |
|   3 | B        |    1 |
|   3 | C        |    3 |

Three orderings on items {A, B, C} in long-format.

This data can be converted from a `data.frame` into a `preferences`
object:

``` r
long <- data.frame(
  ID = rep(1:3, each = 3),
  ItemName = LETTERS[rep(1:3, 3)],
  Rank = c(1, 2, 3, 3, 2, 1, 2, 1, 3)
)
prefs <- preferences(long,
                     format = "long",
                     id = "ID",
                     item = "ItemName",
                     rank = "Rank")
print(prefs)
```

    ## [1] "A > B > C" "C > B > A" "B > A > C"

Another way of tabulating orderings is with each unique ordering on a
single row, with each column representing the rank given to a particular
item:

|   A |   B |   C |
|----:|----:|----:|
|   1 |   2 |   3 |
|   3 |   2 |   1 |
|   2 |   1 |   3 |

Three orderings on items {A, B, C} in a “rankings” format.

This data can be converted from a `data.frame` into a `preferences`
object:

``` r
rankings <- matrix(c(
  1, 2, 3,
  3, 2, 1,
  2, 1, 3
  ),
  nrow = 3,
  byrow = TRUE
)
colnames(rankings) <- LETTERS[1:3]
prefs <- preferences(rankings,
                     format = "ranking")
print(prefs)
```

    ## [1] "A > B > C" "C > B > A" "B > A > C"

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
netflix <- read_preflib("netflix/00004-00000138.soc", from_preflib = TRUE)
head(netflix)
```

    ##                                preferences frequencies
    ## 1 Beverly Hills Cop > Mean Girls > Mis ...          68
    ## 2 Mean Girls > Beverly Hills Cop > Mis ...          53
    ## 3 Beverly Hills Cop > Mean Girls > The ...          49
    ## 4 Mean Girls > Beverly Hills Cop > The ...          44
    ## 5 Beverly Hills Cop > Mission: Impossi ...          39
    ## 6 The Mummy Returns > Beverly Hills Co ...          37

Each row corresponds to a unique ordering of the four movies in the
dataset. The number of Netflix users that assigned each ordering is
given in the `frequencies` column. In this case, the most common
ordering (with 68 voters specifying the same preferences) is the
following:

``` r
print(netflix$preferences[1], width = 100)
```

    ## [1] "Beverly Hills Cop > Mean Girls > Mission: Impossible II > The Mummy Returns"

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Bennett2007" class="csl-entry">

Bennett, J., and S. Lanning. 2007. “The Netflix Prize.” In
*<span class="nocase">Proceedings of the KDD Cup Workshop 2007</span>*,
3–6. ACM.

</div>

</div>
