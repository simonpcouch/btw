
<!-- README.md is generated from README.Rmd. Please edit that file -->

# btw

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/btw)](https://CRAN.R-project.org/package=btw)
<!-- badges: end -->

The goal of btw is to allow you to describe objects in your R
environment in plain text.

- When used **interactively**, `btw::btw()` assembles context on your
  global R environment as well as function or package documentation,
  copying the results to your clipboard for easy pasting into chat
  interfaces.
- The `btw()` function wraps several lower-level functions that can be
  easily incorporated into **ellmer tool calls** for describing various
  kinds of objects in R.

## Installation

You can install the development version of btw like so:

``` r
pak::pak("simonpcouch/btw")
```

## Example

``` r
library(btw)
```

The easiest way to interact with btw is to just call `btw()` with no
inputs, which will describe the objects in your global environment in a
string attached to your clipboard:

``` r
btw()
```

    ✔ btw copied to the clipboard!

The package allows you to describe all sorts of objects, though:

``` r
btw(
  # this dataset in my global environment
  "my_df",
  # this package's documentation
  this_pkg("tidyr")
)
```

    ✔ btw copied to the clipboard!
