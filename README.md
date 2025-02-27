
<!-- README.md is generated from README.Rmd. Please edit that file -->

# btw

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/btw)](https://CRAN.R-project.org/package=btw)
[![R-CMD-check](https://github.com/simonpcouch/btw/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/simonpcouch/btw/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/simonpcouch/btw/graph/badge.svg)](https://app.codecov.io/gh/simonpcouch/btw)
<!-- badges: end -->

btw helps you describe your computational environment to LLMs.

- When used **interactively**, `btw()` assembles context on your global
  R environment as well as function or package documentation, copying
  the results to your clipboard for easy pasting into chat interfaces.
- The `btw()` function wraps several lower-level functions that can be
  easily incorporated into **ellmer tool calls** for describing various
  kinds of objects in R. To equip your ellmer chat with the ability to
  peruse documentation and check out the objects in your R environment,
  pass your chat to `btw_register_tools()`.

## Installation

You can install the development version of btw like so:

``` r
pak::pak("simonpcouch/btw")
```

## Example

``` r
library(btw)
```

### Interactive use

The `btw()` function allows you to compile information about R
stuff–objects, documentation, etc–and copy it to your clipboard.

The easiest way to use btw interactively is to just call `btw()` with no
inputs, which will describe the objects in your global environment in a
string attached to your clipboard:

``` r
btw()
```

    ✔ btw copied to the clipboard!

The package allows you to describe all sorts of objects, though. For
example, if you’d like to describe the mtcars dataset:

``` r
btw(mtcars)
```

The following would be attached to your clipboard:

    mtcars
    #> {"n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{"mpg":{"variable":"mpg","type":"numeric","mean":20.0906,"sd":6.0269,"p0":10.4,"p25":15.425,"p50":19.2,"p75":22.8,"p100":33.9},"cyl":{"variable":"cyl","type":"numeric","mean":6.1875,"sd":1.7859,"p0":4,"p25":4,"p50":6,"p75":8,"p100":8},"disp":{"variable":"disp","type":"numeric","mean":230.7219,"sd":123.9387,"p0":71.1,"p25":120.825,"p50":196.3,"p75":326,"p100":472},"hp":{"variable":"hp","type":"numeric","mean":146.6875,"sd":68.5629,"p0":52,"p25":96.5,"p50":123,"p75":180,"p100":335},"drat":{"variable":"drat","type":"numeric","mean":3.5966,"sd":0.5347,"p0":2.76,"p25":3.08,"p50":3.695,"p75":3.92,"p100":4.93},"wt":{"variable":"wt","type":"numeric","mean":3.2172,"sd":0.9785,"p0":1.513,"p25":2.5812,"p50":3.325,"p75":3.61,"p100":5.424},"qsec":{"variable":"qsec","type":"numeric","mean":17.8487,"sd":1.7869,"p0":14.5,"p25":16.8925,"p50":17.71,"p75":18.9,"p100":22.9},"vs":{"variable":"vs","type":"numeric","mean":0.4375,"sd":0.504,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"am":{"variable":"am","type":"numeric","mean":0.4062,"sd":0.499,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"gear":{"variable":"gear","type":"numeric","mean":3.6875,"sd":0.7378,"p0":3,"p25":3,"p50":4,"p75":4,"p100":5},"carb":{"variable":"carb","type":"numeric","mean":2.8125,"sd":1.6152,"p0":1,"p25":2,"p50":2,"p75":4,"p100":8}}}

Or, to describe the `btw()` function:

``` r
btw(btw::btw)
```

The following would be attached to your clipboard:

    btw::btw
    #> btw                    package:btw                     R Documentation
    #> 
    #> Plain-text descriptions of R objects
    #> 
    #> Description:
    #> 
    #>      This function allows you to quickly describe your computational
    #>      environment to a model by concatenating plain-text descriptions of
    #>      "R stuff", from data frames to packages to function documentation.
    #> 
    #> Usage:
    #> 
    #>      btw(..., clipboard = is_interactive())
    #>      
    #> Arguments:
    #> 
    #>      ...: Objects to describe from your R environment. You can pass
    #>           objects themselves, like data frames or functions, or the
    #>           function also accepts output from btw_tool_*() functions like
    #>           'btw_tool_get_package_help_topics()',
    #>           'btw_tool_get_help_page()', etc. If omitted, this function
    #>           will just describe the elements in your global R environment.
    #> 
    #> clipboard: Whether to write the results to the clipboard. A single
    #>           logical value; will default to 'TRUE' when run interactively.
    #> 
    #> Value:
    #> 
    #>      The combined elements as a string, invisibly. If 'clipboard' is
    #>      'TRUE', the result is also written to the system clipboard.
    #> 
    #> Examples:
    #> 
    #>      btw()
    #>      
    #>      btw(mtcars)
    #>      
    #>      btw(btw::btw)
    #>      

### Supercharging assistants

`btw_register_tools()` equips LLM chats with the ability to peruse the
documentation of your installed packages and check out objects in your R
environment. Use it by calling the function on an existing ellmer chat:

``` r
ch <- ellmer::chat_claude()
ch <- btw_register_tools(ch)

ch$chat("Hey!")
```

    Hello! I'm here to help you work with R-related information and data. I can help 
    you:

    1. Look up information about installed R packages
    2. Get help documentation for specific R packages and functions
    3. Examine and manipulate data frames
    4. Check what's available in the current environment

    What would you like to know about? Feel free to ask a specific question, and I'll 
    use the available tools to help you find the information you need.
