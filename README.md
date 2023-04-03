
<!-- README.md is generated from README.Rmd. Please edit that file -->

# omuecon

<!-- badges: start -->
<!-- badges: end -->

The goal of omuecon is to offer useful tools to minimize routine
formatting of pages of Moodle, meeting reports, etc.

## Installation

You can install the development version of omuecon like so:

``` r
# install.packages("remotes")
remotes::install_github("kenjisato/omuecon")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(omuecon)
moodle_html("mypage.Rmd")
```

The result will be copied to the clipboard, ready to paste on a Moodle
site.
