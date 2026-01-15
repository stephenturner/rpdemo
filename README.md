
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rpdemo

<!-- badges: start -->

<!-- badges: end -->

The goal of rpdemo is to …

## Installation

You can install the development version of rpdemo from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("stephenturner/rpdemo")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rpdemo)
suppress_counts(1:20, threshold = 10)
#>  [1] NA NA NA NA NA NA NA NA NA 10 11 12 13 14 15 16 17 18 19 20
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:
