# Suppress low counts

This suppresses values with low counts for privacy reasons etc.

## Usage

``` r
suppress_counts(x, threshold = 5, symbol = NA)
```

## Arguments

- x:

  a numeric vector of counts

- threshold:

  numeric threshold for the number below which to suppress

- symbol:

  What the suppressed values should be, default is NA

## Value

A vector with low counts suppressed.

## Examples

``` r
suppress_counts(1:10)
#>  [1] NA NA NA NA  5  6  7  8  9 10
suppress_counts(1:20, threshold=10)
#>  [1] NA NA NA NA NA NA NA NA NA 10 11 12 13 14 15 16 17 18 19 20
suppress_counts(1:20, symbol="*")
#>  [1] "*"  "*"  "*"  "*"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12" "13" "14" "15"
#> [16] "16" "17" "18" "19" "20"
```
