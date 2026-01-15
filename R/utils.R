#' Suppress low counts
#'
#' This suppresses values with low counts for privacy reasons etc.
#'
#' @param x a numeric vector of counts
#' @param threshold numeric threshold for the number below which to suppress
#' @param symbol What the suppressed values should be, default is NA
#'
#' @returns A vector with low counts suppressed.
#'
#' @export
#' @examples
#' suppress_counts(1:10)
#' suppress_counts(1:20, threshold=10)
#' suppress_counts(1:20, symbol="*")
suppress_counts <- function(x, threshold = 5, symbol = NA) {
  # Check if input is numeric
  if (!is.numeric(x)) {
    stop("Input x must be numeric.")
  }

  x[x < threshold] <- symbol
  return(x)
}
