#' Suppress low counts
#'
#' This suppresses values with low counts for privacy reasons etc.
#'
#' @param x a numeric vector of counts
#' @param threshold numeric threshold for the number below which to suppress
#'
#' @returns A vector with low counts suppressed.
#'
#' @export
#' @examples
#' suppress_counts(1:10)
#' suppress_counts(1:20, threshold=10)
suppress_counts <- function(x, threshold = 5) {
  x[x < threshold] <- NA
  return(x)
}
