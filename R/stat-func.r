#' Calculation Standard Errors of Mean
#'
#' @param x numeric vector.
#' @param na.rm logical.
#' Specify whether to calculate without missing values.
#'
#' @importFrom stats na.omit
#' @importFrom stats var
#' @export
#' @examples
#' set.seed(120511)
#' a <- rnorm(100)
#' mean(a)
#' se(a)
se <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- na.omit(x)
  }
  sqrt(var(x) / length(x))
}

