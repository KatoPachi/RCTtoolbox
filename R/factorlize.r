#' Convert to Factor Variables
#'
#' @param x vector
#' @param base character. Name of the control.
#'
factorlize <- function(x, base) {
  x <- factor(x)
  arms <- levels(x)
  if (is.null(base)) base <- arms[1]
  x <- factor(x, c(base, arms[grep(paste0("[^", base, "]"), arms)]))
  x
}