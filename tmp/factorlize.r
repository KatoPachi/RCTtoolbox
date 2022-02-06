#' Convert to Factor Variables
#'
#' @param x vector
#' @param ctrl character. Name of the control.
#'
factorlize <- function(x, ctrl) {
  x <- factor(x)
  arms <- levels(x)
  if (is.null(ctrl)) ctrl <- arms[1]
  x <- factor(x, c(ctrl, arms[grep(paste0("[^", ctrl, "]"), arms)]))
  x
}