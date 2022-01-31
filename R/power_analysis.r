#'
#'
calc_power <- function(n0, n1, d, alpha, power) {
  delta <- d * sqrt(1 / n1 + 1 / n0) ^ (-1)
  df <- n1 + n0 - 2
  critical <- qt(alpha / 2, df = df)
  power - (
    pt(-critical, df = df, ncp = delta, lower.tail = FALSE) +
    pt(critical, df = df, ncp = delta, lower.tail = TRUE)
  )
}

#'
#' Power Analysis of T-Test for RCT with Multiple Experimental Arms
#'
#' @export
#'
power_analysis <- function(mod, data, base = NULL) {
  # extract treatment variables
  if (length(all.vars(mod)) > 1) {
    treat <- all.vars(mod)[2]
  } else {
    treat <- all.vars(mod)[1]
  }
  # check class of treatment variables
  dvec <- data[[treat]]
  if (!is.factor(dvec)) dvec <- factor(dvec)
  # treatment levels
  arms <- levels(dvec)
  if (is.null(base)) base <- arms[1]
    
}