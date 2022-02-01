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
#'
se <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- na.omit(x)
  }
  sqrt(var(x) / length(x))
}

#' T-Test for RCTs with Multiple Experimental Arms
#'
#' @description This function performs a t-test to test the null hypothesis
#' that there is no difference between the means of the two groups.
#' The user specifies the outcome variable and treatment variable
#' in the formula object.
#' When there are three or more treatment groups,
#' this function tests the difference of mean between
#' the control group specified by the user with the `base` argument
#' and each group (including the control group).
#'
#' @param mod formula.
#' Specify `y ~ d` where `y` is outcome and `d` is treatment.
#' @param data data object you want to use.
#' @param base character. Specify the name of control group.
#' Default is NULL.
#' If `base = NULL`, the first level of `d` is the control.
#'
#' @return tibble frame containing with five variables.
#' `treat` is a factor of treatments (baseline is the control).
#' `mean` and `se` is a mean and its standard error of each group.
#' `effect` is a difference of mean between each group and the control.
#' `p.value` is a p-value of t-test.
#'
#' @importFrom stats t.test
#' @importFrom broom tidy
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' set.seed(120511)
#' a <- rnorm(100)
#' b <- rnorm(100, mean = 2)
#' c <- rnorm(100, mean = 2, sd = 2)
#' ex <- data.frame(
#'   y = c(a, b, c),
#'   d = c(rep("A", 100), rep("B", 100), rep("C", 100))
#' )
#' ttest(y ~ d, ex, base = "B")
#'
ttest <- function(mod, data, base = NULL) {
  # extract outcome and treatment variables label
  ylab <- all.vars(mod)[1]
  dlab <- all.vars(mod)[2]
  # find treatment levels and define baseline if NULL
  d <- data[[dlab]]
  if (!is.factor(d)) d <- factor(d)
  treat <- levels(d)
  if (is.null(base)) base <- treat[1]
  # vector of outcome variables
  y0 <- data[d == base, ][[ylab]]
  y1 <- lapply(treat, function(x) data[d == x, ][[ylab]])
  # t-test
  t <- lapply(y1, t.test, y = y0)
  t <- lapply(t, broom::tidy)
  # standard errors of mean
  stderr <- lapply(y1, se, na.rm = TRUE)
  # data.frame containing result
  out <- dplyr::bind_rows(t)
  out <- out[, c("estimate", "estimate1", "p.value")]
  colnames(out) <- c("effect", "mean", "p.value")
  out$se <- unlist(stderr)
  new_levels <- c(base, treat[grep(paste0("[^", base, "]"), treat)])
  out$treat <- factor(treat, levels = new_levels)
  out$outcome <- ylab
  out <- out[, c("outcome", "treat", "mean", "se", "effect", "p.value")]
  # output
  class(out) <- append("t_test", class(out))
  out
}
