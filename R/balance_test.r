#' Blance Test by F-test of Overall Significance in Linear Regression
#'
#' @description This function performs a balance test of the treatment
#' with multiple covariates.
#' The balance test calculates the mean of each group
#' and the F-test by linear regression analysis.
#'
#' @param xlist string vector with covariates.
#' If missing, try to find form `option("RCTtool.xlist")`
#' @param arms string of treatment variables.
#' If missing, try to find from `option("RCTtool.arms")`
#' @param data data which you want to use.
#'
#' @return A data frame with `balance_test` class
#'
#' @importFrom stats as.formula
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' # DGP
#' set.seed(120511)
#' n <- 1000
#' x1 <- rnorm(n); x2 <- rnorm(n)
#' d1 <- sample(c("A", "B", "C"), size = n, replace = TRUE)
#' d2 <- ifelse(d1 == "A", "control", "treat")
#' ya <- 0.2 + 0.5 * x1 + 0.01 * x2
#' yb <- 1.2 + 0.3 * x2
#' yc <- -1 - 0.2 * x1 + 0.5 * x2
#' y <- ifelse(d1 == "A", ya, ifelse(d1 == "B", yb, yc))
#' dt <- data.frame(outcome = y, treat = d1, bin_treat = d2, x1, x2)
#'
#' # F-test
#' set_optRCTtool(outcome ~ treat, ~ x1, dt, "A")
#'
#' balance_test(data = dt)
#' balance_test(data = subset(dt, treat != "B"))
#' balance_test(c("x1", "x2"), data = dt)
#' balance_test(arms = "bin_treat", data = dt)
#'
#' set_optRCTtool(xmod = ~ x1 + x2, data = dt)
#' balance_test(data = dt)
#'
#' clear_optRCTtool()
#'
#'
balance_test <- function(xlist, arms, data) {
  # check RCTtool.xlist option
  if (missing(xlist)) {
    xlist <- getOption("RCTtool.xlist")
    if (all(xlist == "")) stop("Not register option('RCTtool.xlist').")
  }

  # check RCTtool.arms option
  if (missing(arms)) {
    arms <- getOption("RCTtool.arms")
    if (arms == "") stop("Not register option('RCTtool.arms')")
  }

  # check RCTtool.arms_label option
  if (missing(arms)) {
    label <- getOption("RCTtool.arms_label")
    if (all(label == "")) {
      warning("You should register option('RCTtool.arms_label')")
      dv <- data[[arms]]
      if (!is.factor(dv)) dv <- factor(dv)
      label <- levels(dv)
    }
  } else {
    dv <- data[[arms]]
    if (!is.factor(dv)) dv <- factor(dv)
    label <- levels(dv)
  }

  # implement F-test
  f <- lapply(xlist, ftest, arms, label, data)
  f <- dplyr::bind_rows(f)

  # output
  class(f) <- append(class(f), "balance_test")
  f
}
