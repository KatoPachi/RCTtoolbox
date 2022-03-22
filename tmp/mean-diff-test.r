#' T-Test for RCTs with Multiple Experimental Arms
#'
#' @description This function performs a t-test to test the null hypothesis
#' that there is no mean difference between the two groups.
#' When there are three or more treatment groups,
#' this function tests the difference of mean between
#' the control group specified by the user with the `ctrl` argument
#' and each group (including the control group).
#'
#' @param outcome a string vector with outcome variables.
#' If missing, try to find out from `option("RCTtool.outcome")`.
#' @param arms a string of treatment variable.
#' If missing, try to find out from `option("RCTtool.arms")`.
#' @param ctrl a string of label of the control.
#' If missing, try to find out from `option("RCTtool.control")`.
#' @param data data object you want to use.
#'
#' @return tibble frame containing with five variables.
#' `treat` is a factor of treatments (baseline is the control).
#' `mean` and `se` is a mean and its standard error of each group.
#' `effect` is a difference of mean between each group and the control.
#' `p.value` is a p-value of t-test.
#'
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' # DGP
#' set.seed(120511)
#' n <- 1000
#' x1 <- rnorm(n); x2 <- rnorm(n)
#' d1 <- sample(c("A", "B", "C"), size = n, replace = TRUE)
#' d2 <- ifelse(d1 == "A", "control", "treat")
#' ya <- 0.2 + 0.5 * x1 + 0.01 * x2
#' yb <- 1.2 + 0.3 * x2
#' yc <- 0.2 * x1 + 0.5 * x2
#' y1 <- ifelse(d1 == "A", ya, ifelse(d1 == "B", yb, yc))
#' y2 <- ifelse(y1 > 1, 1, 0)
#' dt <- data.frame(
#'   outcome = y1,
#'   bin_outcome = y2,
#'   treat = d1,
#'   bin_treat = d2,
#'   x1,
#'   x2
#' )
#'
#' # t-test
#' set_optRCTtool(outcome ~ treat, ~ x1 + x2, dt, "A")
#' mean_diff_test(data = dt)
#' mean_diff_test(data = subset(dt, treat != "B"))
#' mean_diff_test("bin_outcome", data = dt)
#' mean_diff_test(arms = "bin_treat", ctrl = "control", data = dt)
#'
#' set_optRCTtool(bin_outcome ~ treat, data = dt, ctrl = "A")
#' mean_diff_test(data = dt)
#'
mean_diff_test <- function(outcome, arms, ctrl, data) {
  # check RCTtool.outcome option
  if (missing(outcome)) {
    outcome <- getOption("RCTtool.outcome")
    if (all(outcome == "")) stop("Not register option('RCTtool.outcome')")
  }

  # check RCTtool.arms option
  if (missing(arms)) {
    arms <- getOption("RCTtool.arms")
    if (arms == "") stop("Not register option('RCTtool.arms')")
  }

  # check RCTtool.control option
  if (missing(ctrl)) {
    ctrl <- getOption("RCTtool.control")
    if (ctrl == "") stop("Not register option('RCTtool.control')")
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
    if (!(ctrl %in% label)) stop("Control cannot find.")
  }

  # implement T-test
  t <- lapply(outcome, ttest, arms, label, ctrl, data)
  t <- dplyr::bind_rows(t)

  # convert character of treatment to factor
  level <- getOption("RCTtool.arms_level")
  if (any(!(label %in% level))) {
    treat <- label[grep(paste0("[^", ctrl, "]"), label)]
    t$treat <- factor(t$treat, levels = c(ctrl, treat))
  } else {
    t$treat <- factor(t$treat, levels = level)
  }

  # output
  class(t) <- append("t_test", class(t))
  t
}
