#' Power Analysis for Two Sided T-Test
#'
#' @description This is a function that performs
#' a power analysis of the two-sided t-test.
#' Given the effect size, significance level, and power,
#' the required sample size can be calculated
#' (although the control or treatment sample size must be given).
#' Also, given the sample size of the two groups, effect size,
#' and significance level, the power can be calculated.
#' Given the sample size of two groups, significance level,
#' and power of the two groups, the effect size can be calculated.
#' Given the sample size of two groups, effect size,
#' and power of the two groups, the significance level can be calculated.
#'
#' @param n0 numeric. Number of observations in the control.
#' @param n1 numeric. Number of observations in the treatment.
#' @param d numeric. Effect size.
#' @param alpha numeric. Statistically significance
#' (probability of Type I error).
#' @param power numeric. Power (1 - probability of Type II error).
#'
#' @importFrom stats pt
#' @importFrom stats qt
#' @importFrom stats uniroot
#' @export
#'
#' @examples
#' ttest_power(n0 = 100, n1 = 100, alpha = 0.05, power = 0.8)
#'
#'
ttest_power <- function(n0, n1, d, alpha, power) {
  # extract arguments
  comp <- as.list(match.call())[-1]
  full_args <- c("n0", "n1", "d", "alpha", "power")
  miss_arg <- full_args[!(full_args %in% names(comp))]
  message(paste0("Argument '", miss_arg, "' is missing"))

  # check length(comp) == 4
  if (length(comp) != 4) stop("One missing argument must be needed.")

  # define function which returns specified power - calculated power
  power_func <- function(n0, n1, d, alpha, power) {
    delta <- d * sqrt(1 / n1 + 1 / n0) ^ (-1)
    df <- n1 + n0 - 2
    critical <- qt(alpha / 2, df = df)
    power - (pt(-critical, df = df, ncp = delta, lower.tail = FALSE) +
      pt(critical, df = df, ncp = delta, lower.tail = TRUE))
  }
  comp$f <- power_func

  # set interval
  if (missing(n0) | missing(n1)) {
    comp$interval <- c(0, 1e+09)
  } else if (missing(d)) {
    comp$interval <- c(0, 10)
  } else {
    comp$interval <- c(0, 1)
  }
  message(paste0(
    "Find value between [", comp$interval[1], ",", comp$interval[2], "]."
  ))

  # find missing value satisfying specified power - claculated power = 0
  comp[[miss_arg]] <- do.call("uniroot", comp)$root

  # output
  unlist(comp[full_args])
}

#'
#' Power Analysis of Two Sided T-Test for RCT with Multiple Arms
#'
#' @description This function performs a two-sided t-test power analysis
#' given the sample sizes of the treatment and control groups.
#' Calculate the missing value among the effect size, significance level,
#' and power.
#' For the effect size, the non-standardized effect
#' (absolute value of the difference between the average outcomes)
#' is also calculated.
#' If some variable is specified in the `std_dev` argument,
#' the square root of the mean of variances of specified variable
#' of the two group is the standard deviation.
#'
#' @param arms A string of treatment variables
#' If missing, try to find from `options("RCTtool.arms")`.
#' @param ctrl a string of the control.
#' If missing, try to find from `options("RCTtool.control")`.
#' @param data data which you want to use.
#' @param std_dev numeric (default is 1).
#' The standard deviation used when calculating the mean difference.
#' If `std_dev` is numeric, then specified value is used to calculate.
#' If `std_dev` is string, calculate standard deviation of specified variable
#' and use it to calculate.
#' @param \dots some arguments passing in `ttest_power`.
#' You can pass `d` (effect size), `alpha` (significant level), and
#' `power` (power).
#'
#' @return tibble (and data.frame) with class "power_analysis"
#'
#' @importFrom dplyr bind_rows
#' @importFrom stats var
#' @export
#' @examples
#' set.seed(120511)
#' d1 <- sample(LETTERS[1:5], size = 1000, replace = TRUE)
#' d2 <- ifelse(d1 %in% LETTERS[1:3], "Control", "Treat")
#' ex <- data.frame(treat = d1, treat2 = d2)
#' set_optRCTtool(~treat, data = ex, ctrl = "A")
#' power_analysis(data = ex, alpha = 0.05, power = 0.8)
#' power_analysis("treat2", "Control", ex, alpha = 0.05, power = 0.8)
#' clear_optRCTtool()
#'
#'
power_analysis <- function(arms, ctrl, data, std_dev = 1, ...) {
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

  #check RCTtool.treated option
  if (missing(arms)) {
    treat <- getOption("RCTtool.treated")
    if (all(treat == "")) {
      warning("You should register option('RCTtool.treated')")
      dv <- data[[arms]]
      if (!is.factor(dv)) dv <- factor(dv)
      lev <- levels(dv)
      treat <- lev[grep(paste0("[^", ctrl, "]"), lev)]
    }
  } else {
    dv <- data[[arms]]
    if (!is.factor(dv)) dv <- factor(dv)
    lev <- levels(dv)
    if (!(ctrl %in% lev)) stop("Control cannot find.")
    treat <- lev[grep(paste0("[^", ctrl, "]"), lev)]
  }

  # calculate number of observations
  n0 <- length(dv[dv == ctrl])
  n1 <- lapply(treat, function(x) length(dv[dv == x]))

  # perform power analysis
  pass_ttest_power <- list(...)
  pass_ttest_power$n0 <- n0
  pwr <- lapply(n1, function(x) {
    pass_ttest_power$n1 <- x
    do.call("ttest_power", pass_ttest_power)
  })

  # output dataframe
  out <- dplyr::bind_rows(pwr)
  out$treat <- treat

  # calculate unstandarized effect
  if (!is.numeric(std_dev)) {
    ylab <- std_dev
    v0 <- var(data[data[[arms]] == ctrl, ][[ylab]], na.rm = TRUE)
    y1 <- lapply(treat, function(x) data[data[[arms]] == x, ][[ylab]])
    v1 <- lapply(y1, var, na.rm = TRUE)
    std_dev <- sqrt((unlist(v1) + v0) / 2)
  }
  out$std_dev <- std_dev
  out$diff_mean <- out$d * out$std_dev

  # add row containing control information
  out <- dplyr::bind_rows(
    out,
    data.frame(n0 = n0, n1 = n0, treat = ctrl)
  )

  # convert character of treatment to factor
  out$treat <- factor(out$treat, levels = c(ctrl, treat))

  # output
  class(out) <- append("power_analysis", class(out))
  out
}
