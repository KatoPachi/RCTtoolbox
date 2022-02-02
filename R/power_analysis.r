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
ttest_power <- function(
  n0 = NULL, n1 = NULL,
  d = NULL, alpha = NULL, power = NULL
) {
  # make output dataframe
  out <- list(
    n0 = n0,
    n1 = n1,
    d = d,
    alpha = alpha,
    power = power
  )
  # check two or more NULL
  if (length(unlist(out)) < 4 | length(unlist(out)) == 5) {
    stop("One NULL argument must be needed.")
  }
  # define function which returns specified power - calculated power
  power_func <- function(n0, n1, d, alpha, power) {
    delta <- d * sqrt(1 / n1 + 1 / n0) ^ (-1)
    df <- n1 + n0 - 2
    critical <- qt(alpha / 2, df = df)
    power - (pt(-critical, df = df, ncp = delta, lower.tail = FALSE) +
      pt(critical, df = df, ncp = delta, lower.tail = TRUE))
  }
  # find missing value satisfying specified power - claculated power = 0
  if (is.null(n0)) {
    out$n0 <- uniroot(
      power_func, c(0, 1e+09),
      n1 = n1, d = d, alpha = alpha, power = power
    )$root
  }
  if (is.null(n1)) {
    out$n1 <- uniroot(
      power_func, c(0, 1e+09),
      n0 = n0, d = d, alpha = alpha, power = power
    )$root
  }
  if (is.null(d)) {
    out$d <- uniroot(
      power_func, c(0, 10),
      n0 = n0, n1 = n1, alpha = alpha, power = power
    )$root
  }
  if (is.null(alpha)) {
    out$alpha <- uniroot(
      power_func, c(0, 1),
      n0 = n0, n1 = n1, d = d, power = power
    )$root
  }
  if (is.null(power)) {
    out$power <- uniroot(
      power_func, c(0, 1),
      n0 = n0, n1 = n1, d = d, alpha = alpha
    )$root
  }
  unlist(out)
}

#'
#' Power Analysis of Two Sided T-Test for RCT with Multiple Arms
#'
#' @description This function performs a two-sided t-test power analysis
#' given the sample sizes of the treatment and control groups.
#' Calculate the value that is NULL among the effect size, significance level,
#' and power.
#' For the effect size, the non-standardized effect
#' (absolute value of the difference between the average outcomes)
#' is also calculated.
#' If the outcome variable is specified in the `mod` argument,
#' the square root of the mean of the outcome variances of the two group
#' is the standard deviation.
#' If no outcome variable is specified,
#' the standard deviation specified by the std_dev argument is used.
#'
#' @param mod formula. You can pass an one-sided formula `~ d` or
#' two-sided formula `y ~ d` where
#' `y` is outcome and `d` is treatments.
#' @param data data which you want to use.
#' @param base character. Name of the control.
#' If NULL (default), the first level of the variable `d` is control.
#' @param d numeric. Effect size.
#' @param alpha numeric. Significant level.
#' @param power numeric. Power.
#' @param unstd_effect logical.
#' Whether to calculate non-standarized effect.
#' Default is TRUE.
#' @param std_dev numeric.
#' The standard deviation used when calculating the destandardized effect.
#' If an outcome variable is specified,
#' this argument is ignored and the actual standard deviation is used.
#' Default is 0.5.
#'
#' @return tibble (and data.frame) with class "power.analysis"
#'
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' # DGP
#' set.seed(120511)
#' n <- 1000
#' x1 <- rnorm(n); x2 <- rnorm(n)
#' d <- sample(c("A", "B", "C"), size = n, replace = TRUE)
#'
#' ya <- 0.2 + 0.5 * x1 + 0.01 * x2
#' yb <- 1.2 + 0.3 * x2
#' yc <- -1 - 0.2 * x1 + 0.5 * x2
#' y <- ifelse(d == "A", ya, ifelse(d == "B", yb, yc))
#' dt <- data.frame(y, d, x1, x2)
#'
#' # power analysis with variance assumption
#' power_analysis(~d, dt, alpha = 0.05, power = 0.8, std_dev = 0.2)
#'
#' # power analysis with known variance
#' power_analysis(y ~ d, dt, alpha = 0.05, power = 0.8)
#'
#'
power_analysis <- function(
  mod, data, base = NULL,
  d = NULL, alpha = NULL, power = NULL,
  unstd_effect = TRUE, std_dev = 0.5
) {
  # extract treatment and outcome labels
  if (length(all.vars(mod)) > 1) {
    ylab <- all.vars(mod)[1]
    dlab <- all.vars(mod)[2]
  } else {
    ylab <- NULL
    dlab <- all.vars(mod)[1]
  }
  # check factor of treatment variables
  dvar <- data[[dlab]]
  if (!is.factor(dvar)) dvar <- factor(dvar)
  # treated levels
  arms <- levels(dvar)
  if (is.null(base)) base <- arms[1]
  treat <- arms[grep(paste0("[^", base, "]"), arms)]
  # calculate n0
  n0 <- length(dvar[dvar == base])
  # calculate n1
  n1 <- lapply(treat, function(x) length(dvar[dvar == x]))
  # perform power analysis
  pwr <- lapply(
    n1, ttest_power,
    n0 = n0, d = d, alpha = alpha, power = power
  )
  # output dataframe
  out <- dplyr::bind_rows(pwr)
  out$treat <- treat
  # calculate unstandarized effect
  if (unstd_effect) {
    if (is.null(ylab)) {
      std_dev <- std_dev
    } else {
      v0 <- var(data[data[[dlab]] == base, ][[ylab]], na.rm = TRUE)
      y1 <- lapply(treat, function(x) data[data[[dlab]] == x, ][[ylab]])
      v1 <- lapply(y1, var, na.rm = TRUE)
      std_dev <- sqrt((unlist(v1) + v0) / 2)
    }
    out$std_dev <- std_dev
    out$unstd_effect <- out$d * out$std_dev
  }
  # add row containg control information
  out <- dplyr::bind_rows(
    out,
    data.frame(n0 = n0, n1 = n0, treat = base)
  )
  # convert character of treatment to factor
  out$treat <- factor(
    out$treat,
    levels = c(base, treat)
  )
  # output
  class(out) <- append("power_analysis", class(out))
  out
}