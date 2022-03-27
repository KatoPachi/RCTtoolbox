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
#'
#' @export
#'
#'
ttest_power <- function(n0,
                        n1,
                        d,
                        alpha,
                        power) {
  # extract arguments
  comp <- as.list(match.call())[-1]
  full_args <- c("n0", "n1", "d", "alpha", "power")
  miss_arg <- full_args[!(full_args %in% names(comp))]

  # check length(comp) == 4
  if (length(comp) != 4) abort_empty_num(5 - length(comp), 1)

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
  data.frame(comp[full_args])
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
#' @param treat A string of treatment variables
#' @param data data which you want to use.
#' @param treat_levels original level of treatment arms.
#' The first level is control arm.
#' @param treat_labels label of treatment arms corresponding to original level.
#' @param ctrl new control arm.
#' @param subset subset condition.
#' @param sd numeric (default is 1).
#' The standard deviation used when calculating the mean difference.
#' @param \dots some arguments passing in `ttest_power`.
#' You can pass `d` (effect size), `alpha` (significant level), and
#' `power` (power).
#'
#' @importFrom dplyr bind_rows
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats formula
#' @importFrom rlang enquo
#' @importFrom rlang eval_tidy
#'
#' @examples
#' \dontrun{
#' data(RubellaNudge)
#' rct <- create_RCTtoolbox(
#'   atest + avacc ~ treat,
#'   data = RubellaNudge,
#'   treat_levels = LETTERS[1:7]
#' )
#'
#' rct$power(alpha = 0.05, power = 0.8, sd = 0.2)$result
#' }
#'
#'
power_calculation <- function(treat = NULL,
                              data = NULL,
                              treat_levels = NULL,
                              treat_labels = NULL,
                              ctrl = NULL,
                              subset = NULL,
                              sd = 1,
                              ...) {
  # clean data
  order_d <- reorder_arms(treat_levels, treat_labels, ctrl)
  model <- formula(paste("~", treat))
  clean <- clean_RCTdata(
    model,
    data = data,
    treat_levels = order_d$levels,
    treat_labels = order_d$labels,
    subset = enexpr(subset)
  )
  use <- clean$design[, -1]

  # perform power analysis
  pass_ttest_power <- list(...)
  pass_ttest_power$n0 <- sum(1 - rowSums(use))
  pwr <- apply(use, MARGIN = 2, function(n) {
    pass_ttest_power$n1 <- sum(n)
    do.call("ttest_power", pass_ttest_power)
  })

  # output dataframe
  out <- bind_rows(pwr)
  out$arms <- gsub(treat, "", names(pwr))

  # calculate unstandarized effect
  out$sd <- sd
  out$diff_mean <- out$d * out$sd

  # add row containing control information
  out <- bind_rows(
    data.frame(
      n0 = pass_ttest_power$n0,
      n1 = pass_ttest_power$n0,
      arms = order_d$labels[1]
    ),
    out
  )

  # convert character of treatment to factor
  out$arms <- factor(out$arms, order_d$labels)

  # output
  out
}
