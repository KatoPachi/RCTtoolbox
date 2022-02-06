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
#' @param mod formula. Specify one-sided formula `~ treatment`.
#' If missing, find formula from `options("RCTtool.treatment")`.
#' @param data data which you want to use.
#' @param ctrl character. Name of the control.
#' If NULL (default), first try to find out from `options("RCTtool.control")`.
#' If cannot find global options,
#' the first level of the variable `d` is control.
#' @param d numeric. Effect size.
#' @param alpha numeric. Significant level.
#' @param power numeric. Power.
#' @param std_dev numeric.
#' The standard deviation used when calculating the unstandardized effect.
#' If `std_dev` is numeric, then specified value is used to calculate.
#' If `std_dev` is string, calculate standard deviation of specified variable
#' and use it to calculate the unstandardized effect.
#' Default is 0.5.
#'
#' @return tibble (and data.frame) with class "power_analysis"
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
#' # power analysis with full argument
#' power_analysis(
#'   ~d, dt, ctrl = "B", alpha = 0.05, power = 0.8, std_dev = "y"
#' ) #known variance
#'
#' power_analysis(
#'   ~d, dt, ctrl = "B", alpha = 0.05, power = 0.8, std_dev = 1
#' ) #known variance
#'
#' # Use global options
#' optRCTtool(RCTtool.arms = ~ d, RCTtool.control = "A")
#' power_analysis(data = dt, alpha = 0.05, power = 0.8, std_dev = 1)
#' power_analysis(data = dt, ctrl = "B", alpha = 0.05, power = 0.8, std_dev = 1)
#' optRCTtool(clear = TRUE)
#'
#'
power_analysis <- function(
  mod, data, ctrl = NULL,
  d = NULL, alpha = NULL, power = NULL,
  std_dev = 0.5
) {
  # if mod is missing, search global options
  if (missing(mod)) {
    opt_set_arms <- getOption("RCTtool.arms") != ""
    if (opt_set_arms) {
      dlab <- all.vars(getOption("RCTtool.arms"))
    } else {
      stop(paste0(
        "Due to missing mod,",
        "try to search options('RCTtool.arms'), but not register.",
        "Specify models in the mod argument or register global options."
      ))
    }
  } else {
    parts <- parse_model(mod)

    # check rhs
    if (length(parts$rhs) > 1) {
      stop("Only one treatment variable in the rhs")
    } else if (length(parts$rhs) == 0) {
      opt_set_arms <- getOption("RCTtool.arms") != ""
      if (opt_set_arms) {
        parts$rhs <- all.vars(getOption("RCTtool.arms"))
      } else {
        stop("Cannot find treatment arms in both mod and options.")
      }
    }

    # if length(lhs) > 0, warning
    if (length(parts$lhs) > 0) {
      warning(paste0("Ignore LHS"))
    }

    dlab <- parts$rhs
  }

  # extract treatment vector
  dv <- data[[dlab]]

  # register experimental information
  flag_temp_set <- any(
    getOption("RCTtool.arms_level") == "" |
    getOption("RCTtool.control") == "" |
    getOption("RCTtool.treated") == ""
  )
  if (flag_temp_set) {
    if (getOption("RCTtool.control") != "") {
      optRCTtool_expinfo(dv, ctrl = getOption("RCTtool.control"))
    } else {
      optRCTtool_expinfo(dv, ctrl = ctrl)
    }
  }
  ctrl <- getOption("RCTtool.control")
  treat <- getOption("RCTtool.treated")

  # calculate number of observations
  n0 <- length(dv[dv == ctrl])
  n1 <- lapply(treat, function(x) length(dv[dv == x]))

  # perform power analysis
  pwr <- lapply(
    n1, ttest_power,
    n0 = n0, d = d, alpha = alpha, power = power
  )

  # output dataframe
  out <- dplyr::bind_rows(pwr)
  out$treat <- treat

  # calculate unstandarized effect
  if (!is.numeric(std_dev)) {
    ylab <- std_dev
    v0 <- var(data[data[[dlab]] == ctrl, ][[ylab]], na.rm = TRUE)
    y1 <- lapply(treat, function(x) data[data[[dlab]] == x, ][[ylab]])
    v1 <- lapply(y1, var, na.rm = TRUE)
    std_dev <- sqrt((unlist(v1) + v0) / 2)
  }
  out$std_dev <- std_dev
  out$unstd_effect <- out$d * out$std_dev

  # add row containing control information
  out <- dplyr::bind_rows(
    out,
    data.frame(n0 = n0, n1 = n0, treat = ctrl)
  )
  # convert character of treatment to factor
  out$treat <- factor(out$treat, levels = getOption("RCTtool.arms_level"))

  # clear temporal options
  optRCTtool(
    RCTtool.arms_label = "",
    RCTtool.arms_level = "",
    RCTtool.control = "",
    RCTtool.treated = ""
  )

  # output
  class(out) <- append("power_analysis", class(out))
  out
}