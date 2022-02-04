#' F-test of Overall Siginificance in Linear Regression
#'
#' @param mod two-sided formula.
#' The covariate is specified in left-hand side of formula.
#' The treatment is specified in right-hand side of formula.
#' @param data data which you want to use.
#'
#' @importFrom stats complete.cases
#' @importFrom stats anova
#' @importFrom stats lm
#'
#'
ftest <- function(mod, data) {
  # variable labels
  xvar <- all.vars(mod)[1]
  dvar <- all.vars(mod)[2]
  # using data
  usedt <- data[complete.cases(data), ]
  # treatment variable and label of arms
  d <- usedt[[dvar]]
  if (!is.factor(d)) d <- factor(d)
  arms <- levels(d)
  # claculate mean value in each arm
  mu <- lapply(arms, function(x) mean(usedt[d == x, ][[xvar]]))
  # implement F-test
  f <- anova(lm(mod, usedt))[["Pr(>F)"]][1]
  # outcome
  data.frame(
    x = xvar,
    item = c(arms, "P-value (F-test)"),
    val = c(unlist(mu), f)
  )
}

#' Blance Test by F-test of Overall Siginificance in Linear Regression
#'
#' @description This function performs a balance test of the treatment
#' with multiple covariates.
#' The balance test calculates the mean of each group
#' and the F-test by linear regression analysis.
#' User specifies the treatment variable with a one-sided formula
#' and the covariate used for the balance test with a string vector.
#'
#' @param mod formula.
#' You can pass two-sided or one-sided formula.
#' When two-sided formula is passed,
#' the left-hand side of formula is covariate,
#' and the right-hand side of formula is treatment.
#' When one-sided formula such as `~ d` is passed,
#' the right-hand side of formula is treatment.
#' If missing,
#' try to find out the one-sided formula from `options("RCTtool.treatment")`.
#' @param x character vector.
#' When you pass the one-sided formula in `mod` argument,
#' you must specify covariate labels.
#' If NULL, try to find out from `options("RCTtool.xmod")`.
#' @param data data which you want to use.
#'
#' @return A data frame (`balance_test`` class) with 3 variables:
#' \describe{
#'   \item{x}{character of covariates used for balance test}
#'   \item{item}{character of treatment arms and F-test}
#'   \item{val}{outcome mean of each arm and p-value of F-test}
#' }
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
#' d <- sample(c("A", "B", "C"), size = n, replace = TRUE)
#'
#' ya <- 0.2 + 0.5 * x1 + 0.01 * x2
#' yb <- 1.2 + 0.3 * x2
#' yc <- -1 - 0.2 * x1 + 0.5 * x2
#' y <- ifelse(d == "A", ya, ifelse(d == "B", yb, yc))
#' dt <- data.frame(y, d, x1, x2)
#'
#' # Balance test with two or more covariates
#' balance_test(~d, c("x1", "x2"), dt)
#'
#' # Balance test with only one covariates
#' balance_test(x1 ~ d, data = dt)
#'
#' # If you pre-register models through options(),
#' # balance test function is simply implemented.
#' setRCTtool(
#'   RCTtool.treatment = ~ d,
#'   RCTtool.xmod = list(~ x1, ~ x2, ~ x1 + x2)
#' )
#'
#' balance_test(data = dt)
#' clearRCTtool()
#'
#'
balance_test <- function(mod, x = NULL, data) {
  # check options
  opt_treatment <- getOption("RCTtool.treatment") != ""
  opt_xmod <- any(getOption("RCTtool.xmod") != "")

  # add mod if empty argument and option is registered
  if (missing(mod)) {
    if (opt_treatment) {
      dmod <- getOption("RCTtool.treatment")
    } else {
      stop(paste0(
        "one-sided or two-sided formula must be specified in mod argument.",
        "Another way is to register options('RCTtool.treatment')."
      ))
    }
  } else {
    if (length(all.vars(mod)) == 1) {
      dmod <- mod
    } else {
      mod <- if (!is.list(mod)) list(mod) else mod
      dmod <- NULL
    }
  }

  # If dmod is not NULL, create list of two-sided formulas
  if (!is.null(dmod)) {
    if (is.null(x)) {
      if (opt_xmod) {
        xmod <- getOption("RCTtool.xmod")
        x <- lapply(xmod, all.vars)
        x <- unique(unlist(x))
        mod <- lapply(x, function(a) as.formula(paste0(a, "~", all.vars(dmod))))
      } else {
        stop("Covariate vector must be specified.")
      }
    } else {
      mod <- lapply(x, function(a) as.formula(paste0(a, "~", all.vars(dmod))))
    }
  }

  # implement F-test
  f <- lapply(mod, ftest, data)
  f <- dplyr::bind_rows(f)

  # output
  class(f) <- append(class(f), "balance_test")
  f
}