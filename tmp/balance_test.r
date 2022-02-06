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
#' @param mod formula such as `covariate1 + covariate2 ~ treatment`.
#' If missing, try to find out experimental arms and covariates
#' from global options "RCTtool.arms" and "RCTtool.xmod"
#' If formula with rhs only,
#' try to find out covariates from global options "RCTtool.xmod".
#' If formula with lhs only,
#' try to find out experimental arms
#' from global options "RCTtool.arms"
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
#' # use mod argument
#' balance_test(x1 ~ d, data = dt)
#' balance_test(x1 + x2 ~ d, data = dt)
#'
#' # use global options
#' optRCTtool(
#'   RCTtool.arms = ~d,
#'   RCTtool.xmod = list(~x1, ~x2, ~ x1 + x2)
#' )
#' balance_test(data = dt)
#' balance_test(x1 ~ 0, data = dt)
#' optRCTtool(clear = TRUE)
#'
#'
balance_test <- function(mod, data) {
  # If mod is missing, search global options
  if (missing(mod)) {
    # check options
    opt_set_arms <- getOption("RCTtool.arms") != ""
    opt_set_xmod <- any(getOption("RCTtool.xmod") != "")

    # if register, extract from global options
    if (opt_set_arms & opt_set_xmod) {
      # rhs
      rhs <- all.vars(getOption("RCTtool.arms"))

      #rhs
      x <- lapply(getOption("RCTtool.xmod"), all.vars)
      lhs <- unique(unlist(x))
    } else {
      stop(paste0(
        "Due to missing mod,",
        "try to search options('RCTtool.arms') and options('RCTtool.xmod').",
        "Since you don't register it, I cannot construct model.",
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

    # check lhs
    if (length(parts$lhs) == 0) {
      opt_set_xmod <- any(getOption("RCTtool.xmod") != "")
      if (opt_set_xmod) {
        x <- lapply(getOption("RCTtool.xmod"), all.vars)
        parts$lhs <- unique(unlist(x))
      } else {
        stop("Cannot find covariates in both mod and options.")
      }
    }

    lhs <- parts$lhs
    rhs <- parts$rhs
  }

  # implement F-test
  mod <- lapply(lhs, function(a) as.formula(paste(a, "~", rhs)))
  f <- lapply(mod, ftest, data)
  f <- dplyr::bind_rows(f)

  # output
  class(f) <- append(class(f), "balance_test")
  f
}