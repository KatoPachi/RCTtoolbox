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
#' @param x character vector.
#' When you pass the one-sided formula in `mod` argument,
#' you must specify covariate labels.
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
#'
balance_test <- function(mod, x = NULL, data) {
  if (length(all.vars(mod)) > 1) {
    f <- ftest(mod, data)
  } else {
    if (is.null(x)) stop("Covariates must be specified")
    mod <- lapply(x, function(a) as.formula(paste0(a, "~", all.vars(mod))))
    f <- lapply(mod, ftest, data)
    f <- dplyr::bind_rows(f)
  }
  class(f) <- append(class(f), "balance_test")
  f
}