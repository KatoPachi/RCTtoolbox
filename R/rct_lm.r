#' Fitting Models by OLS
#'
#' @description This function generates multiple regression models
#' by passing multiple baseline models
#' (two-sided formula consisting of outcomes and treatments) and
#' multiple covariate models (ons-sided formula) as arguments.
#' In addition, the generated regression model is fitted
#' by the least squares method of the `lm_robust` function
#' in the {estimatr} package.
#'
#' @param outcome a string vector with outcome variables.
#' If missing, try to find out from `options("RCTtool.outcome")`.
#' @param arms a string of treatment variable.
#' If missing, try to find out from `options("RCTtool.arms")`.
#' @param ctrl a string of label of the control.
#' If missing, try to find out from `options("RCTtool.control")`.
#' @param xmod list of covariate model.
#' Covariate model is specified by one-sided formula like `~ x1 + x2`.
#' If missing, try to find out from `options("RCTtool.xmod")`.
#' @param data data which you want to use.
#' @param onlydmod logical (default is TRUE).
#' Whether to estimate a model without covariates.
#' @param subset_outcome numeric vector.
#' Specify position of outcome variables you want to use.
#' @param \dots other arguments passed in `estimatr::lm_robust`
#'
#' @return list with RCT_OLS class. `.$model` is a list of fitted models.
#' `.$res` is a list of result.
#'
#' @importFrom estimatr lm_robust
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
#' # Linear regression
#' set_optRCTtool(outcome ~ treat, ~ x1 + x2, dt, "A")
#' rct_lm(data = dt)
#' rct_lm(ctrl = "B", data = dt)
#' rct_lm(arms = "bin_treat", ctrl = "control", data = dt)
#' rct_lm("bin_outcome", data = dt, se_type = "HC1")
#' rct_lm(xmod = ~x1, data = dt)
#' set_optRCTtool(outcome + bin_outcome ~ treat, data = dt, ctrl = "A")
#' rct_lm(data = dt)
#' rct_lm(data = dt, subset_outcome = 1)
#' set_optRCTtool(outcome ~ treat, list(~x1, ~x2, ~ x1 + x2), dt, "A")
#' est <- rct_lm(data = dt)
#' summary(est)
#' clear_optRCTtool()
#'
#'
rct_lm <- function(
  outcome, arms, ctrl, xmod, data,
  onlydmod = TRUE, subset_outcome, ...
) {
  # check RCTtool.outcome option
  if (missing(outcome)) {
    outcome <- getOption("RCTtool.outcome")
    if (all(outcome == "")) stop("Not register option('RCTtool.outcome')")
  }

  if (missing(subset_outcome)) subset_outcome <- seq_len(length(outcome))

  # check RCTtool.arms option
  if (missing(arms)) {
    arms <- getOption("RCTtool.arms")
    if (arms == "") stop("Not register option('RCTtool.arms')")
  }

  # generate basic models
  basemod <- expand_basic_mod(outcome[subset_outcome], arms)

  # check RCTtool.xmod option
  if (missing(xmod)) {
    xmod <- getOption("RCTtool.xmod")
    if (!is.list(xmod)) stop("Not register option('RCTtool.xmod').")
  } else {
    if (!is.list(xmod)) xmod <- list(xmod)
  }

  # geneate model list
  modlist <- genmod(basemod, xmod, onlydmod)

  # check RCTtool.control option
  if (missing(ctrl)) {
    ctrl <- getOption("RCTtool.control")
    if (ctrl == "") stop("Not register option('RCTtool.control')")
  }

  # check RCTtool.treated option
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

  # factor of treatment
  data[[arms]] <- factor(data[[arms]], levels = c(ctrl, treat))

  # estimation
  est <- lapply(modlist, function(x) estimatr::lm_robust(x, data = data, ...))

  # output
  res <- list(model = modlist, res = est)
  class(res) <- append(class(res), "RCT_OLS")
  res
}

#' Summary Method for RCT_OLS class
#'
#' @param object object RCT_OLS class
#' @param \dots other arguments passing in summary function.
#'
#' @method summary RCT_OLS
#' @export
#'
summary.RCT_OLS <- function(object, ...) {
  lapply(object$res, summary, ...)
}
