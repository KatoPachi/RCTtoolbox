#' Parse Model
#'
#' @param mod formula
#'
#' @importFrom stats terms
#'
#'
parse_model <- function(mod) {
  full_var <- all.vars(mod)
  rhs <- attr(terms(mod), "term.labels")
  lhs <- full_var[!(full_var %in% rhs)]
  list(lhs = lhs, rhs = rhs)
}

#' Generate expanded version of basic model
#'
#' @param y a string vector with outcome variables
#' @param d a string of treatment variable
#'
#' @importFrom stats as.formula
#'
#'
expand_basic_mod <- function(y, d) {
  lapply(y, function(x) as.formula(paste0(x, "~", d)))
}

#' Generate List of Regression Models
#'
#' @param basemod list of baseline model.
#' Baseline model is specified by `y ~ d`
#' where `y` is outcome, and `d` is treatments.
#' @param xmod list of covariate model.
#' Covariate model is specified by one-sided formula like `~ x1 + x2`.
#' @param include_onlyd logical.
#' Whether to estimate a model without covariates.
#'
#' @importFrom fixest xpd
#' @importFrom stats update
#'
#'
genmod <- function(basemod, xmod = NULL, include_onlyd = TRUE) {
  # check list
  basemod <- if (!is.list(basemod)) list(basemod) else basemod
  xmod <- if (!is.null(xmod) & !is.list(xmod)) list(xmod) else xmod
  # length of lists
  num_base <- length(basemod)
  num_xmod <- length(xmod)
  # generate list of models
  modlist <- vector("list", num_base * (num_xmod + include_onlyd))
  for (i in seq_len(num_base)) {
    for (j in seq_len(num_xmod + include_onlyd)) {
      pos <- j + (num_xmod + include_onlyd) * (i - 1)
      if (j == 1 & include_onlyd) {
        modlist[[pos]] <- basemod[[i]]
      } else {
        mod <- update(basemod[[i]], ~ . + ..addx)
        modlist[[pos]] <- fixest::xpd(mod, ..addx = xmod[[j - include_onlyd]])
      }
    }
  }
  # output
  modlist
}
