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
#' @export
#'
#' @examples
#' base <- list(
#'   y1 ~ d,
#'   y2 ~ d
#' )
#' genmod(base, ~ x1 + x2)
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
