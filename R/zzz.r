#' Useful Wrapper Functions for RCTs
#'
#' @section Package Options:
#'
#' RCTtoolbox provides the following [options()]
#' to facilitate consistent analysis:
#' \itemize{
#'   \item `RCTtool.outcome`: a string vector with outcome variables.
#'   \item `RCTtool.arms`: a string of treatment variables.
#'   \item `RCTtool.arms_label`: a string vector with treatment labels.
#'   \item `RCTtool.arms_level`: a string vector with
#'     levels for factor of treatment variables.
#'   \item `RCTtool.control`: String of control arm.
#'   \item `RCTtool.treated`: String of treated arms.
#'   \item `RCTtool.xmod`: A list of one-sided formulas with covariates
#'     on the right-hand side of the equation.
#'     See `rct_lm()` in detail.
#'   \item `RCTtool.xlist`: String vector with covariates.
#'   \item `RCTtool.plot_family`: The name of the font family
#'     used to output the figure with {ggplot2}.
#'   \item `RCTtool.table_fontsize`: The font size in the table
#'     output by {kableExtra} and {flextable} used through {modelsummary}.
#' }
#'
#' @docType package
#' @name RCTtoolbox
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  opt <- options()
  opt_pkg <- list(
    RCTtool.outcome = "",
    RCTtool.arms = "",
    RCTtool.arms_label = "",
    RCTtool.arms_level = "",
    RCTtool.control = "",
    RCTtool.treated = "",
    RCTtool.xmod = "",
    RCTtool.xlist = "",
    RCTtool.plot_family = "",
    RCTtool.table_fontsize = 15
  )
  toset <- !(names(opt_pkg) %in% names(opt))
  if (any(toset)) options(opt_pkg[toset])

  invisible()
}
