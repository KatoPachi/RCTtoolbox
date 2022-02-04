#' Useful Wrapper Functions for RCTs
#'
#' @section Package Options:
#'
#' RCTtoolbox provides the following [options()]
#' to facilitate consistent analysis:
#' \itemize{
#'   \item `RCTtool.outcome`: String vector with outcome variables
#'     such as `c("y1", "y2")`
#'   \item `RCTtool.treatment`: One-sided formula with
#'     treatment variables on the right-hand side of the formula
#'     (e.g. `~ treatment`).
#'   \item `RCTtool.control`: String of control arm.
#'   \item `RCTtool.xmod`: A list of one-sided formulas with covariates
#'     on the right-hand side of the equation.
#'     See [rct_lm()] in detail.
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
    RCTtool.treatment = "",
    RCTtool.control = "",
    RCTtool.xmod = "",
    RCTtool.plot_family = "",
    RCTtool.table_fontsize = 15
  )
  toset <- !(names(opt_pkg) %in% names(opt))
  if (any(toset)) options(opt_pkg[toset])

  invisible()
}
