#' Useful Wrapper Functions for RCTs
#'
#' @section Package Options:
#'
#' RCTtoolbox provides the following [options()] 
#' to facilitate consistent analysis:
#' \itemize{
#'   \item `RCTtoolbox.table_output`: Table output package
#'     ("kableExtra", "flextable")
#'   \item `RCTtoolbox.table_fontsize`: The font size in the table
#'     output by {kableExtra} and {flextable} used through {modelsummary}.
#'   \item `RCTtoolbox.plot_family`: The name of the font family
#'     used to output the figure with {ggplot2}.
#' }
#'
#' @docType package
#' @name RCTtoolbox
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  opt <- options()
  opt_pkg <- list(
    RCTtoolbox.table_output = "kableExtra",
    RCTtoolbox.plot_family = "",
    RCTtoolbox.table_fontsize = 15
  )
  toset <- !(names(opt_pkg) %in% names(opt))
  if (any(toset)) options(opt_pkg[toset])

  invisible()
}
