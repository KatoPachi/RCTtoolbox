#' Manipulation Global Options for RCTtoolbox
#'
#' @param \dots Specify `option_name = option`
#' @param clear logical (default is FALSE).
#' Whether to clear global options.
#'
#' @export
#' @examples
#' \dontrun{
#' optRCTtool(
#'   RCTtool.outcome = c("y1", "y2"),
#'   RCTtool.arms = ~ treatment
#' )
#' getOption("RCTtool.outcome")
#' getOption("RCTtool.arms")
#' getOption("RCTtool.nothing") # not register by .onLoad
#' optRCTtool(clear = TRUE)
#' }
#'
optRCTtool <- function(..., clear = FALSE) {
  # collect arguments
  if (!clear) {
    args <- list(...)
  } else {
    args <- list(
      RCTtool.outcome = "",
      RCTtool.arms = "",
      RCTtool.control = "",
      RCTtool.xmod = "",
      RCTtool.plot_family = "",
      RCTtool.table_fontsize = 15
    )
  }
  # collect current option name
  opt <- names(options())
  # check whether options specified in arguments register
  ok <- names(args) %in% opt
  # register options if ok
  options(args[ok])
}