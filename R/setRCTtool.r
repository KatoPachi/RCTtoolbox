#' Set global options for RCTtoolbox
#'
#' @param \dots Specify `option_name = option`
#'
#' @export
#' @examples
#' \dontrun{
#' setRCTtool(
#'   RCTtool.outcome = c("y1", "y2"),
#'   RCTtool.treatment = ~ d,
#'   RCTtool.nothing = "" # not register by .onLoad
#' )
#' getOption("RCTtool.outcome")
#' getOption("RCTtool.treatment")
#' getOption("RCTtool.nothing") # not register by .onLoad
#' }
#'
setRCTtool <- function(...) {
  # collect arguments
  args <- list(...)
  # collect current option name
  opt <- names(options())
  # check whether options specified in arguments register
  ok <- names(args) %in% opt
  # register options if ok
  options(args[ok])
}

#' Clear global options for RCTtoolbox
#'
#' @export
#'
clearRCTtool <- function () {
  # default value of options
  opt_pkg <- list(
    RCTtool.outcome = "",
    RCTtool.treatment = "",
    RCTtool.xmod = "",
    RCTtool.plot_family = "",
    RCTtool.table_fontsize = 15
  )
  # check whether options specified in arguments register
  opt <- names(options())
  ok <- names(opt_pkg) %in% opt
  # register options if ok
  options(opt_pkg[ok])
}