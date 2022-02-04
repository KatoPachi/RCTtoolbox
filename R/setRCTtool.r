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
