#' Manipulation Global Options for RCTtoolbox
#'
#' @param basicmod two-sided formula with
#' outcome variable on lhs and treatment variable on rhs
#' @param xmod (list of) one-sided formulas with
#' covariates used in regression analysis and balance test on the rhs.
#' @param data data you want to use
#' @param ctrl string. Label of the control.
#' If missing, the first level of factor of treatment variable
#' is the control.
#' @param \dots Specify `option_name = option`
#'
#' @export
#' @examples
#' \dontrun{
#' set.seed(120511)
#' ex <- data.frame(treat = sample(c("A", "B"), size = 100, replace = TRUE))
#' ex$outcome <- ifelse(ex$treat == "A", rnorm(1, mean = 2), rnorm(1))
#'
#' set_optRCTtool(
#'   outcome ~ treat,
#'   data = ex,
#'   ctrl = "B",
#'   RCTtool.table_fontsize = 8
#' )
#' }
#'
set_optRCTtool <- function(basicmod, xmod, data, ctrl, ...) {
  # collect arguments
  args <- list(...)

  # parse basicmod and add arguments
  if (!missing(basicmod)) {
    # parse basicmod and add arguments
    parse_basicmod <- parse_model(basicmod)
    args$RCTtool.outcome <- parse_basicmod$lhs
    args$RCTtool.arms <- parse_basicmod$rhs

    # experimental information and arguments
    d <- data[[parse_basicmod$rhs]]
    d <- if (!is.factor(d)) factor(d)
    arms <- levels(d)
    ctrl <- ifelse(missing(ctrl), arms[1], ctrl)
    treated <- arms[grep(paste0("[^", ctrl, "]"), arms)]
    level <- c(ctrl, treated)
    args$RCTtool.arms_label <- arms
    args$RCTtool.arms_level <- level
    args$RCTtool.control <- ctrl
    args$RCTtool.treated <- treated
  }

  if (!missing(xmod)) {
    # parse xmod and add arguments
    xmod <- if (!is.list(xmod)) list(xmod) else xmod
    xlist <- unique(unlist(lapply(xmod, all.vars)))
    args$RCTtool.xmod <- xmod
    args$RCTtool.xlist <- xlist
  }

  # collect current option name
  opt <- names(options())

  # check whether options specified in arguments register
  ok <- names(args) %in% opt

  # register options if ok
  options(args[ok])

  # show registered options
  options()[grep("RCTtool.", names(options()))]
}

#' Clear Global Options for RCTtoolbox
#'
#' @export
#' @examples
#' \dontrun{
#' clear_optRCTtool()
#' options()[grep("RCTtool", names(options()))]
#' }
#'
clear_optRCTtool <- function() {
  # default option list
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
  # collect current option name
  opt <- names(options())
  # check whether options specified in arguments register
  ok <- names(opt_pkg) %in% opt
  # register options if ok
  options(opt_pkg[ok])
}