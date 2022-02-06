#' Create List of Experimental Information
#'
#' @param d treatment vector.
#' @param ctrl character of the control.
#' If NULL, the first level of variable `d` is control.
#'
expinfo <- function(d, ctrl = NULL) {
  arms <- if (is.factor(d)) levels(d) else unique(d)
  if (is.null(ctrl)) {
    opt_set_ctrl <- getOption("RCTtool.control") != ""
    ctrl <- ifelse(opt_set_ctrl, getOption("RCTtool.control"), arms[1])
  }
  treat <- arms[grep(paste0("[^", ctrl, "]"), arms)]
  level <- c(ctrl, treat)
  list(arms = arms, ctrl = ctrl, treat = treat, level = level)
}
