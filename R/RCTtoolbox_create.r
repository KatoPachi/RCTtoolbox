#' Create New Toolbox
#'
#' Create an R6 class object called "RCT toolbox"
#' for consistent RCT data analysis.
#' The created object has method functions
#' such as t-test and regression analysis.
#'
#' @param baseline two-sided formula with
#' outcome variables on LHS and treatment variable on RHS.
#' @param covariate (list of) one-sided formulas with
#' covariates used in regression and balance test on RHS.
#' @param data data.frame/tibble object you want to use.
#' @param levels character vector of levels of experimental arms.
#' The first element must be control.
#' @param labels character vector of labels of experimental arms.
#' @param \dots other arguments
#'
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @importFrom stats formula
#'
#' @export
#' @examples
#' \dontrun{
#' data(RubellaNudge)
#' create_RCTtoolbox(
#'   baseline = itest + ivacc ~ treat,
#'   covariate = ~ age + educ,
#'   data = RubellaNudge,
#'   ctrl = "A"
#' )
#' }
#'
#'
create_RCTtoolbox <- function(baseline,
                              covariate = NULL,
                              data,
                              levels,
                              labels = NULL,
                              ...
                              ) {
  # check arguments
  if (missing(baseline)) abort_empty_arg("baseline")
  if (missing(data)) abort_empty_arg("data")
  if (missing(levels)) abort_empty_arg("levels")
  if (!is.null(labels)) {
    if (length(levels) != length(labels)) {
      abort_length_arg("`label`", length(levels), length(labels))
    }
  } else {
    labels <- levels
  }

  # parse baseline
  lhs <- all.vars(f_lhs(baseline))
  rhs <- all.vars(f_rhs(baseline))
  if (length(rhs) > 1) abort_length_arg("RHS of `baseline`", 1, rhs)
  baseline <- lapply(lhs, function(x) formula(paste(x, "~", rhs)))

  # parse covariate
  if (!is.null(covariate)) {
    if (!is.list(covariate)) covariate <- list(covariate)
    unique_x <- unique(unlist(lapply(covariate, all.vars)))
    covariate <- lapply(covariate, function(x) {
      formula(paste(". ~ .", as.character(x)[2], sep = " + "))
    })
  } else {
    unique_x <- NULL
  }

  # crate R6 class
  RCTtoolbox$new(
    baseline,
    covariate,
    unique_x,
    data,
    levels,
    labels
  )
}
