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
#'   treat_levels = LETTERS[1:7]
#' )
#' }
#'
#'
create_RCTtoolbox <- function(baseline = NULL,
                              covariate = NULL,
                              data = NULL,
                              treat_levels = NULL,
                              treat_labels = treat_levels
                              ) {
  # check arguments
  if (is.null(baseline)) abort_empty("baseline")
  if (is.null(data)) abort_empty("data")
  if (is.null(treat_levels)) abort_empty("treat_levels")
  if (length(treat_levels) != length(treat_labels)) {
    abort_length("`label`", length(treat_levels), treat_labels)
  }

  # parse baseline
  yvec <- all.vars(f_lhs(baseline))
  dvec <- all.vars(f_rhs(baseline))
  if (length(dvec) > 1) abort_length("RHS of `baseline`", 1, dvec)
  formula_yd <- lapply(yvec, function(x) formula(paste(x, "~", dvec)))

  # parse covariate
  if (!is.null(covariate)) {
    if (!is.list(covariate)) covariate <- list(covariate)
    xvec <- unique(unlist(lapply(covariate, all.vars)))
  } else {
    covariate <- xvec <- NULL
  }

  # crate R6 class
  RCTtoolbox$new(
    formula_yd,
    covariate,
    yvec,
    xvec,
    dvec,
    treat_levels,
    treat_labels,
    data
  )
}
