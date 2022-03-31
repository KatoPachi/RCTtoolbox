#' Create New Toolbox
#'
#' Create an R6 class object called "RCTtoolbox".
#'
#' @param baseline two-sided formula with
#' one or more outcome variables on LHS and
#' one treatment variable on RHS.
#' @param covariate (list of) one-sided formulas with
#' covariates used in regression and balance test on RHS.
#' @param data data.frame/tibble object you want to use.
#' @param treat_levels character vector. Levels of experimental arms.
#' The first element is control arm.
#' @param treat_labels character vector.
#' Labels of experimental arms corresponding to \code{treat_levels}.
#' Default is to use \code{treat_levels} as labels.
#'
#' @return R6 object with "RCTtoolbox" class.
#' The returned object has following fields and methods
#' \describe{
#'   \item{\code{data}}{Field.
#'     Store data.frame/tibble passed to \code{data} argument.
#'     You can access via \code{$data}}
#'   \item{\code{print()}}{Method.
#'     Print information about the returned object.
#'     Run \code{$print()}}
#'   \item{\code{ttest()}}{Method.
#'     Implement two-sided t-test or permutation test.
#'     Run \code{$ttest()}.
#'     See \link{ttest} for help.}
#'   \item{\code{power()}}{Method.
#'     Implement power analysis.
#'     Run \code{$power()}.
#'     See \link{power} for help.}
#'   \item{\code{balance()}}{Method.
#'     Implement balance test.
#'     Run \code{$balance()}.
#'     See \link{balance} for help.}
#'   \item{\code{lm()}}{Method.
#'     Estimate linear model.
#'     Run \code{$lm()}.
#'     See \link{lm} for help.}
#' }
#'
#' @section Private fields:
#' The created R6 object has also private fields.
#' You cannot access from the outside.
#' However, methods of RCTtoolbox use private fields.
#' A list of private fields is as follows:
#' \describe{
#'   \item{formula.yd}{List of two-sided formula
#'     \code{outcome ~ treatment}.
#'     If you pass \code{y1 + y2 ~ d} to
#'     the argument \code{baseline},
#'     \code{formula.yd} refers to
#'     \code{list(y1 ~ d, y2 ~ d)}.}
#'   \item{formula.x}{Lists of one-sided formula
#'     with covariates on RHS
#'     which is passed to the argument \code{covariate}.}
#'   \item{yvec}{Character vector of outcome variables.}
#'   \item{xvec}{Character vector of covariates.}
#'   \item{dvec}{Character of treatment.}
#'   \item{dvec.levels}{Character vector
#'     passed to the argument \code{treat_levels}.}
#'   \item{dvec.labels}{Character vector
#'     passed to the argument \code{treat_labels}.}
#' }
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
  xvec <- NULL
  if (!is.null(covariate)) {
    if (!is.list(covariate)) covariate <- list(covariate)
    xvec <- unique(unlist(lapply(covariate, all.vars)))
  }


  # crate R6 class
  rct <- RCTtoolbox$new(
    formula_yd,
    covariate,
    yvec,
    xvec,
    dvec,
    treat_levels,
    treat_labels,
    data
  )

  rct$print()
  invisible(rct)
}
