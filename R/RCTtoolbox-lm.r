#' Method of RCTtoolbox: \code{lm()}
#'
#' When you create R6 object "RCTtoolbox"
#' via \code{\link{create_RCTtoolbox}}
#' and run its method \code{lm()},
#' the method internally implements a function
#' that fit linear model by least squares.
#' The method \code{lm()} provides some arguments.
#' See the section "Arguments."
#'
#' @param ctrl character. New control arm.
#' If NULL (default),
#' the first element of \code{treat_levels} is control arm.
#' @param subset subset condition.
#' If NULL (default),
#' full observations of \code{data} is used to estimate.
#' @param weights weight variable for weighted least squares.
#' If NULL (default), implement ordinary least squares.
#' @param cluster cluster variable for clustered standard error.
#' If NULL (default), standard error is not clustered.
#' @param only_dmod logical (default is TRUE).
#' Whether to estimate linear model without covariates?
#' @param \dots arguments passed to \code{\link[estimatr]{lm_robust}}.
#'
#' @return R6 object with "RCTtoolbox.balance.test" class.
#' The returned object has following field and methods:
#' \describe{
#'   \item{\code{result}}{Field.
#'     List of estimated result ("lm_robust" class).
#'     See \code{\link[estimatr]{lm_robust}} for each element of list.}
#'   \item{\code{print()}}{Method.
#'     Print information about the returned object.
#'     Run \code{$print()}.}
#'   \item{\code{summary()}}{Method.
#'     Print result in console.
#'     Run \code{$summary()}.}
#'   \item{\code{table()}}{Method.
#'     Create output table of estimation result.
#'     Run \code{$table()}.}
#' }
#'
#' @section Developer Note:
#' A method \code{lm()} provided by R6 object RCTtoolbox
#' implements \code{RCTtoolbox.lm$new()}
#' which generates R6 object with "RCTtoolbox.lm" class.
#' Initialization of R6 object "RCTtoolbox.lm" does two things.
#' First, the private field \code{dvec} of R6 object "RCTtoolbox" is
#' stored in the private field \code{dvar} of returned object.
#' Second, run \code{rct_lm(private$formula.yd, private$formula.x,
#' self$data, private$dvec.levels, private$dvec.labels, ...)}
#' where \code{...} accepts arguments explained in the section "Arguments."
#' The first five arguments passed to \code{rct_lm()} are
#' \describe{
#'   \item{\code{baseline}}{list of two-sided formula \code{outcome ~ treat}.
#'     The method \code{ttest()} automatically
#'     passes the private field \code{formula.yd} of R6 object "RCTtoolbox"
#'     to this argument.}
#'   \item{\code{covariate}}{list of one-sided formula \code{~ var1 + var2}
#'     The method \code{lm()} automatically
#'     passes the private field \code{formula.x} of R6 object "RCTtoolbox"
#'     to this argument.}
#'   \item{\code{data}}{data.frame/tibble object that you want to use.
#'     The method \code{ttest()} automatically
#'     passes a public field \code{data} of R6 object "RCTtoolbox"
#'     to this argument.}
#'   \item{\code{treat_levels}}{character vector. Level of experimental arms.
#'     The first element is control arm.
#'     The method \code{ttest()} automatically
#'     passes the private field \code{dvec.levels} of R6 object "RCTtoolbox"
#'     to this argument.}
#'   \item{\code{treat_labels}}{character vector. Label of experimental arms
#'     corresponding to \code{treat_levels}.
#'     The method \code{ttest()} automatically
#'     passes the private field \code{dvec.labels} of R6 object "RCTtoolbox"
#'     to this argument.}
#' }
#'
#' @examples
#' \dontrun{
#' data(RubellaNudge)
#' rct <- create_RCTtoolbox(
#'   atest + avacc ~ treat,
#'   ~ age + educ,
#'   RubellaNudge,
#'   LETTERS[1:7]
#' )
#'
#' # estimate linear model
#' rct$lm()$summary()
#' }
#'
#' @name lm
#'
NULL