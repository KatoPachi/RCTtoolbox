#' Method of RCTtoolbox: \code{balance()}
#'
#' When you create R6 object "RCTtoolbox"
#' via \code{\link{create_RCTtoolbox}}
#' and run its method \code{balance()},
#' the method internally implements a function
#' that performs balance test.
#' The method \code{balance()} provides some arguments.
#' See the section "Arguments."
#'
#' @param ctrl character. New control arm.
#' If NULL (default),
#' the first element of \code{treat_levels} is control arm.
#' @param subset subset condition.
#' If NULL (default),
#' full observations of \code{data} is used to implement balance test.
#' @param weights weight variable for weighted least squares.
#' If NULL (default), implement ordinary least squares.
#' @param cluster cluster variable for clustered standard error.
#' If NULL (default), standard error is not clustered.
#' @param \dots arguments passed to \code{\link[estimatr]{lm_robust}}.
#'
#' @return R6 object with "RCTtoolbox.balance.test" class.
#' The returned object has following field and methods:
#' \describe{
#'   \item{\code{result}}{Field.
#'     Data frame including estimated result.
#'     See the section "Data Field."}
#'   \item{\code{print()}}{Method.
#'     Print information about the returned object.
#'     Run \code{$print()}.}
#'   \item{\code{table()}}{Method.
#'     Create output table of balance test.
#'     Run \code{$table()}.}
#' }
#'
#' @section Result Field:
#' The R6 object with "RCTtoolbox.balance.test" class has \code{result} field.
#' This field has a data frame with 3 variables:
#' \describe{
#'   \item{x}{Character. Covariates.}
#'   \item{item}{Factor.
#'     Label of experimental arms, f-value,
#'     degree of freedoms of F-distribution, p-value of F.}
#'   \item{val}{Numeric. Value corresponding to item.}
#' }
#'
#' @section Developer Note:
#' The method \code{balance()} provided by R6 object RCTtoolbox
#' implements \code{RCTtoolbox.balance.test$new()}
#' which generates R6 object with "RCTtoolbox.balance.test" class.
#' Initialization of R6 object "RCTtoolbox.balance.test" run
#' \code{balance_test_multi_var(private$xvec, private$dvec,
#' self$data, private$dvec.levels, private$dvec.labels, ...)}
#' where \code{...} accepts arguments explained in the section "Arguments."
#' The first five arguments passed to \code{balance_test_multi_var()} are
#' \describe{
#'   \item{\code{covariate}}{Character vector. Covariate.
#'     The method \code{balance()} automatically
#'     passes the private field \code{xvec} of R6 object "RCTtoolbox"
#'     to this argument.}
#'   \item{\code{treat}}{Character vector. Treatment.
#'     The method \code{balance()} automatically
#'     passes the private field \code{dvec} of R6 object "RCTtoolbox"
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
#'   data(RubellaNudge)
#'   rct <- create_RCTtoolbox(
#'     atest + avacc ~ treat,
#'     ~ age + educ,
#'     RubellaNudge,
#'     LETTERS[1:7]
#'   )
#'
#' # balance test
#' rct$balance()$result
#' }
#'
#' @name balance
#'
NULL