#' Method of RCTtoolbox: \code{power()}
#'
#' When you create R6 object "RCTtoolbox"
#' via \code{link{create_RCTtoolbox}()}
#' and run its method \code{power()},
#' the method internally implements a function
#' that performs power analysis.
#' The method \code{power()} provides some arguments.
#' See the section "Arguments."
#'
#' @param ctrl character. New control arm.
#' If NULL (default),
#' the first element of \code{treat_levels} is control arm.
#' @param subset subset condition.
#' If NULL (default),
#' full observations of \code{data} is used to implement power analysis.
#' @param sd numeric. standard deviation (default is 1).
#' Absolute mean difference between two groups
#' is calculated by effect size times \code{sd}.
#' @param \dots arguments passed to \code{\link{ttest_power}()}.
#' This function has five arguments: \code{n0}, \code{n1}, \code{d},
#' \code{alpha}, and \code{beta}.
#' This method automatically specify the first two arguments.
#' Users must specify two of
#' \code{d} (effect size), \code{alpha} (significant level),
#' and \code{beta} (power) in dot-dot-dot.
#' This method calculate one missing argument using \code{\link{uniroot}}.
#'
#' @return R6 object with "RCTtoolbox.power.analysis" class.
#' The returned object has following field and methods:
#' \describe{
#'   \item{\code{result}}{Field.
#'     Data frame including estimated result.
#'     See the section "Result Field."}
#'   \item{\code{print()}}{Method.
#'     Print information about the returned object.
#'     Run \code{$print()}.}
#'   \item{\code{table()}}{Method.
#'     Create output table of result.
#'     Run \code{$plot()}.}
#'   \item{\code{summary()}}{Method.
#'     Print result in console.
#'     Run \code{$summary()}.}
#' }
#'
#' @section Result Field:
#'
#' The R6 object with "RCTtoolbox.power.analysis" class has
#' \code{result} field.
#' This field has a data frame with 8 variables:
#' \describe{
#'   \item{n0}{Number of observations of control arm}
#'   \item{n1}{Number of observations of each experimental arm}
#'   \item{arms}{Factor. Experimental arms.
#'     The first level is control arm.}
#'   \item{d}{Effect size}
#'   \item{alpha}{Significant level}
#'   \item{power}{Power}
#'   \item{sd}{Standard deviation}
#'   \item{diff_mean}{Absolute mean difference of two groups}
#' }
#'
#' @section Developer Note:
#'
#' A method \code{power()} provided by R6 object RCTtoolbox
#' implements \code{RCTtoolbox.power.analysis$new()}
#' which generates R6 object with "RCTtoolbox.power.analysis" class.
#' Initialization of R6 object "RCTtoolbox.ttest" run
#' \code{power_calculation(private$dvec, self$data, private$dvec.levels,
#' private$dvec.labels, ...)}
#' where \code{...} accepts arguments explained in the section "Arguments."
#' The first four arguments passed to \code{ttest_multi_mod_arm()} are
#' \describe{
#'   \item{\code{treat}}{Character. Treatment Variable.
#'     The method \code{power()} automatically
#'     passes the private field \code{dvec} of R6 object "RCTtoolbox"
#'     to this argument.}
#'   \item{\code{data}}{data.frame/tibble object that you want to use.
#'     The method \code{power()} automatically
#'     passes a public field \code{data} of R6 object "RCTtoolbox"
#'     to this argument.}
#'   \item{\code{treat_levels}}{character vector. Level of experimental arms.
#'     The first element is control arm.
#'     The method \code{power()} automatically
#'     passes the private field \code{dvec.levels} of R6 object "RCTtoolbox"
#'     to this argument.}
#'   \item{\code{treat_labels}}{character vector. Label of experimental arms
#'     corresponding to \code{treat_levels}.
#'     The method \code{power()} automatically
#'     passes the private field \code{dvec.labels} of R6 object "RCTtoolbox"
#'     to this argument.}
#' }
#'
#' @examples
#' \dontrun{
#' data(RubellaNudge)
#' rct <- create_RCTtoolbox(
#'   atest + avacc ~ treat,
#'   data = RubellaNudge,
#'   treat_levels = LETTERS[1:7]
#' )
#'
#' # calculate effect size
#' rct$power(alpha = 0.05, power = 0.8)$summary()
#' # calculate effect size with control arm "C"
#' rct$power(alpha = 0.05, power = 0.8, ctrl = "C")$result
#' }
#'
#' @name power
#'
NULL