#' Method of RCTtoolbox: \code{ttest()}
#'
#' When you create R6 object "RCTtoolbox"
#' via \code{\link{create_RCTtoolbox}}
#' and run its method \code{ttest()},
#' the method internally implements a function
#' that performs two-sided t-test or permutation test.
#' The method \code{ttest()} provides some arguments.
#' See the section "Arguments."
#'
#' @param ctrl character. New control arm.
#' If NULL (default),
#' the first element of \code{treat_levels} is control arm.
#' @param subset subset condition.
#' If NULL (default),
#' full observations of \code{data} is used to implement t-test.
#' @param weights weight variable.
#' If NULL (default),
#' unbiased unweighted average and std.err. is calculated.
#' @param bootse integer.
#' Number of bootstrap samples to calculate std.err. of mean.
#' If 0 (default),
#' theoretical std.err. of mean is calculated.
#' @param bootp integer.
#' If 0 (default), welch two-sided t-test is implemented.
#' If 1 or more, the permutation test is implemented
#' (the number of permutations that randomly reallocate assignment
#' to keep the sample size collected from each group).
#' @param seed integer.
#' Seed value passed to the argument of \code{\link{set.seed}}.
#'
#' @return R6 object with "RCTtoolbox.ttest" class.
#' The returned object has following field and methods:
#' \describe{
#'   \item{\code{result}}{Field.
#'     Data frame including estimated result.
#'     See the section "Result Field."}
#'   \item{\code{print()}}{Method.
#'     Print information about the returned object.
#'     Run \code{$print()}.}
#'   \item{\code{plot()}}{Method.
#'     Visualization of result.
#'     Run \code{$plot()}.}
#'   \item{\code{summary()}}{Method.
#'     Print result in console.
#'     Run \code{$summary()}.}
#' }
#'
#' @section Result Field:
#'
#' The R6 object with "RCTtoolbox.ttest" class has \code{result} field.
#' This field has a data frame with 13 variables:
#' \describe{
#'   \item{mean1}{Outcome average of each experimental arm}
#'   \item{se1}{Std.err. of outcome average of each experimental arm}
#'   \item{n1}{Number of observations of each experimental arm}
#'   \item{mean0}{Outcome average of control arm}
#'   \item{se0}{Std.err. of outcome average of control arm}
#'   \item{n0}{Number of observations of control arm}
#'   \item{diff}{Difference between "mean1" - "mean0"}
#'   \item{t}{t-statistics when \code{bootp = 0L} (Two-sided t-test)}
#'   \item{df}{degree of freedom of t-distribution
#'     when \code{bootp = 0L} (Two-sided t-test)}
#'   \item{pval}{p-value of difference mean test.
#'     Pr(>|t|) when \code{bootp = 0L} (Two-sided t-test).
#'     Pr(>|diff|) when \code{bootp > 0L} (Permutation test).}
#'   \item{method}{Character. Method of difference mean test.}
#'   \item{arms}{Factor. Experimental arms.
#'     The first level is control arm.}
#'   \item{outcome}{Character. Outcome variable.}
#' }
#'
#' @section Developer Note:
#' A method \code{ttest()} provided by R6 object RCTtoolbox
#' implements \code{RCTtoolbox.ttest$new()}
#' which generates R6 object with "RCTtoolbox.ttest" class.
#' Initialization of R6 object "RCTtoolbox.ttest" run
#' \code{ttest_multi_mod_arm(private$formula.yd, self$data, private$dvec.levels,
#' private$dvec.labels, ...)}
#' where \code{...} accepts arguments explained in the section "Arguments."
#' The first four arguments passed to \code{ttest_multi_mod_arm()} are
#' \describe{
#'   \item{\code{baseline}}{list of two-sided formula \code{outcome ~ treat}.
#'     The method \code{ttest()} automatically
#'     passes the private field \code{formula.yd} of R6 object "RCTtoolbox"
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
#'     data = RubellaNudge,
#'     treat_levels = LETTERS[1:7]
#'   )
#'
#'   # two-sided t-test
#'   rct$ttest()
#'   # permutation test with control arm "C"
#'   rct$ttest(bootp = 50, ctrl = "C")
#' }
#'
#' @name ttest
#'
NULL