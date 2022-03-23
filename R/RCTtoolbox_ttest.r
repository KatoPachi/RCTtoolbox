#' RCTtoolbox: T-test
#'
#' @description This function performs a t-test to test the null hypothesis
#' that there is no mean difference between the two groups.
#' When there are three or more treatment groups,
#' this function tests the difference of mean between
#' the control group specified by the user with the `ctrl` argument
#' and each group (including the control group).
#'
#' @param baseline (list of) baseline formulas. `outcome ~ treatment`.
#' @param data data.frame/tibble object you want to use.
#' @param subset subset condition.
#' @param weights weight variable.
#' @param bootse numeric. Number of bootstrap sample to calculate se.
#' If NULL, se is calculated by effective sample size.
#' @param bootp numeric. Number of re-randomization
#' to conduct permutation test.
#' If NULL, perform (welch) two-sided t-test.
#' @param seed numeric. seed value.
#'
#' @importFrom rlang enquo
#' @importFrom dplyr bind_rows
#'
#' @examples
#' \dontrun{
#' data(RubellaNudge)
#' rct <- new_RCTtool(
#'   atest + avacc ~ treat,
#'   data = subset(RubellaNudge, coupon == 1),
#'   ctrl = "A"
#' )
#' rct$ttest()$result
#' rct$ttest(ctrl = "C", bootp = 50)$result
#' }
#'
ttest <- function(baseline,
                  data,
                  ctrl,
                  subset = NULL,
                  weights = NULL,
                  bootse = NULL,
                  bootp = NULL,
                  seed = 120511) {
  # list of baseline model
  if (!is.list(baseline)) baseline <- list(baseline)

  # check arguments
  subset <- enquo(subset)
  weights <- enquo(weights)

  # run ttest_multi_arms
  res <- lapply(
    baseline,
    ttest_multi_arm,
    data,
    ctrl,
    subset,
    weights,
    bootse,
    bootp,
    seed
  )

  bind_rows(res)
}