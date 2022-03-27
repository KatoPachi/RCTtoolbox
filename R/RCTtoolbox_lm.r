#' Fitting Models by OLS
#'
#' @description This function generates multiple regression models
#' by passing multiple baseline models
#' (two-sided formula consisting of outcomes and treatments) and
#' multiple covariate models (ons-sided formula) as arguments.
#' In addition, the generated regression model is fitted
#' by the least squares method of the `lm_robust` function
#' in the {estimatr} package.
#'
#' @param baseline two-sided formula with
#' outcome variables on LHS and treatment variable on RHS.
#' @param covariate (list of) one-sided formulas with
#' covariates used in regression and balance test on RHS.
#' @param data data.frame/tibble object you want to use.
#' @param treat_levels character vector of levels of experimental arms.
#' The first element must be control.
#' @param treat_labels character vector of labels of experimental arms.
#' @param ctrl string of new control arm
#' @param subset subset condition of data
#' @param weights weight variable
#' @param cluster cluster variable
#' @param only_dmod logical. estimate linear model without covariates?
#' @param \dots other arguments passed in `estimatr::lm_robust`
#'
#' @importFrom estimatr lm_robust
#' @importFrom stats formula
#' @importFrom rlang f_lhs
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
#' rct$lm(subset = coupon == 1)$summary()
#'
#' }
#'
#'
rct_lm <- function(baseline = NULL,
                   covariate = NULL,
                   data = NULL,
                   treat_levels = NULL,
                   treat_labels = NULL,
                   ctrl = NULL,
                   subset = NULL,
                   weights = NULL,
                   cluster = NULL,
                   only_dmod = TRUE,
                   ...) {
  # check models
  if (is.null(baseline)) abort_empty("basline")
  if (only_dmod) covariate <- append(list(NULL), covariate)
  model.list <- expand.grid(covariate = covariate, baseline = baseline)
  model.list <- model.list[!duplicated(model.list), ]

  # order of treatment
  order_d <- reorder_arms(treat_levels, treat_labels, ctrl)

  # estimation
  res <- lapply(seq_len(nrow(model.list)), function(i) {
    # model
    baseline <- model.list$baseline[i][[1]]
    covariate <- model.list$covariate[i][[1]]

    # cleaning data
    use <- clean_RCTdata(
      baseline,
      covariate,
      data,
      order_d$levels,
      order_d$labels,
      enexpr(subset),
      enexpr(weights),
      enexpr(cluster)
    )


    # estimation
    assign(all.vars(f_lhs(baseline)), use$outcome)
    x <- use$design

    estimatr::lm_robust(
      formula(paste(all.vars(f_lhs(baseline)), "~ -1 + x")),
      weights = use$weights,
      clusters = use$clusters,
      ...
    )
  })

  # output
  res
}
