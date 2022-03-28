# Fitting Models by OLS
#'
#'
#' @importFrom estimatr lm_robust
#' @importFrom stats formula
#' @importFrom rlang f_lhs
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
