# Balance Test for one covariate
#'
#' @importFrom estimatr lm_robust
#' @importFrom rlang enexpr
#' @importFrom stats pf
#' @importFrom stats formula
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#'
#'
balance_test <- function(baseline = NULL,
                         data = NULL,
                         treat_levels = NULL,
                         treat_labels = NULL,
                         ctrl = NULL,
                         subset = NULL,
                         weights = NULL,
                         cluster = NULL,
                         ...) {
  # data shape
  use <- clean_RCTdata(
    baseline,
    data = data,
    treat_levels = treat_levels,
    treat_labels = treat_labels,
    subset = subset,
    weights = weights,
    cluster = cluster
  )

  # calculate mean value in each arm
  y <- use$outcome
  d <- use$design[, -1]
  w <- use$weights

  ctrl <- as.logical(1 - rowSums(d))
  mu0 <- wtd_mean(y[ctrl], w[ctrl])
  mu1 <- apply(d, 2, function(x) {
    treated <- as.logical(x)
    wtd_mean(y[treated], w[treated])
  })
  mu <- c(mu0, mu1)

  # implement F-test
  reg <- estimatr::lm_robust(
    y ~ 1 + d,
    weights = w,
    clusters = use$cluster,
    ...
  )

  fstat <- reg$fstatistic
  p_fstat <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

  # output
  data.frame(
    x = all.vars(f_lhs(baseline)),
    item = c(
      treat_labels[1],
      gsub(all.vars(f_rhs(baseline)), "", names(mu1)),
      "fstat",
      "df1",
      "df2",
      "p-value"
    ),
    val = c(mu, fstat[1], fstat[2], fstat[3], p_fstat)
  )
}

# Balance Test by F-test of Overall Significance in Linear Regression
#'
#' @importFrom dplyr bind_rows
#'
balance_test_multi_var <- function(covariate = NULL,
                                   treat = NULL,
                                   data = NULL,
                                   treat_levels = NULL,
                                   treat_labels = NULL,
                                   ctrl = NULL,
                                   subset = NULL,
                                   weights = NULL,
                                   cluster = NULL,
                                   ...) {
  # order of experimental arms
  order_d <- reorder_arms(treat_levels, treat_labels, ctrl)

  # list of model
  model <- lapply(covariate, function(x) formula(paste(x, "~", treat)))

  # implement F-test
  res <- lapply(
    model,
    balance_test,
    data,
    order_d$levels,
    order_d$labels,
    ctrl,
    enexpr(subset),
    enexpr(weights),
    enexpr(cluster),
    ...
  )
  res <- bind_rows(res)

  # convert factor
  res$item <- factor(
    res$item,
    c(order_d$labels, "fstat", "df1", "df2", "p-value")
  )

  # output
  res
}
