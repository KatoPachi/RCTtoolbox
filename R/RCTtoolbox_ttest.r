#' Calculate sample average
#'
#' @param x numeric vector
#' @param w numeric vector of weight.
#' If NULL, unweighted sample average is calculated.
#'
#' @importFrom stats weighted.mean
#'
wtd_mean <- function(x, w = NULL) {
  if (is.null(w)) w <- rep_len(1, length(x))
  weighted.mean(x, w)
}

#'
#' Calculate unbiased variance and standard error
#'
#' @param x numeric vector
#' @param w numeric vector of weight.
#' If NULL, unweighted unbiased estimator is calculated.
#' @param se logical. Return standard error (TRUE) or variance (FALSE)
#' @param bootse numeric. Number of bootstrap sample to calculate se.
#' @param seed numeric. seed value.
#'
#' @importFrom stats sd
#'
wtd_var <- function(x,
                    w = NULL,
                    se = TRUE,
                    bootse = NULL,
                    seed = 120511) {
  if (is.null(w)) w <- rep_len(1, length(x))
  neff <- sum(w)^2 / sum(w^2)
  wtdmu <- wtd_mean(x, w)
  wtdv <- (sum(w * (x - wtdmu)^2) / sum(w)) * (neff / (neff - 1))

  if (!se) {
    wtdv
  } else {
    if (is.null(bootse)) {
      sqrt(wtdv / neff)
    } else {
      set.seed(seed)
      boot_wtdmu <- lapply(seq_len(bootse), function(i) {
        pick <- sample(length(x), size = length(x), replace = TRUE)
        bootx <- x[pick]
        bootw <- w[pick]
        wtd_mean(bootx, bootw)
      })
      sd(unlist(boot_wtdmu))
    }
  }
}

#'
#' Perform Two-Sided T-Test/Permutation Test
#'
#' @param y1 numeric. Outcome in group 1.
#' @param y0 numeric. Outcome in group 0.
#' @param w1 numeric. Weight in group 1.
#' If NULL, unweighted sample average is calculated.
#' @param w0 numeric. Weight in group 0.
#' If NULL, unweighted sample average is calculated.
#' @param bootse numeric. Number of bootstrap sample to calculate se.
#' If NULL, se is calculated by effective sample size.
#' @param bootp numeric. Number of re-randomization
#' to conduct permutation test.
#' If NULL, perform (welch) two-sided t-test.
#' @param seed numeric. seed value.
#'
#' @importFrom stats pt
#'
wtd_ttest <- function(y1,
                      y0,
                      w1 = NULL,
                      w0 = NULL,
                      bootse = NULL,
                      bootp = NULL,
                      seed = 120511) {
  # number of observations
  n1 <- length(y1)
  n0 <- length(y0)

  # weights
  if (is.null(w1)) w1 <- rep_len(1, n1)
  if (is.null(w0)) w0 <- rep_len(1, n0)

  # mean
  mean1 <- wtd_mean(y1, w1)
  mean0 <- wtd_mean(y0, w0)

  # t-test or permutation test
  if (is.null(bootp)) {
    se1 <- wtd_var(y1, w1, bootse = bootse, seed = seed)
    se0 <- wtd_var(y0, w0, bootse = bootse, seed = seed)
    t <- abs(mean1 - mean0) / sqrt(se1^2 + se0^2)
    df <- (se1^2 + se0^2)^2 /
      (se1^4 / (n1 - 1) + se0^4 / (n0 - 1))
    p <- 2 * pt(t, df = df, lower.tail = FALSE)

    data.frame(
      diff = mean1 - mean0,
      mean1 = mean1,
      se1 = se1,
      n1 = n1,
      mean0 = mean0,
      se0 = se0,
      n0 = n0,
      t = t,
      df = df,
      pval = p,
      method = "Welch t-test"
    )
  } else {
    y <- c(y1, y0)
    w <- c(w1, w0)
    d <- rep_len(0, length(y))

    set.seed(seed)
    boot_wtdmu <- lapply(seq_len(bootp), function(i) {
      sim1 <- sample(length(y), size = n1)
      d[sim1] <- 1

      sim_y1 <- y[d == 1]
      sim_y0 <- y[d == 0]
      sim_w1 <- w[d == 1]
      sim_w0 <- w[d == 0]

      sim_mean1 <- wtd_mean(sim_y1, sim_w1)
      sim_mean0 <- wtd_mean(sim_y0, sim_w0)
      sim_mean1 - sim_mean0
    })

    p <- mean(abs(unlist(boot_wtdmu)) >= abs(mean1 - mean0))

    data.frame(
      diff = mean1 - mean0,
      mean1 = mean1,
      n1 = n1,
      mean0 = mean0,
      n0 = n0,
      pval = p,
      method = "Permutation test"
    )
  }

}

#'
#' T-test for Multiple Experimental Arms
#'
#' @param baseline formula. `outcome ~ treatment`.
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
#' @importFrom rlang quo_is_null
#' @importFrom dplyr bind_rows
#'
#'
ttest_multi_arm <- function(baseline,
                            data,
                            levels,
                            labels = NULL,
                            ctrl = NULL,
                            subset,
                            weights,
                            bootse = NULL,
                            bootp = NULL,
                            seed = 120511) {
  # clean data
  if (quo_is_null(subset)) subset <- NULL
  if (quo_is_null(weights)) weights <- NULL
  use <- clean_RCTdata(
    baseline,
    data = data,
    subset = subset,
    weights = weights,
    levels = levels,
    labels = labels
  )

  # outcome, treatment, and weight vector
  y <- use[, all.vars(baseline)[1]]
  d <- use[, all.vars(baseline)[2]]
  w <- if (!is.null(use$"(weights)")) use[, "(weights)"] else NULL

  # control arms
  arms <- levels(d)
  if (is.null(ctrl)) ctrl <- levels(d)[1]
  y0 <- y[d == ctrl]
  w0 <- w[d == ctrl]

  # run t-test
  res <- lapply(arms, function(treat) {
    y1 <- y[d == treat]
    w1 <- w[d == treat]
    resdt <- wtd_ttest(y1, y0, w1, w0, bootse, bootp, seed)
    resdt$arms <- treat
    resdt$outcome <- all.vars(baseline)[1]
    resdt
  })

  bind_res <- bind_rows(res)
  bind_res$arms <- factor(bind_res$arms, arms)
  bind_res
}

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
#' rct <- create_RCTtoolbox(
#'   atest + avacc ~ treat,
#'   data = subset(RubellaNudge, coupon == 1),
#'   ctrl = "A"
#' )
#' rct$ttest()$summary()
#' rct$ttest(ctrl = "C", bootp = 50)$summary()
#' }
#'
ttest <- function(baseline,
                  data,
                  levels,
                  labels = NULL,
                  ctrl = NULL,
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
    levels,
    labels,
    ctrl,
    subset,
    weights,
    bootse,
    bootp,
    seed
  )

  bind_rows(res)
}