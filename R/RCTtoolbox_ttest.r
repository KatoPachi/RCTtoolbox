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
ttest <- function(y1,
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

  # se of mean
  se1 <- wtd_var(y1, w1, bootse = bootse, seed = seed)
  se0 <- wtd_var(y0, w0, bootse = bootse, seed = seed)

  # t-test or permutation test
  if (is.null(bootp)) {
    t <- abs(mean1 - mean0) / sqrt(se1^2 + se0^2)
    df <- (se1^2 + se0^2)^2 /
      (se1^4 / (n1 - 1) + se0^4 / (n0 - 1))
    p <- 2 * pt(t, df = df, lower.tail = FALSE)

    data.frame(
      mean1 = mean1,
      se1 = se1,
      n1 = n1,
      mean0 = mean0,
      se0 = se0,
      n0 = n0,
      diff = mean1 - mean0,
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
      mean1 = mean1,
      se1 = se1,
      n1 = n1,
      mean0 = mean0,
      se0 = se0,
      n0 = n0,
      diff = mean1 - mean0,
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
ttest_multi_arm <- function(baseline = NULL,
                            data = NULL,
                            treat_levels = NULL,
                            treat_labels = NULL,
                            subset = NULL,
                            weights = NULL,
                            bootse = NULL,
                            bootp = NULL,
                            seed = 120511) {
  # clean data
  use <- clean_RCTdata(
    baseline,
    data = data,
    subset = subset,
    weights = weights,
    treat_levels = treat_levels
  )

  # outcome, treatment, and weight vector
  y <- use$outcome
  d <- use$design[, -1]
  w <- use$weights

  # control arms
  ctrl <- as.logical(1 - rowSums(d))
  y0 <- y[ctrl]
  w0 <- w[ctrl]
  res1 <- ttest(y0, y0, w0, w0, bootse, bootp, seed)
  res1$arms <- treat_levels[1]

  # run t-test
  res2 <- apply(d, 2, function(x) {
    treated <- as.logical(x)
    y1 <- y[treated]
    w1 <- w[treated]
    ttest(y1, y0, w1, w0, bootse, bootp, seed)
  })

  bind_res2 <- bind_rows(res2)
  bind_res2$arms <- gsub(all.vars(baseline)[2], "", names(res2))

  bind_res <- bind_rows(res1, bind_res2)
  bind_res$outcome <- all.vars(baseline)[1]
  if (is.null(treat_labels)) treat_labels <- treat_levels
  bind_res$arms <- factor(bind_res$arms, treat_levels, treat_labels)
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
#' @importFrom rlang enexpr
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
ttest_multi_mod_arm <- function(baseline = NULL,
                                data = NULL,
                                treat_levels = NULL,
                                treat_labels = NULL,
                                ctrl = NULL,
                                subset = NULL,
                                weights = NULL,
                                bootse = NULL,
                                bootp = NULL,
                                seed = 120511) {
  # list of baseline model
  if (!is.list(baseline)) baseline <- list(baseline)

  # fix order of factor
  if (!is.null(ctrl)) {
    new_ctrl <- seq_len(length(treat_levels))[treat_levels == ctrl]
    new_treat <- seq_len(length(treat_levels))[treat_levels != ctrl]
    treat_levels <- treat_levels[c(new_ctrl, new_treat)]
    if (!is.null(treat_labels)) {
      treat_labels <- treat_labels[c(new_ctrl, new_treat)]
    }
  }

  # capture expressions
  subset <- enexpr(subset)
  weights <- enexpr(weights)

  # run ttest_multi_arms
  res <- lapply(
    baseline,
    ttest_multi_arm,
    data,
    treat_levels,
    treat_labels,
    subset,
    weights,
    bootse,
    bootp,
    seed
  )

  bind_rows(res)
}