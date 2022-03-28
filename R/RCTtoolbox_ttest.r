# Calculate sample average
#'
#' @importFrom stats weighted.mean
#'
wtd_mean <- function(x, w = NULL) {
  if (is.null(w)) w <- rep_len(1, length(x))
  weighted.mean(x, w)
}

# Calculate unbiased variance and standard error
#'
#' @importFrom stats sd
#'
wtd_var <- function(x,
                    w = NULL,
                    se = TRUE,
                    bootse = 0L,
                    seed = 120511L) {
  if (is.null(w)) w <- rep_len(1, length(x))
  neff <- sum(w)^2 / sum(w^2)
  wtdmu <- wtd_mean(x, w)
  wtdv <- (sum(w * (x - wtdmu)^2) / sum(w)) * (neff / (neff - 1))

  if (!se) {
    wtdv
  } else {
    if (bootse == 0L) {
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

# Perform Two-Sided T-Test/Permutation Test
#'
#' @importFrom stats pt
#'
ttest <- function(y1,
                  y0,
                  w1 = NULL,
                  w0 = NULL,
                  bootse = 0L,
                  bootp = 0L,
                  seed = 120511L) {
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
  if (bootp == 0L) {
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

# T-test for Multiple Experimental Arms
#'
#' @importFrom dplyr bind_rows
#' @importFrom rlang f_lhs
#'
ttest_multi_arm <- function(baseline = NULL,
                            data = NULL,
                            treat_levels = NULL,
                            treat_labels = NULL,
                            subset = NULL,
                            weights = NULL,
                            bootse = 0L,
                            bootp = 0L,
                            seed = 120511L) {
  # clean data
  use <- clean_RCTdata(
    baseline,
    data = data,
    subset = subset,
    weights = weights,
    treat_levels = treat_levels,
    treat_labels = treat_labels,
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
  res1$arms <- treat_labels[1]

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
  bind_res$outcome <- all.vars(f_lhs(baseline))
  bind_res$arms <- factor(bind_res$arms, treat_labels)
  bind_res
}

#' Method of RCTtoolbox: \code{ttest()}
#'
#' When you create R6 object "RCTtoolbox"
#' via \code{\link{create_RCTtoolbox}}
#' and run its method \code{ttest()},
#' the method internally implements this function
#' which performs two-sided t-test or permutation test.
#'
#' @param baseline list of two-sided formula \code{outcome ~ treat}.
#' The method \code{ttest()} automatically
#' passes the private field \code{formula.yd} of R6 object "RCTtoolbox"
#' to this argument.
#' @param data data.frame/tibble object that you want to use.
#' The method \code{ttest()} automatically
#' passes a public field \code{data} of R6 object "RCTtoolbox"
#' to this argument.
#' @param treat_levels character vector. Level of experimental arms.
#' The first element is control arm.
#' The method \code{ttest()} automatically
#' passes the private field \code{dvec.levels} of R6 object "RCTtoolbox"
#' to this argument.
#' @param treat_labels character vector. Label of experimental arms
#' corresponding to \code{treat_levels}.
#' The method \code{ttest()} automatically
#' passes the private field \code{dvec.labels} of R6 object "RCTtoolbox"
#' to this argument.
#' @param ctrl character vector. New control arm.
#' If NULL (default),
#' the first element of \code{treat_levels} is control arm.
#' The method \code{ttest()} has this argument.
#' @param subset subset condition.
#' If NULL (default),
#' full observations of \code{data} is used to implement t-test.
#' The method \code{ttest()} has this argument.
#' @param weights weight variable.
#' If NULL (default),
#' unbiased unweighted average and std.err. is calculated.
#' The method \code{ttest()} has this argument.
#' @param bootse integer.
#' Number of bootstrap samples to calculate std.err. of mean.
#' If 0 (default),
#' theoretical std.err. of mean is calculated.
#' The method \code{ttest()} has this argument.
#' @param bootp integer.
#' If 0 (default), welch two-sided t-test is implemented.
#' If 1 or more, the permutation test is implemented
#' (the number of permutations that randomly reallocate assignment
#' to keep the sample size collected from each group).
#' The method \code{ttest()} has this argument.
#' @param seed integer.
#' Seed value passed to the argument of \code{\link{set.seed}}.
#' The method \code{ttest()} has this argument.
#'
#' @details
#' A method \code{ttest()} internally run the function
#' \code{function(...) {ttest_multi_mod_arm(
#' private$formula.yd, self$data,
#' private$dvec.levels, private$dvec.labels, ...
#' )}}.
#' Thus, you can provide additional arguments
#' \code{ctrl}, \code{subset}, \code{weights}, \code{bootse},
#' \code{bootp}, and \code{seed} via dot-dot-dot.
#'
#' @return data frame with 13 variables
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
#'     Pr(>|t|) when \code{bootp = 0L} (Two-sided t-test)
#'     Pr(>|diff|) when \code{bootp > 0L} (Permutation test)}
#'   \item{method}{Character. Method of difference mean test.}
#'   \item{arms}{Factor. Experimental arms}
#'   \item{outcome}{Character. Outcome variable.}
#' }
#'
#' @section Return of method \code{ttest()}:
#'
#' When you run \code{ttest()} which is a method of RCTtoolbox class,
#' it internally implements \code{ttest_multi_mod_arm}
#' and create new R6 object with "RCTtoolbox.ttest" class.
#' The returned object of \code{ttest_multi_mod_arm}
#' is saved in a field \code{result}.
#' Thus, you can access it via \code{$result}.
#' RCTtoolbox.ttest class has two methods related to output:
#' \describe{
#'   \item{\code{plot()}}{Visualization of result.}
#'   \item{\code{summary()}}{Print result in console.}
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
#' @importFrom rlang enexpr
#' @importFrom dplyr bind_rows
#'
ttest_multi_mod_arm <- function(baseline = NULL,
                                data = NULL,
                                treat_levels = NULL,
                                treat_labels = NULL,
                                ctrl = NULL,
                                subset = NULL,
                                weights = NULL,
                                bootse = 0L,
                                bootp = 0L,
                                seed = 120511L) {
  # list of baseline model
  if (!is.list(baseline)) baseline <- list(baseline)

  # fix order of factor
  treat <- reorder_arms(treat_levels, treat_labels, ctrl)

  # capture expressions
  subset <- enexpr(subset)
  weights <- enexpr(weights)

  # run ttest_multi_arms
  res <- lapply(
    baseline,
    ttest_multi_arm,
    data,
    treat$levels,
    treat$labels,
    subset,
    weights,
    bootse,
    bootp,
    seed
  )

  bind_rows(res)
}