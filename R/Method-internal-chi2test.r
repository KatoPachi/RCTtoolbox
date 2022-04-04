# Chi-square test
#'
#' @importFrom stats chisq.test
#' @importFrom stats fisher.test
#' @importFrom rlang f_rhs
#'
chi2test <- function(baseline = NULL,
                     data = NULL,
                     treat_levels = NULL,
                     treat_labels = NULL,
                     ctrl = NULL,
                     subset = NULL,
                     test_type = "f",
                     fisher = FALSE,
                     bootp = FALSE) {
  # clean data
  treat <- reorder_arms(treat_levels, treat_labels, ctrl)
  use <- clean_RCTdata(
    baseline,
    data = data,
    treat_levels = treat$levels,
    treat_labels = treat$labels,
    subset = subset
  )

  # design matrix without intercept
  dmat <- use$design[, -1]
  dvar <- as.character(f_rhs(baseline))

  # ctrl data
  ctrl <- as.logical(1 - rowSums(dmat))
  y0 <- use$outcome[ctrl]

  # condition of test_type
  if (test_type == "t") {
    res <- apply(dmat, 2, function(x) {
      # create cross-tabulation
      treated <- as.logical(x)
      y1 <- use$outcome[treated]
      y <- c(y1, y0)
      d <- c(
        rep_len("Treat", length(y1)),
        rep_len(treat$labels[1], length(y0))
      )
      tab <- table(y, d)

      # fisher or chi-squared test
      if (nrow(tab) == 1 | ncol(tab) == 1) {
        message(paste(
          "Either number of cols or rows of cross-tabulation is one;",
          "Chi-squared test or fisher exact test is not performed. Return NA."
        ))
        pval <- NA_real_
      } else {
        if (fisher) {
          pval <- fisher.test(tab, simulate.p.value = bootp)$p.value
        } else {
          pval <- chisq.test(tab)$p.value
        }
      }

      # output
      list(tab = tab, p = pval)
    })

    names(res) <- gsub(dvar, "", names(res))
    res

  } else if (test_type == "f") {
    # create cross-tabulation
    vector_y1 <- apply(dmat, 2, function(x) {
      treated <- as.logical(x)
      use$outcome[treated]
    })
    vector_d1 <- lapply(names(vector_y1), function(x) {
      rep_len(gsub(dvar, "", x), length(vector_y1[[x]]))
    })
    y <- c(y0, unlist(vector_y1))
    d <- c(rep_len(treat$labels[1], length(y0)), unlist(vector_d1))
    tab <- table(y, d)

    # fisher or chi-squared test
    if (nrow(tab) == 1 | ncol(tab) == 1) {
      message(paste(
        "Either number of cols or rows of cross-tabulation is one;",
        "Chi-squared test or fisher exact test is not performed. Return NA."
      ))
      pval <- NA_real_
    } else {
      if (fisher) {
        pval <- fisher.test(tab, simulate.p.value = bootp)$p.value
      } else {
        pval <- chisq.test(tab)$p.value
      }
    }

    # output
    list(tab = tab, p = pval)
  } else {
    abort_invalid_input("test_type", test_type, '"t" or "f')
  }
}

# Chi-square test with multi baseline model
#'
#' @importFrom rlang enexpr
#' @importFrom rlang f_lhs
#'
chi2test_multi_mod <- function(baseline = NULL,
                               data = NULL,
                               treat_levels = NULL,
                               treat_labels = NULL,
                               ctrl = NULL,
                               subset = NULL,
                               test_type = "f",
                               fisher = FALSE,
                               bootp = FALSE) {
  # list of baseline models
  if (!is.list(baseline)) baseline <- list(baseline)

  # run chi2test
  res <- lapply(
    baseline,
    chi2test,
    data,
    treat_levels,
    treat_labels,
    ctrl,
    enexpr(subset),
    test_type,
    fisher,
    bootp
  )

  # output
  labs <- lapply(baseline, function(m) as.character(f_lhs(m)))
  names(res) <- labs
  list(
    result = res,
    method = list(type = test_type, fisher = fisher)
  )
}