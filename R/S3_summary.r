# Summary of t-test
#'
#' @importFrom stats printCoefmat
#'
summary.RCTtoolbox.ttest <- function(object, ...) {
  res <- object$result
  outcome <- unique(res$outcome)
  method <- unique(res$method)
  ctrl <- levels(res$arms)[1]
  cat("Mean Difference Test:", method, "\n")
  cat("Control arms: ", ctrl, "\n")
  for (y in outcome) {
    cat("Outcome: ", y, "\n")
    dt <- res[res$outcome == y, ]
    coef_mat <- as.matrix(dt[, c("mean1", "diff")])
    tvec <- if (method == "Welch t-test") dt$t else rep_len(NA, nrow(dt))
    pvec <- dt$pval
    show_mat <- cbind(coef_mat, tvec, pvec)
    colnames(show_mat) <- c("Average", "Difference", "t-value", "Pr(>|t|)")
    rownames(show_mat) <- dt$arms
    show_mat[rownames(show_mat) == ctrl, 2:4] <- NA
    printCoefmat(show_mat, digits = 4, na.print = "", signif.stars = FALSE)
    cat("\n")
  }
}

# Summary of power analysis
#'
#' @importFrom stats printCoefmat
#'
summary.RCTtoolbox.power.analysis <- function(object, ...) {
  res <- object$result
  ctrl <- res[res$arms == levels(res$arms)[1], ]
  treat <- res[res$arms != levels(res$arms)[1], ]
  cat("Post Power Analysis for Two-Sided T-Test\n")
  cat("Control Arm:", levels(res$arms)[1], "(N =", ctrl$n1, ")\n")
  alpha <- unique(treat$alpha)
  cat("Significant Size:", alpha, "\n")
  power <- unique(treat$power)
  cat("Power:", power, "\n")
  sd <- unique(treat$sd)
  cat("Std.dev:", sd)
  cat(" (Use transformation standardized effect to unstandardized one)\n")
  show <- as.matrix(treat[, c("n1", "d", "diff_mean")])
  colnames(show) <- c("Num.Obs.", "Standardized", "Unstandardized")
  rownames(show) <- treat$arms
  printCoefmat(show, cs.ind = 0)
  cat("\n")
}

# Summary of linear model
#'
summary.RCTtoolbox.lm <- function(object, ...) {
  lapply(object$result, summary, ...)
}