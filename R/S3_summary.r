#'
#' Summary of t-test
#'
#' @method summary RCTtoolbox.ttest
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
    cat("\n\n")
  }
}