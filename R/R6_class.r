#'
#' @importFrom R6 R6Class
#'
#'
RCTtoolbox <- R6::R6Class("RCTtoolbox",
  public = list(
    initialize = function(baseline = NULL,
                          covariate = NULL,
                          xvec = NULL,
                          data = NULL,
                          levels = NULL,
                          labels = NULL) {
      private$base <- baseline
      private$cov <- covariate
      private$xvec <- xvec
      private$data <- data
      private$levels <- levels
      private$labels <- labels
    },
    print = function(...) {
      y <- unlist(lapply(private$base, function(x) all.vars(x)[1]))
      arm <- all.vars(private$base[[1]])[2]
      cat("------------ Activate Information ------------\n")
      cat("Create new toolbox (Class: RCTtoolbox)\n")
      cat("- Outcomes: ", y, "\n")
      cat("- Treatment: ", arm, "\n")
      cat("  - Control arm: ", private$levels[1], "\n")
      cat("- Covariates: ", private$xvec, "\n")
      cat("------------------ Methods ------------------\n")
      cat("- print(): Show this message\n")
      cat("- ttest(): Run t-test\n")
    },
    ttest = function(ctrl, ...) {
      if (missing(ctrl)) ctrl <- private$levels[1]
      RCTtoolbox.ttest$new(
        private$base,
        private$data,
        ctrl,
        private$levels,
        private$labels,
        ...
      )
    }
  ),
  private = list(
    base = NULL,
    cov = NULL,
    xvec = NULL,
    data = NULL,
    levels = NULL,
    labels = NULL
  )
)

RCTtoolbox.ttest <- R6::R6Class("RCTtoolbox.ttest",
  public = list(
    result = NULL,
    initialize = function(baseline, data, ctrl, levels, labels, ...) {
      res <- ttest(baseline, data, ctrl, ...)
      res$arms <- factor(res$arms, levels, labels)
      self$result <- res
    },
    print = function(...) {
      cat("------------ Activate Information ------------\n")
      cat("Run t-test (Class: RCTtoolbox.ttest)\n")
      cat("------------- Fields and Methods -------------\n")
      cat("- result: Store estimated result\n")
      cat("- print(): Show this message\n")
      cat("- plot(): Visualization\n")
    },
    plot = function(...) rctplot(self, ...)
  )
)
