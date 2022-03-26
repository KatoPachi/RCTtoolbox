#'
#' @importFrom R6 R6Class
#'
#'
RCTtoolbox <- R6::R6Class("RCTtoolbox",
  public = list(
    initialize = function(formula_yd = NULL,
                          formula_x = NULL,
                          yvec = NULL,
                          xvec = NULL,
                          dvec = NULL,
                          dvec_levels = NULL,
                          dvec_labels = NULL,
                          data = NULL) {
      private$formula.yd <- formula_yd
      private$formula.x <- formula_x
      private$yvec <- yvec
      private$xvec <- xvec
      private$dvec <- dvec
      private$dvec.levels <- dvec_levels
      private$dvec.labels <- dvec_labels
      private$data <- data
    },
    print = function(...) {
      cat("------------ Activate Information ------------\n")
      cat("Create new toolbox (Class: RCTtoolbox)\n")
      cat("- Outcomes: ", private$yvec, "\n")
      cat("- Treatment: ", private$dvec, "\n")
      cat("  - Control arm: ", private$dvec.levels[1], "\n")
      cat("- Covariates: ", private$xvec, "\n")
      cat("------------------ Methods ------------------\n")
      cat("- print(): Show this message\n")
      cat("- ttest(): Run t-test\n")
    },
    ttest = function(ctrl = NULL, ...) {
      RCTtoolbox.ttest$new(
        private$formula.yd,
        private$data,
        private$dvec.levels,
        private$dvec.labels,
        ctrl,
        ...
      )
    }
  ),
  private = list(
    formula.yd = NULL,
    formula.x = NULL,
    yvec = NULL,
    xvec = NULL,
    dvec = NULL,
    dvec.levels = NULL,
    dvec.labels = NULL,
    data = NULL
  )
)

RCTtoolbox.ttest <- R6::R6Class("RCTtoolbox.ttest",
  public = list(
    result = NULL,
    initialize = function(baseline, data, levels, labels, ctrl = NULL, ...) {
      self$result <- ttest_multi_mod_arm(
        baseline, data, levels, labels, ctrl, ...
      )
    },
    print = function(...) {
      cat("------------ Activate Information ------------\n")
      cat("Run t-test (Class: RCTtoolbox.ttest)\n")
      cat("------------- Fields and Methods -------------\n")
      cat("- result: Store estimated result\n")
      cat("- print(): Show this message\n")
      cat("- plot(): Visualization\n")
      cat("- summary(): Summary of result\n")
    },
    plot = function(...) rctplot(self, ...),
    summary = function(...) summary(self, ...)
  )
)
