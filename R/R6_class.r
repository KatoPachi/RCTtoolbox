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
      cat("-- Activate Information ---------------------\n")
      cat("Create new toolbox (Class: RCTtoolbox)\n")
      cat("- Outcomes: ", private$yvec, "\n")
      cat("- Treatment: ", private$dvec, "\n")
      cat("  - Control arm: ", private$dvec.levels[1], "\n")
      cat("- Covariates: ", private$xvec, "\n")
      cat("-- Methods ----------------------------------\n")
      cat("- print(): Show this message\n")
      cat("- ttest(): Run t-test\n")
      cat("- power(): Run power analysis\n")
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
    },
    power = function(ctrl = NULL, ...) {
      RCTtoolbox.power.analysis$new(
        private$dvec,
        private$data,
        private$dvec.levels,
        private$dvec.labels,
        ctrl,
        ...
      )
    },
    balance = function(...) {
      RCTtoolbox.balance.test$new(
        private$xvec,
        private$dvec,
        private$data,
        private$dvec.levels,
        private$dvec.labels,
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
      cat("-- Activate Information ----------------\n")
      cat("Run t-test (Class: RCTtoolbox.ttest)    \n")
      cat("-- Fields and Methods ------------------\n")
      cat("- result: Store estimated result\n")
      cat("- print(): Show this message\n")
      cat("- plot(): Visualization\n")
      cat("- summary(): Print result in console\n")
    },
    plot = function(...) rctplot(self, ...),
    summary = function(...) summary(self, ...)
  )
)

RCTtoolbox.power.analysis <- R6::R6Class("RCTtoolbox.power.analysis",
  public = list(
    result = NULL,
    initialize = function(treat, data, levels, labels, ctrl = NULL, ...) {
      self$result <- power_calculation(
        treat, data, levels, labels, ctrl, ...
      )
    },
    print = function(...) {
      cat("-- Activate Information ------------------------------\n")
      cat("Run Power Analysis (Class: RCTtoolbox.power.analysis) \n")
      cat("-- Fields and Methods --------------------------------\n")
      cat("- result: Store estimated result\n")
      cat("- print(): Show this message\n")
      cat("- summary(): Print result in console\n")
      cat("- table(): Create output table\n")
    },
    summary = function(...) summary(self, ...),
    table = function(...) rcttable(self, ...)
  )
)

RCTtoolbox.balance.test <- R6::R6Class("RCTtoolbox.balance.test",
  public = list(
    result = NULL,
    initialize = function(x, d, data, levels, labels, ...) {
      self$result <- balance_test_multi_var(
        x, d, data, levels, labels, ...
      )
    },
    print = function(...) {
      cat("-- Activate Information ------------------------------\n")
      cat("Run Balance Test (Class: RCTtoolbox.balance.test) \n")
      cat("-- Fields and Methods --------------------------------\n")
      cat("- result: Store estimated result\n")
      cat("- print(): Show this message\n")
      cat("- summary(): Print result in console\n")
      cat("- table(): Create output table\n")
    },
    table = function(...) rcttable(self, ...)
  )
)