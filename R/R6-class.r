#'
#' @importFrom R6 R6Class
#'
#'
RCTtoolbox <- R6::R6Class("RCTtoolbox",
  public = list(
    data = NULL,
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
      self$data <- data
    },
    print = function(...) {
      cat("-- Activate Information ---------------------\n")
      cat("Create new toolbox (Class: RCTtoolbox)\n")
      cat("- Outcomes:", private$yvec, "\n")
      cat("- Treatment:", private$dvec, "\n")
      cat("  - Level:", private$dvec.levels)
      cat(" (Control arm:", private$dvec.levels[1], ")\n")
      cat("  - Label:", private$dvec.labels, "\n")
      cat("- Covariates: ", private$xvec, "\n")
      cat("-- Fields and Methods -----------------------\n")
      cat("- data: Store data\n")
      cat("- print(): Show this message\n")
      cat("- ttest(): Run t-test/permutation test\n")
      cat("- power(): Run power analysis\n")
      cat("- balance(): Run balance test\n")
      cat("- lm(): Estimate linear model\n")
      cat("- chi2test(): Run chi-square/fisher exact test\n")
    },
    ttest = function(...) {
      RCTtoolbox.ttest$new(
        private$formula.yd,
        self$data,
        private$dvec.levels,
        private$dvec.labels,
        ...
      )
    },
    power = function(...) {
      RCTtoolbox.power.analysis$new(
        private$dvec,
        self$data,
        private$dvec.levels,
        private$dvec.labels,
        ...
      )
    },
    balance = function(...) {
      RCTtoolbox.balance.test$new(
        private$xvec,
        private$dvec,
        self$data,
        private$dvec.levels,
        private$dvec.labels,
        ...
      )
    },
    lm = function(...) {
      RCTtoolbox.lm$new(
        private$formula.yd,
        private$formula.x,
        self$data,
        private$dvec,
        private$dvec.levels,
        private$dvec.labels,
        ...
      )
    },
    chi2test = function(...) {
      RCTtoolbox.chi2test$new(
        private$formula.yd,
        self$data,
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
    dvec.labels = NULL
  )
)

RCTtoolbox.ttest <- R6::R6Class("RCTtoolbox.ttest",
  public = list(
    result = NULL,
    initialize = function(baseline, data, levels, labels, ...) {
      self$result <- ttest_multi_mod_arm(
        baseline, data, levels, labels, ...
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
    initialize = function(treat, data, levels, labels, ...) {
      self$result <- power_calculation(
        treat, data, levels, labels, ...
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
      cat("- table(): Create output table\n")
    },
    table = function(...) rcttable(self, ...)
  )
)

RCTtoolbox.lm <- R6::R6Class("RCTtoolbox.lm",
  public = list(
    result = NULL,
    initialize = function(yd, x, data, dvec, levels, labels, ...) {
      self$result <- rct_lm(
        yd, x, data, levels, labels, ...
      )
      private$dvar <- dvec
    },
    print = function(...) {
      cat("-- Activate Information ------------------------------\n")
      cat("Estimate Linear Model (Class: RCTtoolbox.lm) \n")
      cat("-- Fields and Methods --------------------------------\n")
      cat("- result: Store estimated result\n")
      cat("- print(): Show this message\n")
      cat("- summary(): Print result in console\n")
      cat("- table(): Create output table\n")
    },
    summary = function(...) summary(self, ...),
    table = function(...) rcttable(self, private$dvar, ...)
  ),
  private = list(dvar = NULL)
)

RCTtoolbox.chi2test <- R6::R6Class("RCTtoolbox.chi2test",
  public = list(
    result = NULL,
    initialize = function(yd, data, levels, labels, ...) {
      self$result <- chi2test_multi_mod(
        yd, data, levels, labels, ...
      )
      private$labels <- labels
    },
    print = function(...) {
      cat("-- Activate Information ------------------------------\n")
      cat("Estimate Linear Model (Class: RCTtoolbox.chi2test) \n")
      cat("-- Fields and Methods --------------------------------\n")
      cat("- result: Store estimated result\n")
      cat("- print(): Show this message\n")
      cat("- table(): Create output table\n")
    },
    table = function(...) rcttable(self, private$labels, ...)
  ),
  private = list(labels = NULL)
)