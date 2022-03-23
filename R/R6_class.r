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
                          ctrl = NULL) {
      private$base <- baseline
      private$cov <- covariate
      private$xvec <- xvec
      private$data <- data
      private$ctrl <- ctrl
    },
    print = function(...) {
      y <- unlist(lapply(private$base, function(x) all.vars(x)[1]))
      arm <- all.vars(private$base[[1]])[2]
      cat("------------ Activate Information ------------\n")
      cat("Create new toolbox (Class: RCTtoolbox)\n")
      cat("- Outcomes: ", y, "\n")
      cat("- Treatment: ", arm, "\n")
      cat("  - Control arm: ", private$ctrl, "\n")
      cat("- Covariates: ", private$xvec, "\n")
      cat("------------------ Methods ------------------\n")
      cat("- print(): Show this message\n")
      cat("- ttest(): Run t-test")
    },
    ttest = function(baseline, data, ctrl, ...) {
      if (missing(ctrl)) ctrl <- private$ctrl
      RCTtoolbox.ttest$new(private$base, private$data, ctrl, ...)
    }
  ),
  private = list(
    base = NULL,
    cov = NULL,
    xvec = NULL,
    data = NULL,
    ctrl = NULL
  )
)

RCTtoolbox.ttest <- R6::R6Class("RCTtoolbox.ttest",
  public = list(
    result = NULL,
    initialize = function(baseline, data, ctrl, ...) {
      self$result <- ttest(
        baseline,
        data,
        ctrl,
        ...
      )
    },
    print = function(...) {
      cat("------------ Activate Information ------------\n")
      cat("Run t-test (Class: RCTtoolbox.ttest)\n")
      cat("------------- Fields and Methods -------------\n")
      cat("- result: Store estimated result\n")
      cat("- print(): Show this message\n")
    }
  )
)
