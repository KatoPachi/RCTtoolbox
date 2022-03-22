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
      cat("Toolbox information:\n")
      cat("- Outcomes:", y, "\n")
      cat("- Treatment variable:", arm, "\n")
      cat("  - Control arm:", private$ctrl, "\n")
      if (!is.null(private$xvec)) {
        cat("- Covariates:", private$xvec, "\n")
      }
      if (!is.null(private$data)) {
        cat("NOTE: You have already registered data.frame/tibble object.\n")
      }
    },
    active = function(type) {
      switch(type,
        "ttest" = RCTtoolbox.ttest$new(
          private$base,
          private$cov,
          private$xvec,
          private$data,
          private$ctrl
        ),
        stop("Invalid `type` value")
      )
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
  inherit = RCTtoolbox,
  public = list(
    result = NULL,
    print = function(...) {
      super$print()
      cat("ACTIVATE: t-test\n")
    },
    run = function(ctrl, ...) {
      if (missing(ctrl)) ctrl <- private$ctrl
      self$result <- ttest(
        private$base,
        private$data,
        ctrl,
        ...
      )
      invisible(self)
    }
  )
)
