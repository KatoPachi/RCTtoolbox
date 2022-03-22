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
      cat("Create new toolbox!! \n")
      cat("Information:\n")
      cat("- Outcomes:", y, "\n")
      cat("- Treatment variable:", arm, "\n")
      cat("  - Control arm:", private$ctrl, "\n")
      if (!is.null(private$xvec)) {
        cat("- Covariates:", private$xvec, "\n")
      }
      if (!is.null(private$data)) {
        cat("NOTE: You have already registered data.frame/tibble object.\n")
      }
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