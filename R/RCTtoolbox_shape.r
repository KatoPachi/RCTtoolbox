#' Shape dataset
#'
#' @param baseline formula. `outcome ~ treatment`.
#' @param covariate one-sided formula. `. ~ . + x1 + x2`.
#' @param data data.frame/tibble object you want to use.
#' @param subset subset condition.
#' @param weights weight variable.
#' @param cluster cluster variable.
#'
#' @importFrom stats update
#' @importFrom stats na.omit
#' @importFrom stats model.frame
#' @importFrom rlang is_call
#' @importFrom rlang enquo
#' @importFrom rlang eval_tidy
#'
clean_RCTdata <- function(baseline,
                          covariate = NULL,
                          data,
                          subset = NULL,
                          weights = NULL,
                          cluster = NULL) {
  # check dataset
  if (missing(data)) abort_empty_arg("data")

  # make formula
  if (missing(baseline)) abort_empty_arg("baseline")
  model <- baseline
  if (!is.null(covariate)) model <- update(model, covariate)

  # weight, cluster, subset condition
  w <- NULL
  if (!is.null(weights)) w <- eval_tidy(weights, data)

  g <- NULL
  if (!is.null(cluster)) g <- eval_tidy(cluster, data)

  sub <- NULL
  if (!is.null(subset)) sub <- eval_tidy(subset, data)

  # model.frame
  args <- list(
    formula = model,
    data = data,
    subset = sub,
    weights = w,
    cluster = g,
    na.action = na.omit
  )

  do.call("model.frame", args)
}
