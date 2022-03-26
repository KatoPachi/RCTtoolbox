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
#' @importFrom stats model.matrix
#' @importFrom rlang enquos
#' @importFrom rlang eval_tidy
#'
clean_RCTdata <- function(baseline = NULL,
                          covariate = NULL,
                          data = NULL,
                          treat_levels = NULL,
                          subset = NULL,
                          weights = NULL,
                          cluster = NULL) {
  # check empty arguments
  if (is.null(data)) abort_empty("data")
  if (is.null(levels)) abort_empty("levels")
  if (is.null(baseline)) abort_empty("baseline")
  if (is.null(treat_levels)) abort_empty("treat_levels")

  # make formula
  model <- baseline
  if (!is.null(covariate)) {
    model <- update(
      model,
      formula(paste(". ~ .", as.character(covariate)[2], sep = " + "))
    )
  }

  # weight, cluster, subset condition
  args <- enquos(
    weights = weights,
    cluster = cluster,
    subset = subset
  )
  args <- lapply(args, eval_tidy, data)

  # add arguments
  args$formula <- model
  args$data <- data
  args$na.action <- na.omit

  # run model.frame
  dt <- do.call("model.frame", args)

  # factor treatments
  dvar <- all.vars(baseline)[2]
  dt[[dvar]] <- factor(dt[[dvar]], treat_levels)
  dt[[dvar]] <- droplevels(dt[[dvar]])

  # outcome vector, design matrix, weight vector, cluster vector
  list(
    outcome = dt[[all.vars(baseline)[1]]],
    design = model.matrix(model, dt),
    weights = dt$"(weights)",
    cluster = dt$"(cluster)"
  )
}
