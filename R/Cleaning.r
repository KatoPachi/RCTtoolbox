#' Shape dataset
#'
#' @param baseline formula. `outcome ~ treatment`.
#' @param covariate one-sided formula. `. ~ . + x1 + x2`.
#' @param data data.frame/tibble object you want to use.
#' @param treat_levels order of experimental arms
#' @param treat_labels labels of experimental arms
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
#' @importFrom rlang f_rhs
#' @importFrom rlang f_lhs
#'
clean_RCTdata <- function(baseline = NULL,
                          covariate = NULL,
                          data = NULL,
                          treat_levels = NULL,
                          treat_labels = NULL,
                          subset = NULL,
                          weights = NULL,
                          cluster = NULL) {
  # check empty arguments
  if (is.null(data)) abort_empty("data")
  if (is.null(levels)) abort_empty("levels")
  if (is.null(baseline)) abort_empty("baseline")
  if (is.null(treat_levels)) abort_empty("treat_levels")

  # weight, cluster, subset condition
  args <- enquos(
    weights = weights,
    cluster = cluster,
    subset = subset
  )

  # create list of arguments
  args <- lapply(args, eval_tidy, data)
  args$data <- data
  args$na.action <- na.omit

  # make formula
  model <- baseline
  if (!is.null(covariate)) {
    if (is.null(f_lhs(baseline))) {
      model <- update(
        model,
        formula(paste("~ .", as.character(covariate)[2], sep = " + "))
      )
    } else {
      model <- update(
        model,
        formula(paste(". ~ .", as.character(covariate)[2], sep = " + "))
      )
    }
  }

  args$formula <- model

  # run model.frame
  dt <- do.call("model.frame", args)

  # factor treatments
  dvar <- all.vars(f_rhs(baseline))
  if (is.null(treat_labels)) treat_labels <- treat_levels
  dt[[dvar]] <- droplevels(factor(dt[[dvar]], treat_levels, treat_labels))

  # outcome vector, design matrix, weight vector, cluster vector
  list(
    outcome = if (is.null(f_lhs(baseline))) {
      NULL
    } else {
      dt[[all.vars(f_lhs(baseline))]]
    },
    design = model.matrix(model, dt),
    weights = dt$"(weights)",
    cluster = dt$"(cluster)"
  )
}

#' Reorder of treatment arms
#'
#' @param treat_levels original level of treatment arms.
#' The first level is control arm.
#' @param treat_labels label of treatment arms corresponding to original level.
#' @param ctrl new control arm.
#'
#'
reorder_arms <- function(treat_levels = NULL,
                         treat_labels = NULL,
                         ctrl = NULL) {
  # check NULL arguments
  if (is.null(treat_levels)) abort_empty("treat_levels")
  if (is.null(treat_labels)) treat_labels <- treat_levels

  # reorder if ctrl is specified
  if (!is.null(ctrl)) {
    new_ctrl <- seq_len(length(treat_levels))[treat_levels == ctrl]
    new_treat <- seq_len(length(treat_levels))[treat_levels != ctrl]
    treat_levels <- treat_levels[c(new_ctrl, new_treat)]
    treat_labels <- treat_labels[c(new_ctrl, new_treat)]
  }

  list(levels = treat_levels, labels = treat_labels)
}