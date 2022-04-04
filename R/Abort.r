#'
#' @importFrom glue glue
#' @importFrom rlang abort
#'
#'
abort_empty <- function(arg) {
  msg <- glue("Argument {arg} must be specified")
  msg <- glue("{msg}; not missing or NULL")
  abort(
    "error_empty",
    message = msg,
    arg = arg
  )
}

abort_length <- function(arg, true, obj) {
  obs_len <- length(obj)
  msg <- glue("length of {arg} must be {true}")
  msg <- glue("{msg}; not {obs_len}")

  abort(
    "error_length",
    message = msg,
    arg = arg,
    true = true,
    obs_len = obs_len,
    obj = obj
  )
}

abort_empty_num <- function(obs, true) {
  msg <- glue("Need {true} missed arguments")
  msg <- glue("{msg}; not {obs}")
  abort(
    "error_number_of_missed_args",
    message = msg,
    true = true,
    obs = obs
  )
}

abort_invalid_input <- function(arg, obs, true) {
  msg <- glue("`{arg}` must be {true}")
  msg <- glue("{msg}; not {obs}")
  abort(
    "error_invalid_input",
    message = msg,
    arg = arg,
    obs = obs,
    true = true
  )
}