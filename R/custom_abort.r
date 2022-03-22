#'
#' @importFrom glue glue
#' @importFrom rlang abort
#'
#'
abort_empty_arg <- function(arg) {
  msg <- glue("Argument {arg} must be specified")
  abort(
    "error_empty_arg",
    message = msg,
    arg = arg
  )
}

abort_length_arg <- function(arg, true, obj) {
  obs_len <- length(obj)
  msg <- glue("length of {arg} must be {true}")
  msg <- glue("{msg}; not {obs_len}")

  abort(
    "error_length_arg",
    message = msg,
    arg = arg,
    true = true,
    obs_len = obs_len,
    obj = obj
  )
}