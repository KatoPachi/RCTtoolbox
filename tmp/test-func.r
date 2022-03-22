#' F-test of Overall Siginificance in Linear Regression
#'
#' @param x a string of covariates
#' @param arms a string of treatment variables
#' @param arms_label a string vector with labels of treatment arms.
#' @param data data which you want to use.
#'
#' @importFrom stats complete.cases
#' @importFrom stats anova
#' @importFrom stats lm
#'
#'
ftest <- function(x, arms, arms_label, data) {
  # data shape
  usedt <- data[, c(x, arms)]
  usedt <- usedt[complete.cases(usedt), ]

  # calculate mean value in each arm
  mu <- lapply(arms_label, function(t) {
    mean(usedt[usedt[[arms]] == t, ][[x]])
  })

  # implement F-test
  f <- anova(lm(paste0(x, "~", arms), usedt))[["Pr(>F)"]][1]

  # output
  data.frame(
    x = x,
    item = c(arms_label, "P-value (F-test)"),
    val = c(unlist(mu), f)
  )
}

#' T-Test for RCTs with Multiple Experimental Arms
#'
#' @param y a string of outcome.
#' @param arms a string of treatment variables.
#' @param arms_label a string vector of label of treatments.
#' @param ctrl a string of the control.
#' @param data data object you want to use.
#'
#' @importFrom stats t.test
#' @importFrom broom tidy
#' @importFrom dplyr bind_rows
#'
ttest <- function(y, arms, arms_label, ctrl, data) {
  # using data
  usedt <- data[, c(y, arms)]
  usedt <- usedt[complete.cases(usedt), ]

  # vector of outcome variables
  y0 <- usedt[usedt[[arms]] == ctrl, ][[y]]
  y1 <- lapply(arms_label, function(x) usedt[usedt[[arms]] == x, ][[y]])

  # t-test
  t <- lapply(y1, t.test, y = y0)
  t <- lapply(t, broom::tidy)

  # standard errors of mean
  stderr <- lapply(y1, se, na.rm = TRUE)

  # data.frame containing result
  out <- dplyr::bind_rows(t)
  out <- out[, c("estimate", "estimate1", "p.value")]
  colnames(out) <- c("effect", "mean", "p.value")
  out$se <- unlist(stderr)
  out$outcome <- y
  out$treat <- arms_label
  out <- out[, c("outcome", "treat", "mean", "se", "effect", "p.value")]

  # output
  out
}