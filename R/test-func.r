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