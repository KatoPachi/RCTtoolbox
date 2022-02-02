#' Output Table of Empirical Analysis for RCTs
#'
#' @description This is an S3 generic function
#' for creating table of RCT analysis.
#' RCTtoolbox provides a method for power_analysis
#' (generated by the power_analysis function).
#'
#' @param \dots Specify the arguments to pass to this function.
#' The first argument must be a dataframe
#' with a class supported by rct_table (power_analysis).
#' You can check the arguments that can be passed in
#' the power_analysis class with `help(rct_table.power_analysis)`.
#'
#' @export
#'
#' @examples
#' # DGP
#' set.seed(120511)
#' n <- 1000
#' x1 <- rnorm(n); x2 <- rnorm(n)
#' d <- sample(c("A", "B", "C"), size = n, replace = TRUE)
#'
#' ya <- 0.2 + 0.5 * x1 + 0.01 * x2
#' yb <- 1.2 + 0.3 * x2
#' yc <- -1 - 0.2 * x1 + 0.5 * x2
#' y <- ifelse(d == "A", ya, ifelse(d == "B", yb, yc))
#' dt <- data.frame(y, d, x1, x2)
#'
#' # power analysis with variance assumption
#' tmp <- power_analysis(~d, dt, alpha = 0.05, power = 0.8, std_dev = 0.2)
#' rct_table(
#'   tmp,
#'   title = paste(
#'     "Power Analysis:",
#'     "Least Mean Difference to Keep 80% Power and 5% Significant Level"
#'   ),
#'   footnote = paste(
#'     "Note: To calculate mean difference,",
#'     "we assume that standard deviation of outcome is 0.2."
#'   ),
#'   output = "kableExtra"
#' )
#'
rct_table <- function(...) {
  UseMethod("rct_table")
}

#' Output Table for Power Analysis
#'
#' @description This function outputs
#' the result of power analysis in tabular form.
#' For tabular output,
#' this function uses the `datasummary` function of the {modelsummary} package
#' that corresponds to various outputs.
#' `rct_table.power_analysis` supports "kableExtra" (for PDF and HTML output),
#' "flextable" (MS Word and MS Powerpoint), and "data.frame".
#'
#' @param data dataframe with class "power_analysis"
#' @param target character. Specify which value to output.
#' If "d", the effect size is output.
#' If "unstd_d", the non-standarized effect size
#' (i.e., absolute value of mean difference) is output.
#' If "alpha", the significant level is output.
#' If "power", the power is output.
#' @param digits numeric. 
#' Specify the number of decimal places to display (Default is 3).
#' @param title character. Table title.
#' @param output character. Output format. This functions supports
#' "kableExtra", "flextable", and "data.frame"
#' @param footnote character. Footnote.
#' @param size numeric. Font size.
#'
#' @method rct_table power_analysis
#' @importFrom modelsummary datasummary
#' @importFrom magrittr %>%
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra footnote
#' @importFrom flextable add_footer_lines
#' @importFrom flextable fontsize
#' @export
#'
#' @examples
#' # DGP
#' set.seed(120511)
#' n <- 1000
#' x1 <- rnorm(n); x2 <- rnorm(n)
#' d <- sample(c("A", "B", "C"), size = n, replace = TRUE)
#'
#' ya <- 0.2 + 0.5 * x1 + 0.01 * x2
#' yb <- 1.2 + 0.3 * x2
#' yc <- -1 - 0.2 * x1 + 0.5 * x2
#' y <- ifelse(d == "A", ya, ifelse(d == "B", yb, yc))
#' dt <- data.frame(y, d, x1, x2)
#'
#' # power analysis with variance assumption
#' tmp <- power_analysis(~d, dt, alpha = 0.05, power = 0.8, std_dev = 0.2)
#' rct_table(
#'   tmp,
#'   title = paste(
#'     "Power Analysis:",
#'     "Least Mean Difference to Keep 80% Power and 5% Significant Level"
#'   ),
#'   footnote = paste(
#'     "Note: To calculate mean difference,",
#'     "we assume that standard deviation of outcome is 0.2."
#'   ),
#'   output = "kableExtra"
#' )
#'
rct_table.power_analysis <- function(
  data, target = "unstd_d", digits = 3,
  title = NULL, output, footnote = NULL, size = 15,
  ...
) {
  data0 <- data[data$treat == levels(data$treat)[1], ]
  data0$treat <- droplevels(data0$treat)

  nprint <- function(x) sprintf("%1d", x)
  addrow <- modelsummary::datasummary(
    treat ~ (` ` = n1) * nprint + (` ` = d) * nprint,
    data = data0,
    output = "data.frame"
  )
  attr(addrow, "position") <- 1

  model <- switch(target,
    "d" = quote(
      (`Treatments` = treat) ~ (` ` = nprint) * (`N` = n1) +
        (` ` = tarprint) * (`ES` = d) * Arguments(digits = digits)
    ),
    "unstd_d" = quote(
      (`Treatments` = treat) ~ (` ` = nprint) * (`N` = n1) +
        (` ` = tarprint) * (`Mean difference` = unstd_effect) *
        Arguments(digits = digits)
    ),
    "alpha" = quote(
      (`Treatments` = treat) ~ (` ` = nprint) * (`N` = n1) +
        (` ` = tarprint) * (`Signigicance level` = alpha) *
        Arguments(digits = digits)
    ),
    "power" = quote(
      (`Treatments` = treat) ~ (` ` = nprint) * (`N` = n1) +
        (` ` = tarprint) * (`Power` = power) *
        Arguments(digits = digits)
    )
  )

  data1 <- data[data$treat != levels(data$treat)[1], ]
  data1$treat <- droplevels(data1$treat)

  tarprint <- function(x, digits = 3) sprintf(paste0("%1.", digits, "f"), x)
  tab <- modelsummary::datasummary(
    eval(model),
    data = data1,
    add_rows = addrow,
    title = title,
    output = output,
    align = "lcc"
  )

  if (output == "kableExtra") {
    tab %>%
      kableExtra::kable_styling(font_size = size, ...) %>%
      kableExtra::footnote(
        general_title = "",
        general = footnote,
        threeparttable = TRUE,
        escape = FALSE
      )
  } else if (output == "flextable") {
    tab %>%
      flextable::add_footer_lines(values = footnote) %>%
      flextable::fontsize(size = size, part = "all")
  } else if (output == "data.frame") {
    tab
  } else {
    stop(paste0(
      "You can pass 'kableExtra', 'flextable', and 'data.frame'",
      "in the output argument."
    ))
  }

}
