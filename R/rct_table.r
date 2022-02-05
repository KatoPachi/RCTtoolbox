#' Output Table of Empirical Analysis for RCTs
#'
#' @description This is an S3 generic function
#' for creating table of RCT analysis.
#' RCTtoolbox provides a method for power_analysis
#' (generated by the power_analysis function),
#' balance_test
#' (generated by the balance_test function), and
#' RCT_OLS
#' (generated by the rct_lm function).
#'
#' @param \dots Specify the arguments to pass to this function.
#' The first argument must be a dataframe
#' with a class supported by rct_table (power_analysis).
#' You can check the arguments that can be passed in
#' the power_analysis class by `help(rct_table.power_analysis)`,
#' in the balance_test class by `help(rct_table.balance_test)`,
#' in the RCT_OLS class by `help(rct_table.rct_lm)`.
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
#'
#' # Balance test with two or more covariates
#' btest <- balance_test(x1 + x2 ~ d, dt)
#'
#' # table output of power analysis
#' library(modelsummary)
#' rct_table(
#'   tmp,
#'   title = paste(
#'     "Power Analysis:",
#'     "Least Mean Difference to Keep 80% Power and 5% Significant Level"
#'   ),
#'   footnote = paste(
#'     "Note: To calculate mean difference,",
#'     "we assume that standard deviation of outcome is 0.2."
#'   )
#' )
#'
#' # Output table of balance test
#' rct_table(btest, title = "Balance Test")
#'
#' # Run regression
#' est <- rct_lm(y ~ d, xmod = list(~ x1 + x2), data = dt)
#' rct_table(
#'   est,
#'   outcome_map = c("y" = "outcome"),
#'   coef_map = c("dB" = "State B", "dC" = "State C"),
#'   not_show_x = list("Covariates" = c("x1", "x2"))
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
#' @param tar character. Specify which value to output.
#' If "d", the effect size is output.
#' If "unstd_effect" (default), the non-standarized effect size
#' (i.e., absolute value of mean difference) is output.
#' If "alpha", the significant level is output.
#' If "power", the power is output.
#' @param dlab character. Label of treatment column.
#' @param tardigits numeric.
#' Specify the number of decimal places to display (Default is 3).
#' @param tarlab character. Label of output `tar` column.
#' @param title character. Table title.
#' @param footnote character. Footnote (kableExtra or flextable).
#' @param size numeric. Font size (kableExtra or flextable).
#' @param output character. Output format.
#' @param \dots Other arguments to pass to `kableExtra::kable_styling`
#'
#' @method rct_table power_analysis
#' @importFrom modelsummary datasummary
#' @importFrom magrittr %>%
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra footnote
#' @importFrom flextable add_footer_lines
#' @importFrom flextable fontsize
#' @importFrom tables Heading
#' @importFrom tables Format
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
#'
#' # table output of power analysis
#' library(modelsummary)
#' rct_table(
#'   tmp,
#'   title = paste(
#'     "Power Analysis:",
#'     "Least Mean Difference to Keep 80% Power and 5% Significant Level"
#'   ),
#'   footnote = paste(
#'     "Note: To calculate mean difference,",
#'     "we assume that standard deviation of outcome is 0.2."
#'   )
#' )
#'
rct_table.power_analysis <- function(
  data, tar = "unstd_effect",
  dlab = "Treatments", tardigits = 3, tarlab = "Mean Difference",
  title = NULL, footnote = NULL,
  size = 15, output = "kableExtra",
  ...
) {
  # shape passed data
  usedt <- data[, c("treat", "n1", tar)]
  colnames(usedt) <- c("d", "n", "tar")

  # basic datasummary
  rawvalue <- function(x) x
  tab <- modelsummary::datasummary(
    Heading(dlab, character.only = TRUE) * d ~ (` ` = rawvalue) * (
      Heading("N") * n * Format(digits = 0) +
      Heading(tarlab, character.only = TRUE) * tar *
      Format(digits = tardigits)
    ),
    data = usedt,
    title = title,
    output = output,
    align = "lcc"
  )

  # footnote and font size for kableExtra or flextable
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
  } else {
    tab
  }

}

#' Output Table for Balance Test
#'
#' @description This function outputs
#' the result of balance test in tabular form.
#' For tabular output,
#' this function uses the `datasummary` function of the {modelsummary} package
#' that corresponds to various outputs.
#' `rct_table.power_analysis` supports "kableExtra" (for PDF and HTML output),
#' "flextable" (MS Word and MS Powerpoint), and "data.frame".
#'
#' @param data data.frame with class "balance_test"
#' @param digits numeric.
#' Specify the number of decimal places to display (Default is 3).
#' @param title character. Table title.
#' @param output character. Output format.
#' @param footnote character. Footnote.
#' @param size numeric. Font size.
#' @param \dots Other arguments to pass to `kableExtra::kable_styling`
#'
#' @importFrom modelsummary datasummary
#' @importFrom magrittr %>%
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra add_header_above
#' @importFrom kableExtra footnote
#' @importFrom flextable add_footer_lines
#' @importFrom flextable add_header_row
#' @importFrom flextable fontsize
#'
#' @method rct_table balance_test
#' @export
#'
#' @examples
#' # DGP
#' set.seed(120511)
#' n <- 1000
#' x1 <- rnorm(n); x2 <- rnorm(n)
#' d <- sample(c("A", "B", "C"), size = n, replace = TRUE)
#' ya <- 0.2 + 0.5 * x1 + 0.01 * x2
#' yb <- 1.2 + 0.3 * x2
#' yc <- -1 - 0.2 * x1 + 0.5 * x2
#' y <- ifelse(d == "A", ya, ifelse(d == "B", yb, yc))
#' dt <- data.frame(y, d, x1, x2)
#'
#' # Balance test with two or more covariates
#' btest <- balance_test(x1 + x2 ~ d, dt)
#'
#' # Output table
#' library(modelsummary)
#' rct_table(btest, title = "Balance Test")
#'
rct_table.balance_test <- function(
  data, digits = 3,
  title = NULL, output = "kableExtra",
  footnote = NULL, size = 15,
  ...
) {
  # number of experimental arms
  numarms <- length(unique(data$item)) - 1
  align <- paste0(c("l", rep("c", numarms + 1)), collapse = "")

  # datasummary
  rawvalue <- function(x) x
  tab <- modelsummary::datasummary(
    (` ` = x) ~ (` ` = rawvalue) * val * item,
    data = data,
    fmt = digits,
    align = align,
    title = title,
    output = output
  )

  if (output == "kableExtra") {
    tab %>%
      kableExtra::kable_styling(font_size = size, ...) %>%
      kableExtra::add_header_above(
        c(" " = 1, "Treatments" = numarms, " " = 1)
      ) %>%
      kableExtra::footnote(
        general_title = "",
        general = footnote,
        threeparttable = TRUE,
        escape = FALSE
      )
  } else if (output == "flextable") {
    tab %>%
      flextable::add_footer_lines(values = footnote) %>%
      flextable::add_header_row(
        values = c("", "Treatments", ""),
        colwidths = c(1, numarms, 1)
      ) %>%
      flextable::fontsize(size = size, part = "all")
  } else {
    tab
  }

}

#' Output Table for Regression Analaysis
#'
#' @param object object with RCT_OLS class.
#' @param coef_map character vector with name.
#' Specify `c("Original variable" = "label", ...)`.
#' @param outcome_map character vector with name.
#' Specify `c("Original variable" = "label", ...)`.
#' @param not_show_x list.
#' You can specify how to display information about
#' whether you are controlling a variable that is not displayed by a factor.
#' For example, `list("label 1" = "x1")` changes the label of "x1" to "label 1".
#' Also, when specified as `list("label" = c ("x1", "x2"))`,
#' if x1 and x2 are not controlled or
#' the coefficients are not displayed in all models,
#' "x1" and "x2" labels are aggregated into one line with the name "label".
#' @param keep_gof character.
#' Which regression statistics (e.g. number of observations and R-squares)
#' to keep?
#' @param digits numeric.
#' Specify the number of decimal places to display (Default is 3).
#' @param stars vector. Indicator of p-value.
#' If `c("**" = 0.05, "***" = 0.01)`,
#' then return `***` if p-value <= 0.01,
#' and return `**` if 0.01 < p-value <= 0.05.
#' If `stars = FALSE`, no stars are output.
#' @param title character. Title of table.
#' @param footnote character. Table footnote.
#' @param output character. Output format.
#' @param size numeric. Font size (default is 15).
#' @param \dots other arguments passing in kableExtra::kable_styling.
#'
#' @importFrom modelsummary modelsummary
#' @importFrom magrittr %>%
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra add_header_above
#' @importFrom kableExtra footnote
#' @importFrom flextable add_footer_lines
#' @importFrom flextable add_header_row
#' @importFrom flextable fontsize
#'
#' @method rct_table RCT_OLS
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
#'
#' dt <- data.frame(y, d, x1, x2)
#'
#' # Run regression
#' est <- rct_lm(y ~ d, xmod = list(~ x1 + x2), data = dt)
#' rct_table(
#'   est,
#'   outcome_map = c("y" = "outcome"),
#'   coef_map = c("dB" = "State B", "dC" = "State C"),
#'   not_show_x = list("Covariates" = c("x1", "x2"))
#' )
#' 
rct_table.RCT_OLS <- function(
  object, coef_map,
  outcome_map = NULL, not_show_x = NULL,
  keep_gof = "Num.Obs.|R2|R2 Adj.",
  digits = 3,
  stars = c("***" = .01, "**" = .05, "*" = .1),
  title = NULL, footnote = NULL,
  output = "kableExtra", size = 15,
  ...
) {
  # Step1: Header of Outcome Labels
  yvec <- unlist(lapply(object$model, function(x) all.vars(x)[1]))
  ylab <- c(1, rle(yvec)$length)
  names(ylab) <- c(" ", rle(yvec)$values)
  if (!is.null(outcome_map)) {
    # rename labels
    for (i in seq_len(length(outcome_map))) {
      names(ylab)[grep(names(outcome_map)[i], names(ylab))] <- outcome_map[i]
    }
  }

  # Step2: create add_rows about missing covariates
  # covariate list
  xvec <- lapply(object$model, function(x) all.vars(x)[- (1:2)])
  tab <- data.frame(x = unique(unlist(xvec)))

  # which covariates each model includes
  for (i in seq_len(length(xvec))) {
    tab[, i + 1] <- apply(
      as.matrix(tab[, 1], ncol = 1),
      MARGIN = 1, function(x) sum(xvec[[i]] == x)
    )
  }

  # remove covariates if coef_map includes
  "%out%" <- Negate("%in%")
  tab <- subset(tab, x %out% coef_map)

  # create flag whether it can be grouped
  x <- NULL
  if (!is.null(not_show_x)) {
    flag <- NULL
    for (i in seq_len(length(not_show_x))) {
      if (sum(not_show_x[[i]] %in% names(coef_map)) > 0) {
        flag[i] <- 0
      } else {
        ctrl <- apply(subset(tab, x %in% not_show_x[[i]])[, -1], 2, sum)
        flag[i] <- ifelse(
          sum(0 < ctrl & ctrl < length(not_show_x[[i]])) != 0, 0, 1
        )
      }
    }

    # replace grouped covariates into group label
    for (i in seq_len(length(flag))) {
      if (flag[i] == 1) {
        bool <- apply(
          subset(tab, x %in% not_show_x[[i]])[, -1],
          2, function(x) 1 * (sum(x) > 0)
        )
        newtab <- data.frame(x = names(not_show_x)[i], t(bool))
        tab <- subset(tab, x %out% not_show_x[[i]])
        tab <- dplyr::bind_rows(tab, newtab)
      }
    }
  }

  for (i in seq_len(ncol(tab) - 1)) {
    tab[, 1 + i] <- ifelse(tab[, 1 + i] == 1, "X", "")
  }

  # Step 3: Set model names and align
  names(object$res) <- paste0("(", seq_len(length(object$res)), ")")
  align <- paste0(c("l", rep("c", length(object$res))), collapse = "")

  # Step 4: basic output
  tab <- modelsummary::modelsummary(
    object$res,
    coef_map = coef_map,
    gof_omit = paste0("[^", keep_gof, "]"),
    stars = stars,
    align = align,
    add_rows = tab,
    title = title
  )
  # Step 5: additional options for kabelExtra and flextable
  if (output == "kableExtra") {
    tab %>%
      kableExtra::kable_styling(font_size = size, ...) %>%
      kableExtra::add_header_above(ylab) %>%
      kableExtra::footnote(
        general_title = "",
        general = footnote,
        threeparttable = TRUE,
        escape = FALSE
      )
  } else if (output == "flextable") {
    tab %>%
      flextable::add_footer_lines(values = footnote) %>%
      flextable::add_header_row(values = names(ylab), colwidths = ylab) %>%
      flextable::fontsize(size = size, part = "all")
  } else {
    tab
  }
}
