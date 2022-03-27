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
#' with a class supported by rcttable (power_analysis).
#' You can check the arguments that can be passed in
#' the power_analysis class by `help(rcttable.power_analysis)`,
#' in the balance_test class by `help(rcttable.balance_test)`,
#' in the RCT_OLS class by `help(rcttable.rct_lm)`.
#'
#'
rcttable <- function(...) {
  UseMethod("rcttable")
}

#' Output Table for Power Analysis
#'
#' @description This function outputs
#' the result of power analysis in tabular form.
#' For tabular output,
#' this function uses the `datasummary` function of the {modelsummary} package
#' that corresponds to various outputs.
#' `rcttable.power_analysis` supports "kableExtra" (for PDF and HTML output),
#' "flextable" (MS Word and MS Powerpoint), and "data.frame".
#'
#' @param obj object with R6 class "RCTtoolbox.power.analysis"
#' @param treat.label character. Label of treatment column.
#' @param target character. Specify which value to output.
#' If "d", the effect size is output.
#' If "diff_mean" (default), the non-standarized effect size
#' (i.e., absolute value of mean difference) is output.
#' If "alpha", the significant level is output.
#' If "power", the power is output.
#' @param target.digits numeric.
#' Specify the number of decimal places to display (Default is 3).
#' @param target.label character. Label of output `tar` column.
#' @param title character. Table title.
#' @param footnote character. Footnote (kableExtra or flextable).
#' @param size numeric. Font size (kableExtra or flextable).
#' @param output character. Output format.
#' @param \dots Other arguments to pass to `kableExtra::kable_styling`
#'
#' @method rcttable RCTtoolbox.power.analysis
#' @importFrom modelsummary datasummary
#' @importFrom magrittr %>%
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra footnote
#' @importFrom flextable add_footer_lines
#' @importFrom flextable fontsize
#' @importFrom tables Heading
#' @importFrom tables Format
#'
#' @examples
#' \dontrun{
#' data(RubellaNudge)
#' rct <- create_RCTtoolbox(
#'   atest + avacc ~ treat,
#'   ~ age + educ,
#'   RubellaNudge,
#'   LETTERS[1:7]
#' )
#'
#' rct$power(alpha = 0.05, power = 0.8)$table()
#'
#' }
#'
rcttable.RCTtoolbox.power.analysis <- function(obj,
                                               treat.label = "Treatments",
                                               target = "diff_mean",
                                               target.digits = 3,
                                               target.label = "Mean Difference",
                                               title = NULL,
                                               footnote = NULL,
                                               size = 12,
                                               output = "kableExtra",
                                               ...) {
  # shape passed data
  data <- obj$result
  usedt <- data[, c("arms", "n1", target)]
  colnames(usedt) <- c("d", "n", "tage")

  # basic datasummary
  rawvalue <- function(x) x
  tab <- modelsummary::datasummary(
    Heading(treat.label, character.only = TRUE) * d ~
      (` ` = rawvalue) * (
        Heading("N") * n * Format(digits = 0) +
        Heading(target.label, character.only = TRUE) * tage *
        Format(digits = target.digits)
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
#' `rcttable.power_analysis` supports "kableExtra" (for PDF and HTML output),
#' "flextable" (MS Word and MS Powerpoint), and "data.frame".
#'
#' @param data data.frame with class "balance_test"
#' @param treat.label character. Label of treatment column.
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
#' @method rcttable RCTtoolobox.balance.test
#'
#' @examples
#' \dontrun{
#' data(RubellaNudge)
#' rct <- create_RCTtoolbox(
#'   atest + avacc ~ treat,
#'   ~ age + educ,
#'   RubellaNudge,
#'   LETTERS[1:7]
#' )
#'
#' rct$balance(subset = coupon == 1)$table()
#'
#' }
#'
rcttable.RCTtoolbox.balance.test <- function(obj,
                                             treat.label = "Treatments",
                                             digits = 3,
                                             title = NULL,
                                             footnote = NULL,
                                             size = 12,
                                             output = "kableExtra",
                                             ...) {
  data <- obj$result
  data <- data[!(data$item %in% c("fstat", "df1", "df2")), ]
  data$item <- droplevels(data$item)

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

  header_val <- c(" ", treat.label, " ")
  header_col <- c(1, numarms, 1)

  if (output == "kableExtra") {
    header <- header_col
    names(header) <- header_val

    tab %>%
      kableExtra::kable_styling(font_size = size, ...) %>%
      kableExtra::add_header_above(header) %>%
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
        values = header_val,
        colwidths = header_col
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
#' @method rcttable RCTtoolbox.lm
#'
#'
#' @examples
#' \dontrun{
#' data(RubellaNudge)
#' rct <- create_RCTtoolbox(
#'   atest + avacc ~ treat,
#'   list(~age, ~ age + educ),
#'   RubellaNudge,
#'   LETTERS[1:7],
#'   letters[1:7]
#' )
#'
#' rct$
#'   lm(subset = coupon == 1)$
#'   est$table(
#'     add_coef_map = c("age" = "Age"),
#'     outcome_map = c("atest" = "Test", "avacc" = "Vaccination"),
#'     not_show_x = list(Covariates = c("educ")),
#'     digits = 4,
#'     keep_gof = "Num.Obs.|R2"
#'   )
#' }
#'
rcttable.RCTtoolbox.lm <- function(object,
                                   dvar,
                                   add_coef_map = NULL,
                                   outcome_map = NULL,
                                   not_show_x = NULL,
                                   keep_gof = "Num.Obs.|R2|R2 Adj.",
                                   digits = 3,
                                   stars =
                                     c("***" = .01, "**" = .05, "*" = .1),
                                   title = NULL,
                                   footnote = NULL,
                                   output = "kableExtra",
                                   size = 12,
                                   ...) {
  res <- object$result

  # Step1: Header of Outcome Labels
  yvec <- unlist(lapply(res, function(x) all.vars(f_lhs(formula(x$terms)))))
  ylab <- c(1, rle(yvec)$length)
  names(ylab) <- c(" ", rle(yvec)$values)
  if (!is.null(outcome_map)) {
    # rename labels
    for (i in seq_len(length(outcome_map))) {
      pat <- names(outcome_map)[i]
      newlab <- outcome_map[i]
      names(ylab)[pat == names(ylab)] <- newlab
    }
  }

  # Step2: create coefficient mapping
  # covariate list
  xvec <- lapply(res, function(x) {
    v <- all.vars(f_rhs(formula(x$terms)))
    v[-grep(dvar, v)]
  })
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
  tab <- subset(tab, x %out% names(add_coef_map))

  # create flag whether it can be grouped
  x <- NULL
  if (!is.null(not_show_x)) {
    flag <- NULL
    for (i in seq_len(length(not_show_x))) {
      if (sum(not_show_x[[i]] %in% names(add_coef_map)) > 0) {
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

  # create coef_map
  dvec <- lapply(res, function(x) {
    v <- all.vars(f_rhs(formula(x$terms)))
    v[grep(dvar, v)]
  })
  dvec <- unique(unlist(dvec))

  coef_map <- gsub(dvar, "", dvec)
  names(coef_map) <- dvec
  coef_map <- c(coef_map, add_coef_map)

  for (i in seq_len(ncol(tab) - 1)) {
    tab[, 1 + i] <- ifelse(tab[, 1 + i] == 1, "X", "")
  }

  # Step 3: Set model names and align
  names(res) <- paste0("(", seq_len(length(res)), ")")
  align <- paste0(c("l", rep("c", length(res))), collapse = "")

  # Step 4: basic output
  tab <- modelsummary::modelsummary(
    res,
    coef_map = coef_map,
    gof_omit = paste0("[^", keep_gof, "]"),
    stars = stars,
    align = align,
    add_rows = tab,
    title = title,
    output = output,
    fmt = digits
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
