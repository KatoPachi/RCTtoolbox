# Class: RCTtoolbox.power.analysis
#'
#' @importFrom modelsummary datasummary
#' @importFrom magrittr %>%
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra footnote
#' @importFrom flextable add_footer_lines
#' @importFrom flextable fontsize
#' @importFrom tables Heading
#' @importFrom tables Format
#'
rcttable.RCTtoolbox.power.analysis <- function(obj,
                                               treat.label = "Treatments",
                                               target = "diff_mean",
                                               target.digits = 3,
                                               target.label = "Mean Difference",
                                               title = NULL,
                                               footnote = NULL,
                                               size = getOption("RCTtoolbox.table_font_size"),
                                               output = getOption("RCTtoolbox.table_output"),
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

# Class: RCTtoolbox.balance.test
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
rcttable.RCTtoolbox.balance.test <- function(obj,
                                             treat.label = "Treatments",
                                             digits = 3,
                                             title = NULL,
                                             footnote = NULL,
                                             size = getOption("RCTtoolbox.table_font_size"),
                                             output = getOption("RCTtoolbox.table_output"),
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

# Class: RCTtoolbox.lm
#'
#' @importFrom modelsummary modelsummary
#' @importFrom magrittr %>%
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra add_header_above
#' @importFrom kableExtra footnote
#' @importFrom flextable add_footer_lines
#' @importFrom flextable add_header_row
#' @importFrom flextable fontsize
#' @importFrom broom tidy
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
                                   size = getOption("RCTtoolbox.table_font_size"),
                                   output = getOption("RCTtoolbox.table_output"),
                                   ...) {
  res <- object$result

  # Step1: Header of Outcome Labels
  yvec <- unlist(lapply(res, function(x) unique(tidy(x)$outcome)))
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
    v <- tidy(x)$term
    v <- v[-grep(dvar, v)]
    v <- v[-grep("Intercept", v)]
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
  tab$x <- substr(tab$x, 2, nchar(tab$x))
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
    v <- tidy(x)$term
    v[grep(dvar, v)]
  })
  dvec <- unique(unlist(dvec))

  coef_map <- gsub(paste0("x", dvar), "", dvec)
  names(coef_map) <- dvec
  if (!is.null(add_coef_map)) {
    names(add_coef_map) <- paste0("x", names(add_coef_map))
  }
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

# Class: RCTtoolbox.chi2test
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr recode
#' @importFrom modelsummary datasummary
#' @importFrom tables Format
#' @importFrom magrittr %>%
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra add_header_above
#' @importFrom kableExtra footnote
#' @importFrom flextable add_footer_lines
#' @importFrom flextable add_header_row
#' @importFrom flextable fontsize
rcttable.RCTtoolbox.chi2test <- function(object,
                                         levels,
                                         show.n0 = TRUE,
                                         outcome_label = NULL,
                                         value_label = NULL,
                                         p.digits = 3,
                                         size = 15,
                                         footnote = NULL,
                                         output = "kableExtra",
                                         ...) {
  res <- object$result
  rawvalue <- function(x) x

  if (res$method$type == "t") {
    # create data frame
    ctab <- NULL
    for (y in names(res$result)) {
      list_ctab <- lapply(names(res$result[[y]]), function(x) {
        tab <- data.frame(t(res$result[[y]][[x]]$tab))
        tab$p <- ifelse(tab$d == "Treat", res$result[[y]][[x]]$p, NA)
        tab$d <- ifelse(tab$d == "Treat", x, as.character(tab$d))
        tab$outcome <- y
        tab
      })
      part_ctab <- bind_rows(list_ctab)
      part_ctab <- part_ctab[!duplicated(part_ctab), ]
      ctab <- rbind(ctab, part_ctab)

    }

    if (!show.n0) {
      ctab <- ctab[ctab$y == 1, ]
      ctab$y <- droplevels(ctab$y)
    }

    ctab$d <- factor(ctab$d, levels)
    if (!is.null(outcome_label)) {
      ctab$outcome <- dplyr::recode(ctab$outcome, !!!outcome_label)
    }
    if (!is.null(value_label)) {
      ctab$y <- dplyr::recode(ctab$y, !!!value_label)
    }

    # datasummary
    align <- paste0(
      c("l", rep_len("c", (3 - !show.n0) * length(names(res$result)))),
      collapse = ""
    )

    tabu <- datasummary(
      (`Treatments` = d) ~ outcome * (
        (` ` = Freq) * (` ` = rawvalue) * Format(digits = 0) * (` ` = y) +
          p * (`p-value` = rawvalue)
      ),
      ctab,
      fmt = p.digits,
      align = align
    )

    if (!is.null(footnote)) footnote <- paste("Note:", footnote)

    if (output == "kableExtra") {
      tabu %>%
        kableExtra::kable_styling(font_size = size, ...) %>%
        kableExtra::footnote(
          general_title = "",
          general = footnote,
          threeparttable = TRUE,
          escape = FALSE
        )
    } else if (output == "flextable") {
      tabu %>%
        flextable::add_footer_lines(values = footnote) %>%
        flextable::fontsize(size = size, part = "all")
    } else {
      tabu
    }

  } else if (res$method$type == "f") {
    # create data frame
    list_ctab <- lapply(names(res$result), function(x) {
      data.frame(
        t(res$result[[x]]$tab),
        outcome = x
      )
    })
    ctab <- bind_rows(list_ctab)

    if (!show.n0) {
      ctab <- ctab[ctab$y == 1, ]
      ctab$y <- droplevels(ctab$y)
    }

    ctab$d <- factor(ctab$d, levels)
    if (!is.null(outcome_label)) {
      ctab$outcome <- dplyr::recode(ctab$outcome, !!!outcome_label)
    }
    if (!is.null(value_label)) {
      ctab$y <- dplyr::recode(ctab$y, !!!value_label)
    }

    # create footnote of test result
    foot <- NULL
    for (i in names(res$result)) {
      outcome <- if (!is.null(outcome_label)) {
        dplyr::recode(i, !!!outcome_label)
      } else {
        i
      }

      p <- res$result[[i]]$p

      pfmt <- paste("p = {round(p,", p.digits, ")}.")
      fmt <- paste("{fisher} (Outcome: {outcome}):", pfmt)

      if (is.null(foot)) {
        foot <- glue(fmt)
      } else {
        foot <- glue(paste("{foot}", fmt))
      }
    }

    # datasummary
    align <- paste0(
      c("l", rep_len("c", (2 - !show.n0) * length(names(res$result)))),
      collapse = ""
    )

    tabu <- datasummary(
      (`Treatments` = d) ~ outcome * (` ` = Freq) * (` ` = rawvalue) * y,
      ctab,
      fmt = 0,
      align = align
    )

    fnote <- paste("Note:", foot)
    if (!is.null(footnote)) fnote <- paste(fnote, footnote)

    if (output == "kableExtra") {
      tabu %>%
        kableExtra::kable_styling(font_size = size, ...) %>%
        kableExtra::footnote(
          general_title = "",
          general = fnote,
          threeparttable = TRUE,
          escape = FALSE
        )
    } else if (output == "flextable") {
      tabu %>%
        flextable::add_footer_lines(values = fnote) %>%
        flextable::fontsize(size = size, part = "all")
    } else {
      tabu
    }

  }
}