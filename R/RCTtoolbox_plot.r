# Visualization of RCTtoolbox
rctplot <- function(...) {
  UseMethod("rctplot")
}

#
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr case_when
#' @importFrom glue glue
#' @importFrom rlang enquos
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 theme
#' @importFrom patchwork wrap_plots
#' @importFrom patchwork plot_layout
#' @importFrom forcats fct_rev
#'
rctplot.RCTtoolbox.ttest <- function(obj,
                                     zval = 1,
                                     xlab = "Experimental Arms",
                                     ylab = "Average (+/- Std.Err.)",
                                     title = NULL,
                                     caption = NULL,
                                     inplot_lab_format =
                                       "{round(mean1, 3)}{pstar}",
                                     inplot_lab_size = 5,
                                     inplot_lab_adjust = 0,
                                     ylim = NULL,
                                     flip = FALSE,
                                     patchwork = TRUE,
                                     ncol = NULL,
                                     nrow = NULL,
                                     family = getOption("RCTtoolbox.plot_family"),
                                     ...) {
  mean1 <- se1 <- arms <- ymin <- ymax <- label <- NULL
  # plot data
  pdt <- obj$result %>%
    mutate(pstar = case_when(
      pval <= .01 ~ "***",
      pval <= .05 ~ "**",
      pval <= .1 ~ "*",
      TRUE ~ ""
    ))

  method <- unique(pdt$method)
  if (method == "Welch t-test") {
    pdt <- pdt %>%
      mutate(
        ymin = mean1 - se1 * zval,
        ymax = mean1 + se1 * zval
      )
  }

  if (!is.null(inplot_lab_format)) {
    pdt <- pdt %>% mutate(label = glue(inplot_lab_format))
  }

  # plot arguments
  args <- enquos(
    xlab = xlab,
    ylab = ylab,
    title = title,
    caption = caption,
    inplot_lab_adjust = inplot_lab_adjust,
    ylim = ylim
  )

  # plot list
  outcome <- unique(pdt$outcome)
  plist <- lapply(outcome, function(z) {
    input <- subset(pdt, outcome == z)
    args <- lapply(args, eval_tidy, list(outcome = z))
    inplot_lab_pos <- if (method == "Welch t-test") {
      max(input$ymax) + inplot_lab_adjust
    } else {
      max(input$mean1) + inplot_lab_adjust
    }

    if (flip) {
      p <- ggplot(input, aes(x = fct_rev(arms), y = mean1))
    } else {
      p <- ggplot(input, aes(x = arms, y = mean1))
    }

    p <- p +
      geom_bar(stat = "identity", fill = "grey80", color = "black") +
      geom_hline(aes(yintercept = 0))

    if (method == "Welch t-test") {
      p <- p +
        geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.5)
    }

    if (!is.null(input$label)) {
      p <- p +
        geom_text(
          aes(y = inplot_lab_pos, label = label),
          size = inplot_lab_size, family = family
        )
    }

    if (!is.null(ylim)) p <- p + ylim(args$ylim)

    p <- p +
      labs(
        x = args$xlab,
        y = args$ylab,
        title = args$title,
        caption = args$caption
      )

    if (flip) p <- p + coord_flip()

    p + simplegg(flip = flip, font_family = family, ...)
  })

  # patchwork
  if (patchwork) {
    wrap_plots(plist, ncol = ncol, nrow = nrow) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
  } else {
    plist
  }
}