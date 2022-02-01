#' Visualization of Empirical Analysis of RCTs
#'
#' @export
#'
#'
rct_plot <- function(data, ...) {
  UseMethod("rct_plot", data)
}

#' Visualization of T-test Result
#'
#' @method rct_plot t_test
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_test
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 coord_flip
#' @export
#'
#' @examples
#' ttest(y ~ d, data = ex, base = "B") %>%
#'   rct_plot(label = "{{mean}}({{p.value}})")
#'
rct_plot.t_test <- function(
  data, confint = FALSE,
  label = NULL, label_digits = 3,
  xlab = "Treatments", ylab = NULL,
  title = NULL, caption = NULL,
  text_size = 5, text_adjust = 0.2,
  font_family = NULL, flip = FALSE,
  ...
) {
  # errorbar
  z <- ifelse(confint, 1.96, 1)
  data$ymin <- data$mean - data$se * z
  data$ymax <- data$mean + data$se * z

  # label
  if (!is.null(label)) {
    data$label <- mapply(
      ttest_label_maker,
      data$mean, data$se, data$effect, data$p.value,
      MoreArgs = list(label = label, digits = label_digits)
    )
  }

  # plot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = treat, y = mean)) +
    ggplot2::geom_bar(
      stat = "identity", fill = "grey80", color = "black"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax), width = 0.5
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = 0)
    )

  if (!is.null(label)) {
    plot <- plot +
      ggplot2::geom_text(
        ggplot2::aes(y = ymax + text_adjust, label = label),
        size = text_size
      )
  }

  if (is.null(ylab)) {
    ylab <- ifelse(confint, "Average (+/- 95%CI)", "Average (+/- Std.Err)")
  }

  if (is.null(title)) title <- unique(data$outcome)

  plot <- plot +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      caption = caption
    )

  if (flip) plot <- plot + ggplot2::coord_flip()

  plot + simplegg(flip = flip, font_family = font_family, ...)
}
