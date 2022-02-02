#' Visualization of Empirical Analysis of RCTs
#'
#' @description This is an S3 generic function for visualizing RCT analysis.
#' RCTtoolbox provides a method for t_test (generated by the ttest function).
#'
#' @param \dots Specify the arguments to pass to this function.
#' The first argument must be a dataframe
#' with a class supported by rct_plot (t_test).
#' You can check the arguments that can be passed in
#' the t_test class with `help(rct_plot.t_test)`.
#'
#' @export
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
#' # plot of result of t-test
#' ttest(y ~ d, dt) %>%
#'   rct_plot(
#'     label = "{{mean}}[{{effect}}{{star}}]",
#'     title = "Simulated Outcome"
#'   )
#'
rct_plot <- function(...) {
  UseMethod("rct_plot")
}

#' Visualization of T-test Result
#'
#' @description This is a function that visualizes
#' the result of the t-test executed by the `ttest` function with {ggplot2}.
#'
#' @param data tibble or data.frame with "t_test" class.
#' @param confint logical.
#' Specify whether error bars represent 95% confidence intervals.
#' @param label character. See details.
#' @param label_digits numeric. See details.
#' @param xlab character. Specify title of x-axis.
#' Default is "Treatments".
#' @param ylab character. Specify title of y-axis.
#' Default is NULL.
#' If NULL, the title of y-axis is "Average (+/- Std.Err)" or
#' "Average (+/- 95%CI)".
#' @param title character. Specify title of plot.
#' Default is NULL.
#' If NULL, the label of outcome variable is plot title.
#' @param caption character. Specify caption of plot.
#' @param text_size numeric. Sepecify font size of label.
#' Default is 5.
#' @param text_adjust numeric.
#' Specify a value that corrects the label position along the y-axis.
#' @param font_family character. Specify font family. Default is NULL.
#' @param flip logical. Specify whether to swap the x-axis and y-axis.
#' Default is FALSE.
#' @param \dots other arugments passing `simplegg`.
#'
#' @details By using the `label` argument,
#' the result of the t-test can be embedded in the plot as text.
#' The result of the t-test can be specified by \{\{type\}\}.
#' If \{\{mean\}\}, the mean value of each group is embedded.
#' If \{\{se\}\}, the standard error of the mean of each group is embedded.
#' If \{\{effect\}\}, the effect of each group is embedded.
#' If \{\{pvalue\}\}, the p-value of each group effect is embedded.
#' If \{\{star\}\}, the p-value star of each group effect is embedded.
#' Combining these four, you specify the output of the text.
#' For example, if the mean is 0.5 and its standard error is 0.1,
#' then specifying "\{\{mean\}\}(\{\{se\}\})" will embed "0.5(0.1)" in the plot.
#' The font size of label can by specified by `text_size` argument.
#' The default is 5.
#'
#' @method rct_plot t_test
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 coord_flip
#' @importFrom forcats fct_rev
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
#' # plot of result of t-test
#' ttest(y ~ d, dt) %>%
#'   rct_plot(
#'     label = "{{mean}} [{{effect}}{{star}}]",
#'     title = "Simulated Outcome"
#'   )
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
    data$labpos <- ifelse(
      data$ymax < 0,
      data$ymax - text_adjust,
      data$ymax + text_adjust
    )
  }

  # plot
  if (flip) {
    plot <- ggplot2::ggplot(
      data, ggplot2::aes(x = forcats::fct_rev(treat), y = mean)
    )
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = treat, y = mean))
  }

  plot <- plot +
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
        ggplot2::aes(y = labpos, label = label),
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