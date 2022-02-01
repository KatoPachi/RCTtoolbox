#' Making label for Visualization
#'
#'
ttest_label_maker <- function(label, mean, se, effect, p, digits = 3) {
  decompose <- strsplit(label, "\\{\\{|\\}\\}")[[1]]
  textpart <- decompose[grep("[^'']", decompose)]
  numeric_format <- paste0("%1.", digits, "f")

  textpart[grep("mean", textpart)] <- sprintf(numeric_format, mean)
  textpart[grep("se", textpart)] <- sprintf(numeric_format, se)
  textpart[grep("effect", textpart)] <- sprintf(numeric_format, effect)
  textpart[grep("p.value", textpart)] <- sprintf(numeric_format, p)
  textpart[grep("star", textpart)] <- dplyr::case_when(
    p < .01 ~ "***",
    p < .05 ~ "**",
    p < .1 ~ "*",
    TRUE ~ ""
  )

  paste0(textpart, collapse = "")
}