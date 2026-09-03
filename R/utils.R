#' Utilities for {xref} system
#'
#'
#'
quarto_target <- function() {
  knitr::is_html_output()
  if (grepl("html", format)) "html"
  else if (grepl("pdf", format)) "pdf"
  else NA
}

