#' Create cross-reference anchors
#'
#' The {xref} system makes use of cross-reference anchors
#' embedded in document files. In Quarto, these are created with
#' labels like `#fig-my-id`. These functions allow you to create them
#' from R.
#'
#' @export
anchor_definition <- function(word, for_show = word){
  word2 <- tolower(gsub(" ", "-", word))
  if (knitr::is_html_output()) {
  # for HTML
    glue::glue('☞ **{for_show}**[ ☜]{{id="finger-{word2}-definition" data-label="{word}"}}')
  } else {
  # for PDF
    glue::glue('$\\Rightarrow$**{for_show}**[$\\Leftarrow$]{{id="finger-{word2}-definition" data-label="{word}"}}')
  }
}

