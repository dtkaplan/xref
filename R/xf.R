#' Refer to a cross-reference
#'
#' In Quarto, you create cross-references with markup like
#' `label: fig-my-id`, and reference the location using the
#' notation `@fig-my-id`. Unfortunately, this makes it hard
#' to do cross-referencing between different projects or files
#' within a project (unless "book" mode for Quarto is being used).
#'
#' With `xf()`, you reference using R syntax: `r xf("fig-my-id")`.
#'
#' @param id Character string naming the ID to make a cross reference to, e.g. "fig-my-id".
#' @param just_number Logical (default FALSE) indicating whether the use
#' the full title of the cross-reference or just the "number."
#' @param label Character string. Disregard the cross-reference title and use this label instead.
#' @rdname xf
#' @export
xf <- function(id, just_number = FALSE, label = NULL) {
  if (nrow(xf:::.anchor_table.) == 0) {
    warning("No anchor tables loaded. See add_anchor_file().")
    return(paste("No xref anchor file given.", id))
  }
  id <- tolower(id)
  id <- gsub("^@", "", id) # Remove @ just in case it was used in the ID
  this_one <- xf:::.anchor_table. |>
    filter(ID == id)
  text <- if (nrow(this_one) == 0) {
    paste("UNRESOLVED", id)
  } else if (nrow(this_one) > 1) {
    paste("MULTIPLE DEFS of", id)
  } else {
    if (just_number) {
      # Just keep the number
      gsub("\\[[a-zA-Z]* ", "[", this_one$link)
    } else {
      # if a label has been given explicitly, use it.
      if (is.null(label)) {
        this_one$link
      } else {
        gsub("\\[.*\\]", paste0("[", label, "]"), this_one$link)
      }
    }
  }

  text
}

#' @rdname xf
#' @export
xf <- function(id, suppress_label = FALSE, label = NULL) {
  if (!exists("XREFS")) load("XREFS.rda", envir = parent.frame(n = 2))
  id <- tolower(id)
  id <- gsub("^@", "", id) # Remove @ just in case it was used in the ID
  this_one <- XREFS |>
    filter(ID == id)
  text <- if (nrow(this_one) == 0) {
    paste("UNRESOLVED", id)
  } else {
    this_one <- this_one[1,] # in case there are duplicates. Need something BETTER!!

    if (suppress_label) {
      # Just keep the number
      gsub("\\[[a-zA-Z]* ", "[", this_one$link)
    } else {
      # if a label has been given explicitly, use it.
      if (!is.null(label)) {
        gsub("\\[.*\\]", paste0("[", label, "]"), this_one$link)
      } else {
        this_one$link
      }
    }
  }

  text
}

#' @rdname xf
#' @export
xf_anchor <- function(ID, label="unlabelled") {
  glue::glue('[ ☜]{{id="finger-{ID}" data-label="{label}"}}')
}

# Creates an ID entry for the <word>, which should be in cannonical form.
# The optional <text> argument provides an override for the printed version.
# Example: xf_definition("rate", "Rates") while place Rates in the output but use "rate"
# as the cannonical form.
#' @rdname xf
#' @export
xf_definition <- function(word, text = word){
  word2 <- tolower(gsub(" ", "-", word))
  if (knitr::is_html_output()) { # for HTML
    glue::glue('☞ **{text}**[ ☜]{{id="finger-{word2}-definition" data-label="{word}"}}')
  } else { # for PDF
    glue::glue('![](www/finger-right.png){{width=0.35cm}}[**{text}**]{{id="finger-{word2}-definition" data-label="{word}"}}![](www/finger-left.png){{width=0.35cm}}')
  }
}

# A cross-reference to a previous definition
#' @rdname xf
#' @export
xf_to_def <- function(word, label = word) {
  word <- tolower(word) # Just in case
  word <- gsub(" ", "-", word)
  # if a label was given, use
  xf(glue::glue("finger-{word}-definition"), label = label)
}

#' @rdname xf
#' @export
xf_new_words <- function(chapter) {
  if (!exists("XREFS")) load("XREFS.rda", envir = parent.frame(n = 2))
  chap_name <- glue::glue("Chap-{chapter}")
  refs_in_chapter <- XREFS |> dplyr::filter(grepl(chap_name, file))
  definitions <- refs_in_chapter |>
    dplyr::filter(grepl("-definition", ID))

  wordset = sort(definitions$link)
  if (knitr::is_html_output()) {
    paste(paste("- [ ] ", sort(definitions$link)), collapse = "\n\n")
  } else {
    wordset = gsub(" ", "~", wordset)
    paste(sort(definitions$link), collapse = " : :  ")
  }

}
