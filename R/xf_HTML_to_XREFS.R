#' Make the XREFS.rda file
#'

#'
#' In the xf cross-referencing system, which is an alternative
#' to the pandoc cross referencing, the author inserts markup like
#' `xf_definition("new word")` or `xf("fig-my-fig")`
#' Such commands insert content into the HTML files created by quarto.
#' These functions scan the HTML files and use the xf_ content to create an
#' index, by default called `XREFS.rda`, which provides the basis for `xf("fig-my-fig")` and such
#' The user-interface to this HTML to index translation is
#`` xref_html_files(save = TRUE)

#' @name make_xf_index

library("stringr")
library("tibble")

#' @param save Flag to save the index to a file. Only set to `FALSE` for debugging.
#' @rdname make_xf_index
#' @export

xref_html_files <- function(save = FALSE) {
  files <- dir("docs", pattern = "html$")
  XREFS <- lapply(paste0("docs/", files), function(x) xref_make_index(x)) |>
    dplyr::bind_rows()
  if (save) {
    save(XREFS, file="XREFS.rda")
  } else {
    warning("You aren't saving the xref index.")
    XREFS
  }

}

#' @rdname make_xf_index
#' @export

xref_make_index <- function(htmlname) {
  Lines <- readLines(con = htmlname)
  Lines <- gsub("&nbsp;", "", Lines)
  for_reference <- gsub("docs/", "", htmlname)
  One <- xref_sections_find(Lines)
  Two <- xref_figures_find(Lines)
  Three <- xref_exercises_find(Lines)
  Four <- xref_callouts_find(Lines)
  Five <- xref_fingers_find(Lines)
  Six <- xref_tables_find(Lines)

  Res <- dplyr::bind_rows(One, Two, Three, Four, Five, Six)
  Res$file <- for_reference

  Res$link <- with(Res, glue::glue("[{label}]({file}#{ID})"))

  Res
}

#' @rdname make_xf_index
#' @export

xref_fingers_find <- function(Lines) {
  keepers <- grep("<span id=\"finger-", Lines)
  Finger_lines <- Lines[keepers]
  # Sometimes there are muyltiple entries on a single line
  # so split them into multiple lines
  Finger_lines <- strsplit(Finger_lines, "</span") |> unlist()
  # But not all the split lines
  keepers <- grep("<span id=\"finger-", Finger_lines)
  Finger_lines <- Finger_lines[keepers]
  # Get the IDs
  Shorts <- gsub("^.*<span id=\"", "", Finger_lines)
  IDs <- gsub("\".*$", "", Shorts)
  # Get the labels
  Shorts <- gsub("^.*data-label=\"", "", Finger_lines)
  Shorts2 <- gsub("\"([a-zA-Z0-9 ]*)\".*$", "\1", Shorts)
  Shorts2 <- gsub('"><span.*$', "", Shorts2) # Just in case
  labels <- gsub("\".*$", "", Shorts2)

  tibble(label = labels, ID = IDs)
}

#' @rdname make_xf_index
#' @export

xref_sections_find <- function(Lines) {
  keepers <- grep("^<section", Lines)
  Section_lines <- Lines[keepers]
  Section_lines <- gsub("<section ", "", Section_lines)
  Section_lines <- gsub(">$", "", Section_lines)
  IDs <- str_match(Section_lines, 'id="([^ ]*)\\"')[,2]
  Classes <- str_match(Section_lines, "(level[0-9])")[,2]
  Number <- str_match(Section_lines, 'data-number="([^ ]*)\\"')[,2]
  Label <- ifelse(grepl("level1", Classes), "Chapter", "Section")
  Res <- tibble(label = paste(Label, Number), ID = IDs)


  Res

}

#' @rdname make_xf_index
#' @export

xref_figures_find <- function(Lines) {
  indices <- grep("^<figcaption", Lines)
  IDs <- str_match(Lines[indices], 'id="([^ ]*)\\"')[,2]
  IDs <- gsub("-caption-[0123456789abcdef-]{8,}", "", IDs)
  IDs <- tolower(IDs)
  labels <- gsub("&nbsp;", "", Lines[indices+1])
  # Get the label
  titles <- str_match(labels, '^((Figure|Table))')[,2]
  numbers <- str_match(labels, "([0-9.]{1,5})")[,2]

  tibble(label = paste(titles, numbers), ID = IDs)

}

#' @rdname make_xf_index
#' @export

xref_exercises_find <- function(Lines) {
  indices <- grep('<div id="exr-', Lines, fixed=TRUE)
  IDs <- str_match(Lines[indices], '<div id="(.*)" class?')[,2]
  missing <- is.na(IDs) # some are missing id names
  IDs <- IDs[!missing]
  indices <- indices[!missing]
  labels <- str_match(Lines[indices+1], "<strong>(.*)</strong")[,2]
  labels <- gsub("\\. ", ".", labels)

  tibble(label = labels, ID = IDs)
}

#' @rdname make_xf_index
#' @export

xref_tables_find <- function(Lines) {
  indices <- grep('<div id="tbl-', Lines, fixed=TRUE)
  IDs <- str_match(Lines[indices], '<div id="(.*)" class?')[,2]
  missing <- is.na(IDs) # some are missing id names
  IDs <- IDs[!missing]
  indices <- indices[!missing]
  # Get the table number
  label_lines <- Lines[indices + 3]
  label_lines <- gsub("&nbsp;", "", label_lines)
  labels <- gsub("^(Table [0-9\\.]*)*:.*$", "\\1", label_lines)
  #labels <- "Table"

  tibble(label = labels, ID = IDs)
}

#' @rdname make_xf_index
#' @export

xref_callouts_find <- function(Lines) {
  indices <- grep('class="callout ', Lines)
  IDs <- str_match(Lines[indices], '<div id="([^ ]*)"')[,2]
  missing <- is.na(IDs) # some are missing id names
  indices <- indices[!missing] # don't follow if there is no ID
  IDs <-IDs[!missing]

  if (length(IDs) == 0) return(NULL) # no labels with IDs

  # restrict the label search to the lines following the indices
  # the line I want is below the one that matches
  inrange_indices <- c(sapply(indices, function(x) x + 5:9))

  in_range_lines <- Lines[inrange_indices]
  has_label <- grep('<div class="callout-title-container', in_range_lines) + 1

  # get everything before the first colon-space
  labels <- gsub(": .*", "", in_range_lines[has_label])
  labels <- gsub("&nbsp;", ' ', labels)

  tibble(label = labels, ID = IDs)
}

# Compile a table of definition words and the file they are in.

