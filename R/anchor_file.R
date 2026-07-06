#' Provide the table of cross references
#'
#' The {xref} system scans HTML files to locate cross-reference
#' anchors and stores these anchors in a data frame. `xf()` looks
#' to this anchor data frame to resolve cross references, turning
#' the ID into a link and label.
#'


#' @param fname Name of rda file with anchor table (def: XREFS.rda)
#' @export
add_anchor_file <- function(fname = "XREFS.rda") {

  ext <- tools::file_ext(fname)
  if (ext == "rda") {
    load(fname)
    newtable <- XREFS # just the default name
  } else if (ext == "csv") {
    newtable <- readr::read_csv(fname)
  }

  .anchor_table. <<- # Global var
    dplyr::bind_rows(.anchor_table, newtable)
}

#' @export
add_anchor_url <- function(url) {
  # For a table stored at a URL or equivalent
}
#' @export
add_anchor_gsheet <- function(gsheet_id) {
  # For Google Sheets
}
