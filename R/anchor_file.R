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

  if (!file.exists(fname)) {
    fname <- paste0("../", fname) # check upstairs
    if (!file.exists(fname)) {
      fname <- paste0("../", fname) # check upstairs
    }
  }
  if (!file.exists(fname)) stop("Can't find file XREFS.rda.")
  ext <- tools::file_ext(fname)
  if (ext == "rda") {
    load(fname)
    newtable <- XREFS
  } else if (ext == "rds") {
    newtable <- readRDS(fname)
  } else if (ext == "csv") {
    newtable <- readr::read_csv(fname)
  }

  set_anchor_table(
    dplyr::bind_rows(
      get_anchor_table(), newtable)
  )
}


#' @export
add_anchor_url <- function(url) {
  # Use add_anchor_file()
  # For a table stored at a URL or equivalent
}
#' @export
add_anchor_gsheet <- function(gsheet_id) {
  # For Google Sheets
}
