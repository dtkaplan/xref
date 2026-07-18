#' Extract instances of xf() calls from a set of qmd files.
#'
#' @returns A data frame where each row is one instance.
#'
#' @param file_vector String vector containing file names
#' @param A character string vector with the words to look for in each file

#' @rdname extract
#' @export
extract_from_qmd <- function(file_vector) {
  # Map over each file and process contents
  purrr::map_df(file_vector, function(f) {
    if (!file.exists(f)) return(NULL)

    # Read entire file as text lines
    lines <- readLines(f, warn = FALSE)

    # Regex to find `r xf_definition("...")` and capture the text inside quotes
    pattern <- '`r\\s+xf_definition\\("([^"]+)"\\)`'

    # Extract all matches from the file text
    matches <- stringr::str_match_all(lines, pattern)

    # Flatten the extracted capture groups
    terms <- purrr::keep(purrr::map(matches, ~ .x[, 2]), ~ length(.x) > 0) |> purrr::flatten_chr()

    # Return a data frame for this specific file
    if (length(terms) == 0) return(NULL)
    data.frame(word = terms, file = basename(f), stringsAsFactors = FALSE)
  })
}

#' @rdname extract
#' @export
find_terms <- function(file_vector, terms) {
  # Map over each file to process the content
  purrr::map_df(file_vector, function(f) {
    if (!file.exists(f)) return(NULL)

    # Read entire file and collapse into a single text string
    file_text <- paste(readLines(f, warn = FALSE), collapse = "\n")

    # Check each term against the file content
    purrr::map_df(terms, function(t) {
      # Escape special characters and enforce word boundaries
      # stringr::regex() with ignore_case = TRUE makes it case-insensitive
      pattern <- stringr::regex(
        paste0("\\b", stringr::str_escape(t), "\\b"),
        ignore_case = TRUE
      )

      # Locate all starting positions of matches to get the total count
      matches <- stringr::str_locate_all(file_text, pattern)[[1]]
      match_count <- nrow(matches)

      # If found, return a data frame with one row per instance
      if (!is.null(match_count) && match_count > 0) {
        return(data.frame(
          word = rep(t, match_count),
          file = rep(basename(f), match_count),
          use = rep("uses", match_count),
          stringsAsFactors = FALSE
        ))
      }
      return(NULL)
    })
  })
}

#' qmd_files <- list.files("/Users/kaplan/QR-courses/QR-A", pattern = "^Chap-[0-9]{2}-.*\\.qmd$", full.names = TRUE)[-16]
#' defines <- extract_from_qmd(qmd_files)
#' uses <- find_terms(qmd_files, defines$word)
#' defines$use <- "defines"
#' all <- dplyr::bind_rows(defines, uses) |> dplyr::arrange(tolower(word), file)
#' chap_count <- all |> summarize(count = n()-1, .by = word) |> arrange(desc(count))
