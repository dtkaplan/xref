# R/globals.R

# Create a private package environment
.pkg_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Define your initial data.frame
  .anchor_table. <- data.frame(stringsAsFactors = FALSE)


  # Create .anchor_table. in the .pkg_env
  assign(".anchor_table.",
         data.frame(stringsAsFactors = FALSE),
         envir = .pkg_env)
}

#' Get the global data frame
#' @export
get_anchor_table <- function() {
  get(".anchor_table.", envir = .pkg_env)
}

#' Update the global data frame
#' @export
set_anchor_table <- function(new_df) {
  if (!is.data.frame(new_df)) {
    stop("Input must be a data.frame")
  }
  assign(".anchor_table.", new_df, envir = .pkg_env)
}

