#' Returns FALSE if database connection is not avaiable.
#'
#' Currently this is a stub
#' @import testthat
#' @export
skip_if_no_files_mounted <- function() {
  path <- stri_c(get_directory("BASE"), "/", get_directory("UNPROCESSED"))
  all_files <- list.files(path = path, recursive = TRUE, pattern = '.pdf',
                          all.files = FALSE)
  if (length(all_files) <= 0)
    skip("Skipping: files not mounted.")
}
