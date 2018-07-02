#' Returns FALSE if database connection is not avaiable.
#'
#' Currently this is a stub
#' @import testthat
#' @export
skip_if_no_db <- function() {
  skip("Skipping: database not available.")
}
