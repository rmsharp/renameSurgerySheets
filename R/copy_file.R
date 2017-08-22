#' Copies file from source directory to the proper year directory
#'
#' Copies files from the unprocessed file directory in the the proper CAMP
#' directory based on date of surgery.
#' @param file character vector of length one having the basename of the file.
#' @import stringi
#' @export
copy_file <- function(file) {
  # Make sure I can read it
  status <- 1 + file.access(stri_c(get_directory("BASE"), "/",
                                   get_directory("UNPROCESSED"),
                                   "/", file), mode = 4)
  status <- as.logical(status)
  if (status) {
    status <- file.copy(stri_c(get_directory("BASE"), "/",
                               get_directory("UNPROCESSED"),
                               "/", file),
                        stri_c(get_directory("BASE"), "/",
                               get_directory("CAMP"),
                               "/", get_sheet_name(file)))
  }
  return(status)
}
