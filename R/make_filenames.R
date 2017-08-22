#' Make new sequenced filenames from full path names
#'
#' @returns Character vector of properly sequenced file names.
#'
#' @param file_list character vector of one or more having the path and basename
#' of a list of files to be renamed.
#' @export
make_filenames <- function(file_list) {
  sapply(file_list, make_filename_A, simplify = "array")
}
