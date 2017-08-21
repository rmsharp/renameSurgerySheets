#' Make new sequenced filenames from full path names
#' 
#' @returns Character vector of properly sequenced file names.
#' @export
make_filenames <- function(file_list) {
  sapply(file_list, make_filename_A, simplify = "array")
}
