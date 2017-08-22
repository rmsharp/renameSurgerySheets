#' Make initial sequenced filename from full path name
#'
#' @return Character vector of length one having the first sequenced
#' file name, which contains the letter 'A' as the sequence position indicator.
#' @param path_name character vector of length one having the path and basename
#' of the file.
#' @export
make_filename_A <- function(path_name) make_filename(path_name, 'A')
