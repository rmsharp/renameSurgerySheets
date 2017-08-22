#' Make surgery sheet file name from initial from full path file name
#'
#' @returns Character vector of length one having properly sequence file name.
#' @param path_name character vector of length one having the path and basename
#' of the file.
#' @export
get_sheet_name <- function(path_name) {
  return(make_filename(path_name, get_next_sequence_char(path_name)))
}