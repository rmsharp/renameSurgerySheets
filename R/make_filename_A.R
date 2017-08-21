#' Make initial sequenced filename from full path name
#' 
#' @returns Character vector of length one having the first sequenced 
#' file name, which contains the letter 'A' as the sequence position indicator.
#' @export
make_filename_A <- function(path_name) make_filename(path_name, 'A')
