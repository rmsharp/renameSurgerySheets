#' Make new sequenced filename from full path name
#'
#' @returns Character vector of length one having the properly sequenced
#' file name.
#' @param path_name character vector of length one having the path and basename
#' of the file.
#' @param sequence_str character vector of length one having the characters
#' that indicate the chronological order of this particular file among the
#' other files for the same animal on the same date.
#' @import stringi
#' @export
make_filename <- function(path_name, sequence_str) {
  return(stri_c(extract_date(path_name), '_',
                sequence_str, '_', extract_id(path_name),
                '.pdf'))
}
