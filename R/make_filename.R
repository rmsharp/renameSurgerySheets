#' Make new sequenced filename from full path name
#' 
#' @returns Character vector of length one having the properly sequenced 
#' file name.
#' @import stringi
#' @export
make_filename <- function(path_name, sequence_str) {
  return(stri_c(extract_date(path_name), '_',
                sequence_str, '_', extract_id(path_name),
                '.pdf'))
}
