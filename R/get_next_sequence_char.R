#' Get next sequence character
#'
#' @return Next sequence character for making ordered filenames when one animal
#' has more than one surgery sheet per day. The first character returned is
#' the letter 'A' if there is not a filename for that animal on that date with
#' the letter A in sequence position.
#'
#' @param path_name character vector of length one having the path and basename
#' of the file.
#'
#' @import stringi
#' @export
get_next_sequence_char <- function(path_name) {
  for (i in 1:26) {
    if (file.exists(stri_c(stri_c(BASE_DIRECTORY, "/", CAMP_DIRECTORY, "/",
                                  make_filename(path_name, LETTERS[[i]])))))
      next
    else
      return(LETTERS[[i]])
  }
  for (i in 27:1000) {
    if (file.exists(stri_c('CAMP/', make_filename(path_name, stri_c(i)))))
      next
    else
      return(stri_c(i))
  }
  stop("Too many surgery sheets for the same animal on the same day.")
}
