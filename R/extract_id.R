#' Extract animal ID from full path file name
#'
#' Uses several huristic techniques based on historical data to isolate the
#' animal ID.
#'
#' @return Character vector of length 1 having the date in \%m-\%d-\%Y format
#' extracted from the file name.
#' @param path_name character vector of length one having the path and basename
#' of the file.
#'
#' @import stringi
#' @export
extract_id <- function(path_name) {
  filename <- basename(path_name)
  space_ptr <- stri_locate_first_fixed(filename, " ")
  space_ptr <- space_ptr[[1, 2]]
  if (is.na(space_ptr)) {
    start <- stri_locate_all_fixed(filename, "-")
    start <- start[[1]][[length(start[[1]])]] + 1
  }
  else
    start <- space_ptr
  glp_ptr <- stri_locate_first_fixed(filename, '-glp')
  at_sign_ptr <- stri_locate_first_fixed(filename, '@')
  ns_ptr <- stri_locate_first_fixed(filename, 'NS')
  if (!is.na(glp_ptr[[1, 1]])) {
    start <- stri_locate_all_fixed(filename, "-")
    start <- start[[1]][[length(start[[1]]) - 1]] + 1
    stop <- glp_ptr[[1, 1]] - 1
  }
  else if (!is.na(at_sign_ptr[[1, 1]]))
    stop <- at_sign_ptr[[1, 1]] - 2
  else if (!is.na(ns_ptr[[1, 1]]))
    stop <- ns_ptr[[1, 1]] - 1
  else {
    period_ptr <- stri_locate_first_fixed(filename, ".pdf")
    stop <- period_ptr[[1,1]] - 1
  }
  id <- toupper(stri_trim_both(stri_sub(filename, start, stop)))
  return(id)
}
