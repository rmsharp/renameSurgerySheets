#' Extract date from full path file name
#' 
#' @returns Character vector of length 1 having the date in \%m-\%d-\%Y format
#' extracted from the file name.
#' @import stringi
#' @export
extract_date <- function(path_name) {
  filename <- basename(path_name)
  filename <- stri_replace_all_fixed(filename, "=", "-")
  filename <- stri_replace_all_fixed(filename, "--", "-")
  space_ptr <- stri_locate_first_fixed(filename, " ")
  space_ptr <- space_ptr[[1, 2]]
  if (!is.na(space_ptr))
    stop <- space_ptr - 1
  else {
    dash_ptr <- stri_locate_all_fixed(filename, '-')
    stop <- dash_ptr[[1]][[3]] - 1
  }
  date_str <- stri_trim_both(stri_sub(filename, 1, stop))
  if (is.na(any(stri_locate_first_fixed(stri_sub(date_str, stop - 3, stop), 
                                        c(' ', '-'))))) {
    date <- as.Date(date_str, format = "%m-%d-%Y")
  } else {
    date <- as.Date(date_str, format = "%m-%d-%y")
  }
  name_date_str <- strftime(date, format = "%Y%m%d")
  if (is.na(name_date_str))
    cat(stri_c(filename, " has a date field that cannot be interpreted.\n",
               " The full path is ", path_name))
  return(name_date_str)
}
