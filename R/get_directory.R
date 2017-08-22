#' Get name of directory base on directory type
#'
#' @param dir_type character vector of length one having the value of the index
#' to a specific directory type. Available types are "BASE", "CAMP", and
#' "UNPROCESSED".
#' @export
get_directory <- function(dir_type) {
  dirs <- c(
    BASE = "/Volumes/Surgery Data$",
    CAMP = "CAMP",
    UNPROCESSED = "unprocessed_surgery_sheets"
  )
  if (any(names(dirs) == dir_type)) {
    dirs[names(dirs) == dir_type]
  } else {
    stop(stri_c("'", dir_type, "' is an invalid directory type must be one of ",
                get_and_or_list(names(dirs), "or"), "."))
  }
}
