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
  dirs[dirs == dir_type]
}
