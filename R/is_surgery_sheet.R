#' Is the file indicated a surgery sheet
#'
#' The filename is examined to see if it contains a valid animal ID and a
#' valid procedure date for that animal. If both are true, then the file is
#' determined to be a surgery sheet.
#'
#' The contents of the file are not examined in any way.
#' @return TRUE if the filename contains a valid procedure date and animal ID.
#' @param conn database connection object
#' @param file_path character vector of length one having the path and basename
#' of the file.
#' @import rmsutilityr
#' @export
is_surgery_sheet <- function(conn, file_path) {
  id <- blank_fill_ids(extract_id(file_path))
  date_str <- extract_date(file_path)
  return(is_animal(conn, id) & is_procedure_date(conn, id, date_str))
}
