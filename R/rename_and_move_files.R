#' Rename and move files
#' 
#' Takes a list of surgery sheet file names, looks to see if there is an
#' animal procedure on the date corresponding to the date indicated by the 
#' filename. If there is the file is copied to the appropriate year directory
#' and renamed.
#' 
#' @return A list containing \code{moved_files}, \code{bad_file_count},
#' \code{surgery_sheet_count}, and the \code{failed_to_copy_count}.
#' 
#' @import stringi
#' @export
rename_and_move_files <- function(conn, all_files) {
  moved_files <- character(length(all_files))
  bad_file_count <- 0
  surgery_sheet_count <- 0
  failed_to_copy_count <- 0
  if (length(all_files > 0)) {
    i <- 1
    for (file in all_files) {
      surgery_sheet_count <- surgery_sheet_count + 1
      if (is_surgery_sheet(conn, file)) {
        if (!copy_file(file)) {
          failed_to_copy_count <- failed_to_copy_count + 1
          print(stri_c(file, " failed to copy"))
        } else {
          move_file(file)
          moved_files[i] <- file
          i = i + 1
        }
      } else {
        print(stri_c(file, " is not a valid surgery sheet."))
        bad_file_count <- bad_file_count + 1
      }
    }
    moved_files <- moved_files[!moved_files == '']
  } 
  
  list(moved_files = moved_files, bad_file_count = bad_file_count, 
       surgery_sheet_count = surgery_sheet_count,
       failed_to_copy_count = failed_to_copy_count)
}
