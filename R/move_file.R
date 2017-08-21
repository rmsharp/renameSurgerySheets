#' Move file
#' 
#' Extracts the date from the file name to use in identifying which year
#' directory to move the file to. The file is then moved and any errors that
#' ensue are reported.
#' 
#' @import stringi
#' @export
move_file <- function(file) {
  year_str <- stri_sub(extract_date(file), 1, 4)
  oldfilename <- stri_c(BASE_DIRECTORY, "/", UNPROCESSED_SURGERY_SHEET_DIRECTORY, 
                        "/", file)
  subdirectory <- stri_c(BASE_DIRECTORY,"/SURGERY SHEETS ", year_str, "/") 
  if (!file.exists(subdirectory))
    dir.create(subdirectory)
  newfilename <- stri_c(subdirectory, file)
  status <- file.rename(oldfilename, newfilename)
  if (status) {
    return(status)
  } else {
    print(stri_c(file, " failed to move to"))
    print(newfilename) # newline for long names
    return(status)
  }
}
