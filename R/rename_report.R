#' Date stamp report file.
#' 
#' Looks for a report file with the name of \code{renameSurgerySheets.pdf}
#' and renames it with a call to rmsutilityr::get_dated_filename()
#' to a date stamped version of \code{Surgery Sheet Report.pdf}. 
#' 
#' @import rmsutilityr
#' @export
rename_report <- function() {
  old_name <- "renameSurgerySheets.pdf"
  new_name <- ""
  if (file.exists(old_name)) {
    new_name <- get_dated_filename("Surgery_Sheet_Report.pdf")
    status <- file.rename(old_name, new_name)
    if (!status) 
      cat(stri_c("\nFailed to rename ", old_name, " to ", new_name, ".\n"))
  }
  new_name
}
