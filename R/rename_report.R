#' Date stamp report file.
#'
#' Looks for a report file with the name of \code{renameSurgerySheets.pdf}
#' and renames it with a call to rmsutilityr::get_dated_filename()
#' to a date stamped version of \code{Surgery Sheet Report.pdf}.
#'
#' @return Character vector of length one having the new date and time
#' stamped file name.
#' @param path character vector of length one containing the path to add to the
#' new file name.
#' @import rmsutilityr
#' @import stringi
#' @export
rename_report <- function(path = "") {
  old_name <- "renameSurgerySheets.pdf"
  new_name <- ""
  if (file.exists(old_name)) {
    new_name <- stri_c(path, get_dated_filename("Surgery_Sheet_Report.pdf"))
    status <- file.rename(old_name, new_name)
    if (!status)
      cat(stri_c("\nFailed to rename ", old_name, " to ", new_name, ".\n"))
  }
  new_name
}
