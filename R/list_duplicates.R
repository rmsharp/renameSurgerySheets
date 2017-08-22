#' List any possible file duplicates based on animal name and date.
#'
#' This is only a list of possible duplicates. It is possible to have more than
#' one PDF generated as a surgery sheet per data for an animal, but it is
#' rare enough to have someone ensure the contents of the PDFs are infact
#' different.
#' @param all_files character vector with file names of surgery sheets.
#'
list_duplicates <- function(all_files) {
  files_df <- data.frame(org_name = all_files, new_name = make_filenames)
  files_df <- files_df[order(files_df$new_name), ]
  dups_df <- files_df[duplicated(files_df$new_name), ]
  if (nrow(dups_df) > 0) {
    dups_df <- dups_df[!duplicated(dups_df$new_name), ]
    result <- data.frame()
    for (file in dups_df$new_name) {
      result <- rbind(result, dups_df[dups_df$new_name == file, ])
    }

  } else {
    result <- "There are no duplicates."
  }
  result
}
