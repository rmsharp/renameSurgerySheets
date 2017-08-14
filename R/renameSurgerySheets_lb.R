#' renameSurgerySheets_lb.R
library(RODBC)
library(stringi)
library(animalr, quietly = TRUE)

extract_filename <- function(path_name) {
  slash_list <- stri_locate_all_fixed(path_name, "/")
  if (is.na(slash_list[[1]][1])) {
    slash_ptr <- 1
  } else {
    slash_ptr <- slash_list[[1]][[length(slash_list[[1]])]] + 1
  }
  return(stri_sub(path_name, slash_ptr))
}
extract_date <- function(path_name) {
  filename <- extract_filename(path_name)
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
  if (is.na(any(stri_locate_first_fixed(stri_sub(date_str, stop - 3, stop), c(' ', '-'))))) {
    date <- as.Date(date_str, format = "%m-%d-%Y")
  } else {
    date <- as.Date(date_str, format = "%m-%d-%y")
  }
  name_date_str <- strftime(date, format = "%Y%m%d")
  if (is.na(name_date_str))
    print(stri_c(filename, " has a date field that cannot be interpreted.\n",
                " The full path is ", path_name))
  return(name_date_str)
}
extract_id <- function(path_name) {
  filename <- extract_filename(path_name)
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
make_filename <- function(path_name, sequence_str) {
  return(stri_c(extract_date(path_name), '_',
               sequence_str, '_', extract_id(path_name),
               '.pdf'))
}

make_filename_A <- function(path_name) make_filename(path_name, 'A')

make_filenames <- function(file_list, sequence_str) {
  sapply(file_list, make_filename_A, simplify = "array")
}

next_sequence <- function(path_name) {
  for (i in 1:26) {
    if (file.exists(stri_c(stri_c(BASE_DIRECTORY, "/", CAMP_DIRECTORY, "/", 
                                make_filename(path_name, LETTERS[[i]])))))
      next
    else
      return(LETTERS[[i]])
  }
  for (i in 27:1000) {
    if (file.exists(stri_c('CAMP/', make_filename(path_name, stri_c(i)))))
      next
    else
      return(stri_c(i))
  }
  stop("Too many surgery sheets for the same animal on the same day.")
}

sheet_name <- function(path_name) {
  return(make_filename(path_name, next_sequence(path_name)))
}

is_animal <- function(conn, id) {
  result <- sqlQuery(conn, stri_c(
    "select id from master where id = '", id, "'"), errors = FALSE)
  if (length(result$id) == 1)
    return(TRUE)
  else {
    warning(stri_c("Animal '", id, "' does not exist within the ",
                  "animal database."))
    return(FALSE)
  }
}

is_procedure_date <- function(conn, id, date_str) {
  result <- sqlQuery(conn, stri_c(
    "select ae.animal_id  
    from ANIMAL_EVENTS ae
    where ae.animal_id = '", id, "' ",
    "    and convert(char(8), ae.event_date_tm, 112) = '",
    date_str,"'"), errors = FALSE)
  if (length(result$animal_id) >= 1)
    return(TRUE)
  else {
    warning(stri_c("Animal '", id, "' did not have a procedure on ",
                  date_str, "."))
    return(FALSE)
  }
}


is_surgery_sheet <- function(conn, file_path) {
  id <- blank_fill_ids(extract_id(file_path))
  date_str <- extract_date(file_path)
  return (is_animal(conn, id) & is_procedure_date(conn, id, date_str))
}

copy_file <- function(file) {
  # Make sure I can read it
  status <- 1 + file.access(stri_c(BASE_DIRECTORY, "/", 
                                  UNPROCESSED_SURGERY_SHEET_DIRECTORY, 
                                  "/", file), mode = 4)
  status <- as.logical(status)
  if (status) {
    status <- file.copy(stri_c(BASE_DIRECTORY, "/", 
                              UNPROCESSED_SURGERY_SHEET_DIRECTORY, 
                              "/", file), 
                        stri_c(BASE_DIRECTORY, "/", CAMP_DIRECTORY, 
                              "/", sheet_name(file)))
  }
  return(status)
}

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
list_duplicates <- function(all_files) {
  possible_duplicate <- FALSE
  all_files_cnt <- length(all_files)
  if (all_files_cnt >= 2) {
    paths <- list()
    all_files_cnt <- length(all_files)
    filenames <- make_filenames(all_files, "A")
    for (i in 1:(all_files_cnt - 1)) {
      first_filename <- make_filename(all_files[i], 'A')
      duplicates <- 1
      paths[duplicates] <- all_files[i]
      for (j in (i + 1):all_files_cnt) {
        if (filenames[j] == first_filename) {
          duplicates <- duplicates + 1
          paths[duplicates] <- all_files[j]
        }
      }
      if (duplicates > 1) {
        cat("The following files are potential duplicates.")
        for (duplicate in 1:duplicates) {
          cat(stri_c('        ', paths[[duplicate]], ".\\"))                                                                                                                       
        }
        posible_duplicate <- TRUE
      }
    }      
  }
  if (!possible_duplicate) { 
    cat("There are no duplicates.\\")
  }
  cat("\\")
}
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
        if(!copy_file(file)) {
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
