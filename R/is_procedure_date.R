#' Is the date a procedure date for the specified animal
#'
#' Uses a simplisted database query to see if there is any type of clinical,
#' maintenance or research procedure of the indicated date.
#'
#' This should be updated to look for surgeries when possible.
#'
#' @param conn database connection object
#' @param id character vector of length one having the ID of the animal
#' @param date_str characer vector of length one having the date of the surgery.
#' @import RODBC
#' @import stringi
#' @export
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
