#' Is ID an animal ID
#' 
#' @returns TRUE if the \code{id} argument represents an animal ID.
#' 
#' @param conn database connection object
#' @param id character vector of length one having the ID of the animal
#' 
#' @import RODBC
#' @import stringi
#' @export
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
