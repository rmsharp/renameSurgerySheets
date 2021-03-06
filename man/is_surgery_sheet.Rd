% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_surgery_sheet.R
\name{is_surgery_sheet}
\alias{is_surgery_sheet}
\title{Is the file indicated a surgery sheet}
\usage{
is_surgery_sheet(conn, file_path)
}
\arguments{
\item{conn}{database connection object}

\item{file_path}{character vector of length one having the path and basename
of the file.}
}
\value{
TRUE if the filename contains a valid procedure date and animal ID.
}
\description{
The filename is examined to see if it contains a valid animal ID and a
valid procedure date for that animal. If both are true, then the file is
determined to be a surgery sheet.
}
\details{
The contents of the file are not examined in any way.
}
