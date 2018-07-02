#!/Library/Frameworks/R.\ framework/Versions/3.4/Resources/bin/Rscript
library(knitr)
library(renameSurgerySheets)
library(stringi)
if (stri_detect_fixed(Sys.info()[["nodename"]], "benjy")) {
  ## Make sure it is not already mounted
  if (!file.exists("/Volumes/Surgery Data$/unprocessed_surgery_sheets")) {
    system("open ~msharp/scripts/connectToSeldonSurgeryData.app")
    Sys.sleep(60) # Give the system time to connect.
    if (!file.exists("/Volumes/Surgery Data$/unprocessed_surgery_sheets"))
      stop("Surgery Data file system did was not successfully mounted.")
  }
}

args <- commandArgs(trailingOnly = TRUE)
cat(stri_c(args, "\n"), file = "args.txt")
rnw_file <- args[1]
tex_file <- stri_c(stri_sub(rnw_file, 1, stri_length(rnw_file) - 3), "tex")
knit(rnw_file)
system(stri_c("pdflatex ", tex_file, "\n"))
report_name <- rename_report(path = "../reports/")
cat(report_name, file = "args.txt", append = TRUE)
library(mailR)
from <- "thawkins@TxBiomed.org"
to <- c("Debbie Christian <debbiec@txbiomed.org>", "thawkins@txbiomed.org")
cc <- c("Karen Rice <krice@txbiomed.org>", "Scott Rouse <srouse@txbiomed.org>")
subject <- "Surgery Sheet Renaming Report"
body <- stri_c("Attached is the most recent surgery sheet renaming report (",
               report_name, ").\n\n",
              "Contact R. Mark Sharp (ext. 9476; msharp@txbiomed.org) ",
              "if you have any questions.\n\n")
encoding <- "iso-8859-1" # Character encoding to use for the email.
                         # Supported encodings include iso-8859- 1 (default),
                         # utf-8, us-ascii, and koi8-r.
html <- FALSE # A boolean indicating whether the body of the email should
              # be parsed as HTML.
inline <- FALSE # A boolean indicating whether images in the HTML file
                # should be embedded inline.
smtp <- list(host.name = "mail.txbiomed.org", port = 25, user.name = "thawkins",
             passwd = "TERRYPASSWORD", tls = TRUE)
authenticate = TRUE # A boolean variable to indicate whether authorization is
                    # required to connect to the SMTP server.
                    # # If set to true, see details on parameters required
                    # in smtp parameter.
attach_files <- report_name # A character vector of paths in the file system
                            # linking to files or *valid* URLs to be attached
                            # # to the email (see details for more info on
                            # attaching URLs)
debug <- FALSE
send <- FALSE
send.mail(from = from, to = to, cc = cc, subject = subject, body = body,
          html = html, inline  = inline,
          smtp = smtp, authenticate = authenticate,
          send = send, attach.files = attach_files, debug = debug)
