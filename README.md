
[![Rdoc](http://www.rdocumentation.org/badges/version/roxygen2)](http://www.rdocumentation.org/packages/roxygen2) [![Build Status](https://travis-ci.org/rmsharp/renameSurgerySheets.svg?branch=master)](https://travis-ci.org/rmsharp/renameSurgerySheets) [![codecov](https://codecov.io/gh/rmsharp/renameSurgerySheetsr/branch/master/graph/badge.svg)](https://codecov.io/gh/rmsharp/renameSurgerySheets) [![Rdoc](http://www.rdocumentation.org/badges/version/RDocumentation)](http://www.rdocumentation.org/packages/RDocumentation) [![Rdoc](http://www.rdocumentation.org/badges/version/renameSurgerySheets)](http://www.rdocumentation.org/packages/gh/rmsharp/renameSurgerySheets) <!-- README.md is generated from README.Rmd. Please edit that file -->

renameSurgerySheets
===================

Purpose
-------

The purpose of renameSurgerySheets is to provide a set of R functions used to manage PDF versions of paper surgical sheets. These functions are used by \`\`renameSurgerySheets.Rnw'' to identify potential duplicate files, to copy files into the correct file system directories for use by database systems, to identify obvious file naming errors, and to rename the files to present the database functions with a uniform parsable file name that provides animal name and event date as metadata.

Installation
------------

You can install **renameSurgerySheets** from github with:

``` r
install.packages("devtools")
devtools::install_github("rmsharp/renameSurgerySheets")
```

All missing dependencies should be automatically installed.

Function
--------

### Renaming and copying files

A report is generated by running \`\`renameSurgerySheets.Rnw''. It is to describe the process and purpose of renaming the surgery sheets files that are in PDF format and to report the results of running this script, which copies the files from *//seldon/surgery data/unprocessed\_surgery\_sheets/* under the new file name to *//seldon/surgery data/CAMP/* and moves the successfully copied files to a directory named *//seldon/surgery data/SURGERY SHEETS YYYY* where YYYY is the year part of the surgery sheet date.

These files are to all be named with a common convention so that they can be found from within CAMP. The convention is *Id\_sequence\_letter\_YYYYMMDD.pdf*, where Id is the animal Id, an underscore character, a capitalized letter of the alphabet, an underscore character, and finally *YYYYMMDD* is the four character year, two numeral month, and two numeral day indicators. If there are more than one surgery sheet for an animal on any one day, the capitalized letter is incremented through the alphabet from A to Z. If there are more than 26 surgeries for an animal within one day, the capitalized letter will be replaced by an incremented integer starting with 27 and continuing with subsequent whole numbers up to 1000. At the 1001 sheet for the same animal on the same day, the program stops with an error message indicating what happened. Obviously, we do not anticipate using many of the letters much less the integers. Thus, the sequence would go as follow: 1X0123\_A\_20110203.pdf, 1X0123\_B\_20110203.pdf, ... , 1X0123\_Z\_20110203.pdf, 1X0123\_27\_20110203, 1X0123\_28\_20110203, ... , 1X0123\_1000\_20110203

### Listing potential duplicate files

This function identifies those files that have the same animal having more than one surgical sheet on the same day. Some of these are not duplicates, but as of this writting, it is a rare enough event to warrent manual examination.

### Detecting defective file names

File names are used to look up animal records on the date listed in the file name. If there is not a procedure on that date, the file is flagged as not matching data in the database.

Malformed dates are noted as having a date field that cannot be interpreted.

Use
---

The user of `renameSurgerySheets.Rnw'' will need to have a database DSN named **frogstar-vortex-animal**. This can be changed on line 114 of`renameSurgerySheets.Rnw''.

The function **get\_directory** defines BASE, UNPROCESSED and CAMP. Files are read from BASE/UNPROCESSED and written to BASE/CAMP. You can read the function **copy\_file** to see how these are used.

The user has to mount the file system containing the files on the same system that is running the script.

Inside a terminal session, go to the renameSurgerySheets/vignettes directory. For example,

    cd ~/Documents/Development/R/renameSurgerySheets/vignettes

Once you are in the `R` directory, the following two command lines will produce the PDF file `sedation.pdf`. *Note each of these commands typically generate several lines of text to the console*.

    Rscript -e "library(knitr); knit('renameSurgerySheets.Rnw')"
    pdflatex renameSurgerySheets.tex

The script *renameReport.R* worked from the command line of a terminal session that could be used with a cron job to create the report, time stamp the report name, and email it out. It will take a bit of customization to make it work for someone else. Search for TERRY and msharp to find places to edit.
