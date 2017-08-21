library(knitr)
knit("renameSurgerySheets.Rnw")
source("renameSurgerySheets_lb.R")
cat(rename_report())
