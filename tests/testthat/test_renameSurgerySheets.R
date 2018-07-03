library(rmsutilityr, quietly = TRUE)
library(RODBC, quietly = TRUE)
library(stringi)

pn_1 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2010/08-09-10   16550.pdf"
pn_2 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2011/3-15-11  19111.pdf"
pn_3 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2010/10-22-10 19231.pdf"
pn_4 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2009/5-11--09  19105.pdf"
pn_5 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2006/08-17--06  27795.pdf"
pn_6 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2006/06--27-06  10126.pdf"
pn_7 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2011/2-7-11 4x0293.pdf"
pn_8 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2011/3-25-11 13788 @ 10.45am.pdf"
pn_9 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2007/8-31-07-29008.pdf"
pn_10 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2007/8-2-07-16859.pdf"
pn_11 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2007/7-11-07-16287-glp.pdf"
pn_12 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2007/11-1-07-17977.pdf"
pn_13 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2008/6-03-08-9395NS.pdf"
pn_14 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2008/10-16--08-29710.pdf"
pn_15 <- "/Volumes/Surgery Data$//SURGERY SHEETS 2005/10-07=05 17254.pdf"
pn_16 <- "10-08=5 17254.pdf"
pn_17 <- "/Volumes/Surgery Data$//URGERY SHEETS 2007/1-12-2007-14379.pdf"

context("renameSurgerySheets")
test_that("extract_date() works", {
  expect_equal(extract_date(pn_1), "20100809")
  expect_equal(extract_date(pn_2), "20110315")
  expect_equal(extract_date(pn_3), "20101022")
  expect_equal(extract_date(pn_4), "20090511")
  expect_equal(extract_date(pn_5), "20060817")
  expect_equal(extract_date(pn_6), "20060627")
  expect_equal(extract_date(pn_7), "20110207")
  expect_equal(extract_date(pn_8), "20110325")
  expect_equal(extract_date(pn_9), "20070831")
  expect_equal(extract_date(pn_10), "20070802")
  expect_equal(extract_date(pn_11), "20070711")
  expect_equal(extract_date(pn_12), "20071101")
  expect_equal(extract_date(pn_13), "20080603")
  expect_equal(extract_date(pn_14), "20081016")
  expect_equal(extract_date(pn_15), "20051007")
  expect_equal(extract_date(pn_16), "20051008")
  expect_equal(extract_date(pn_17),"20070112")
})
test_that("extract_id() works", {
  expect_equal(extract_id(pn_1), "16550")
  expect_equal(extract_id(pn_2), "19111")
  expect_equal(extract_id(pn_3), "19231")
  expect_equal(extract_id(pn_4), "19105")
  expect_equal(extract_id(pn_5), "27795")
  expect_equal(extract_id(pn_6), "10126")
  expect_equal(extract_id(pn_7), "4X0293")
  expect_equal(extract_id(pn_8), "13788")
  expect_equal(extract_id(pn_9), "29008")
  expect_equal(extract_id(pn_10), "16859")
  expect_equal(extract_id(pn_11), "16287")
  expect_equal(extract_id(pn_12), "17977")
  expect_equal(extract_id(pn_13), "9395")
  expect_equal(extract_id(pn_14), "29710")
  expect_equal(extract_id(pn_15), "17254")
  expect_equal(extract_id(pn_16), "17254")
})
test_that("get_sheet_name() works", {
  skip_if_no_files_mounted()
  expect_equal(get_sheet_name(pn_1), "20100809_B_16550.pdf")
  expect_equal(get_sheet_name(pn_2), "20110315_B_19111.pdf")
  expect_equal(get_sheet_name(pn_3), "20101022_A_19231.pdf")
  expect_equal(get_sheet_name(pn_4), "20090511_B_19105.pdf")
  expect_equal(get_sheet_name(pn_5), "20060817_B_27795.pdf")
  expect_equal(get_sheet_name(pn_6), "20060627_B_10126.pdf")
  expect_equal(get_sheet_name(pn_7), "20110207_B_4X0293.pdf")
  expect_equal(get_sheet_name(pn_8), "20110325_C_13788.pdf")
  expect_equal(get_sheet_name(pn_9), "20070831_B_29008.pdf")
  expect_equal(get_sheet_name(pn_10), "20070802_B_16859.pdf")
  expect_equal(get_sheet_name(pn_11), "20070711_B_16287.pdf")
  expect_equal(get_sheet_name(pn_12), "20071101_B_17977.pdf")
  expect_equal(get_sheet_name(pn_13), "20080603_B_9395.pdf")
  expect_equal(get_sheet_name(pn_14), "20081016_B_29710.pdf")
  expect_equal(get_sheet_name(pn_15), "20051007_B_17254.pdf")
  expect_equal(get_sheet_name(pn_16), "20051008_A_17254.pdf")
})

test_that("blank_fill_ids() works", {
  expect_equal(blank_fill_ids("123"), "   123")
  expect_equal(blank_fill_ids("1234"), "  1234")
  expect_equal(blank_fill_ids("12345"), " 12345")
  expect_equal(blank_fill_ids("4x1234"), "4X1234")
  expect_equal(blank_fill_ids("123456"), "123456")
  expect_that(blank_fill_ids("1"), gives_warning())
})

test_that("is_procedure_date() works", {
  skip_on_travis()
  skip_if_no_db()
  conn <- odbcConnect('frogstar-vortex-animal')
  expect_true(is_procedure_date(conn, ' 16550', '20100809'))
  expect_that(is_procedure_date(conn, ' 16550', '20110809'), gives_warning())
  odbcClose(conn)
})
test_that("is_surgery_sheet() works", {
  skip_on_travis()
  skip_if_no_db()
  conn <- odbcConnect('frogstar-vortex-animal')
  expect_true(is_surgery_sheet(conn, pn_1))
  odbcClose(conn)
})
test_that("is_animal() works", {
  skip_on_travis()
  skip_if_no_db()
  conn <- odbcConnect('frogstar-vortex-animal')
  expect_that(is_animal(conn, '12345'), gives_warning())
  expect_true(is_animal(conn, ' 12345'))
  odbcClose(conn)
})
test_that("rename_report() works", {
  skip_on_travis()
  old_name <- "renameSurgerySheets.pdf"
  if (!file.exists(old_name)) {
    file.create(old_name)
  }
  expected_name <- get_dated_filename("Surgery_Sheet_Report.pdf")
  new_name <- rename_report()
  expect_equal(expected_name, new_name)
  result <- tryCatch({
    file.remove(old_name)
  }, warning = function(w) {
    NULL #cat(paste0(w, "\n")) ## Never seen during testing.
  }, error = function(e) {
    NULL #cat(paste0(e, "\n")) ## If you want to see what happens
  }, finally = {NULL}
  )
  file.remove(new_name)
})

