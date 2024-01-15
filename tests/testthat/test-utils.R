test_that("check summary calculation", {
  startTime <- Sys.time()
  result <- printDurationAndMessage(message = "testMessage", start = startTime)

  expect_true(!is.null(result))
  expect_equal(class(result), class(startTime))
  expect_true((result - startTime) > 0)
})

# Check if a zip file has been created, extract it and check csv
checkResultOnDisk <- function(resultList, outFolder, filename) {
  outputZipFile <- list.files(outFolder, full.names = TRUE, pattern = paste0(filename, ".zip"))
  expect_true(endsWith(outputZipFile, paste0(filename, ".zip")))
  outputCsvFolder <- file.path(outFolder, "csv")
  zip::unzip(zipfile = outputZipFile,  exdir = outputCsvFolder)
  unzippedFolder <- file.path(outputCsvFolder, "test")
  outputCsvFiles <- list.files(unzippedFolder, pattern = "*.csv")
  expect_equal(outputCsvFiles, c("item1.csv", "item2.csv"))
  item1Path <- file.path(unzippedFolder, "item1.csv")
  item1 <- read.csv(item1Path)
  expect_equal(class(item1), "data.frame")
  expect_equal(colnames(item1), c("database_id", colnames(resultList[["item1"]])))
  item2Path <- file.path(unzippedFolder, "item2.csv")
  item2 <- read.csv(item2Path)
  expect_equal(class(item2), "data.frame")
  expect_equal(colnames(item2), c("database_id", colnames(resultList[["item2"]])))
}

test_that("check writeResultToDisk", {
  outFolder <- tempdir()
  resultList <- list("item1" = mtcars,
                     "item2" = iris)

  dbId <- "test"
  filename <- paste0(c(dbId,
                       "DrugDiagnostics",
                       format(Sys.Date(), format="%Y%m%d")),
                     collapse = "_")

  result <- writeResultToDisk(resultList = resultList,
                              databaseId = dbId,
                              outputFolder = outFolder,
                              filename = filename)
  checkResultOnDisk(resultList, outFolder, filename)

  # no filename given, default to 'dbId'.zip
  result <- writeResultToDisk(resultList = resultList,
                              databaseId = dbId,
                              outputFolder = outFolder)

  checkResultOnDisk(resultList, outFolder, dbId)
  unlink(outFolder)
})

test_that("checkIsIngredient", {
  messageStore <- checkmate::makeAssertCollection()
  cdm <- mockDrugExposure()

  checkIsIngredient(cdm = cdm, conceptId = 1125315, messageStore = messageStore)
  expect_true(messageStore$isEmpty())

  # concept id does not exist
  messageStore <- checkmate::makeAssertCollection()
  checkIsIngredient(cdm = cdm, conceptId = 123456789, messageStore = messageStore)
  expect_true(!messageStore$isEmpty())
  expect_equal(messageStore$getMessages()[2], "- ingredient concept (123456789) could not be found in concept table")

  # concept id has wrong class
  messageStore <- checkmate::makeAssertCollection()
  checkIsIngredient(cdm = cdm, conceptId = 19133768, messageStore = messageStore)
  expect_true(!messageStore$isEmpty())
  expect_equal(messageStore$getMessages()[2], "- ingredient concept (19133768) does not have concept_class_id of Ingredient")


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
  })
