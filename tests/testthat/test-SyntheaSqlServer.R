library(testthat)
library(dplyr, warn.conflicts = FALSE)

test_that("test methods against test server", {
  skip_if(Sys.getenv("TESTDB_USER") == "")

  con <- DBI::dbConnect(odbc::odbc(),
    Driver   = Sys.getenv("TESTDB_DRIVER"),
    Server   = Sys.getenv("TESTDB_SERVER"),
    Database = Sys.getenv("TESTDB_NAME"),
    UID      = Sys.getenv("TESTDB_USER"),
    PWD      = Sys.getenv("TESTDB_PWD"),
    Port     = Sys.getenv("TESTDB_PORT"),
    bigint = c("numeric")
  )
  cdm <- CDMConnector::cdmFromCon(con,
                                  cdmSchema = Sys.getenv("TESTDB_CDM_SCHEMA"),
                                  writeSchema = Sys.getenv("TESTDB_WRITE_SCHEMA"))

  result <- executeChecks(cdm = cdm,
                          ingredients = c(1307863), #verapamil
                          checks = DrugExposureDiagnostics:::getAllCheckOptions(),
                          sample = 1000,
                          verbose = TRUE)

  # checks
  expect_equal(length(result), 23)
  expect_true(all(grepl("verapamil", result$ingredientConcepts$concept_name)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
