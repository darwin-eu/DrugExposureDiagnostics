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
                        Port     = Sys.getenv("TESTDB_PORT"))
  cdm <- CDMConnector::cdm_from_con(con, cdm_schema = Sys.getenv("TESTDB_CDM_SCHEMA"))

  result <- executeChecks(cdm = cdm, ingredients = c(1125315), verbose = TRUE) #acetaminophen

  # checks
  expect_equal(length(result), 25)
  expect_true(all(grepl("acetaminophen", result$ingredientConcepts$concept_name)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
