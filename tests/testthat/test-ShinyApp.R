library(testthat)
library(shiny)
library(R6)

test_that("ShinyApp", {
  skip_if_not(ableToRun()$shiny)

  cdm <- mockDrugExposure()
  resultList <- DrugExposureDiagnostics::executeChecks(
    cdm = cdm,
    ingredients = 1125315,
    checks = c("missing", "exposureDuration", "type", "route", "sourceConcept", "daysSupply", "verbatimEndDate", "dose", "sig", "quantity"),
    minCellCount = 5,
    earliestStartDate = "2000-01-01"
  )

  app <- DrugExposureDiagnostics:::ShinyApp$new(
    resultList = resultList,
    database_id = "Eunomia"
  )

  # Fields
  expect_false(is.null(app$namespace))
  expect_false(is.null(app$moduleId))
  expect_equal(app$moduleName, "ShinyApp")
  expect_false(is.null(app$instanceId))

  # UI
  expect_s3_class(app$UI(), "shiny.tag.list")

  # Server
  testServer(app = app$server, {
    expect_true(is.character(session$token))
  })
})
