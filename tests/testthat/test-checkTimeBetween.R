getInputDb <- function() {
  ingredient_drug_records <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5"),
    drug_concept_id = c("1", "1", "1", "2", "2"),
    drug = c("x", "x", "x", "xx", "xx"),
    ingredient_concept_id = c("1", "1", "1", "1", "1"),
    ingredient = c("a", "a", "a", "a", "a"),
    person_id = c("1", "1", "1", "2", "2"),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"), as.Date("2010-03-01"),
      as.Date("2012-01-01"), as.Date("2012-06-01"),
      as.Date("2013-01-01")
    ),
    drug_exposure_end_date = c(
      as.Date("2013-01-01"), as.Date("2011-02-01"),
      as.Date("2010-01-01"), as.Date("2010-01-01"),
      as.Date("2014-01-01")
    )
  )

  DrugExposureDiagnostics:::mockDrugExposure(ingredient_drug_records = ingredient_drug_records)
}

test_that("check summary calculation overall", {
  testData <- getInputDb()

  result <- summariseTimeBetween(testData, "ingredient_drug_records", byConcept = FALSE, sampleSize = 100)

  expect_identical(as.double(result$minimum_time_between_days), 59)
  expect_identical(as.double(result$maximum_time_between_days), 671)
  expect_identical(round(as.numeric(result$q05_time_between_days), 1), 74.5)

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})

test_that("check summary calculation by concept", {
  testData <- getInputDb()

  result <- summariseTimeBetween(testData, "ingredient_drug_records", byConcept = TRUE, sampleSize = 100) %>%
    dplyr::arrange(drug_concept_id)

  expect_identical(as.double(result$minimum_time_between_days), c(59, 214))
  expect_identical(as.double(result$maximum_time_between_days), c(671, 214))
  expect_identical(round(as.numeric(result$q05_time_between_days), 1), c(89.6, 214))

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})
