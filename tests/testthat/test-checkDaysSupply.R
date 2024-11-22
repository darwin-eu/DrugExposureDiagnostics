getInputDb <- function() {
  ingredient_drug_records <- tibble::tibble(
    drug_exposure_id = as.integer(c("1", "2", "3", "4", "5", "6", "7")),
    person_id = as.integer(c("1", "2", "3", "4", "5", "6", "7")),
    drug_concept_id = as.integer(c("1", "4", "2", "2", "3", "1", "2")),
    drug = c("x", "iv", "xx", "xx", "xxx", "iv", "v"),
    ingredient_concept_id = c("1", "1", "2", "2", "3", "4", "4"),
    ingredient = c("a", "a", "b", "b", "c", "d", "d"),
    drug_exposure_start_date = as.Date(c("2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01")),
    drug_exposure_end_date = as.Date(c("2016-01-02", "2017-01-03", "2018-01-04", "2019-01-05", "2020-01-06", "2021-01-07", "2022-01-08")),
    days_supply = c(1, 3, NA, 4, 5, 6, 7)
  )

  mockDrugExposure(ingredient_drug_records = ingredient_drug_records)
}

test_that("check days supply overall", {
  testData <- getInputDb()

  result <- checkDaysSupply(testData, "ingredient_drug_records", byConcept = FALSE, sampleSize = 100)

  expect_equal(result$n_different_days_supply_and_drug_dates, c(1, 0, 0, 0))

  expect_equal(result$n_days_supply_match_drug_dates, c(1, 1, 1, 2))

  expect_equal(result$n_missing_days_supply, c(0, 1, 0, 0))

  totalLength <- dim(testData$ingredient_drug_records %>% dplyr::collect())[1]

  expect_true(sum(
    result$n_different_days_supply_and_drug_dates, result$n_days_supply_match_drug_dates,
    result$n_missing_days_supply
  ) == totalLength)

  expect_equal(result$minimum_drug_exposure_days_supply, c(1, 4, 5, 6))
  expect_equal(result$n_records, c(2, 2, 1, 2))
  expect_equal(ncol(result), 20)
  expect_equal(result$n_person, c(2, 2, 1, 2))

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})


test_that("check days supply by Concept", {
  testData <- getInputDb()

  result <- checkDaysSupply(testData, "ingredient_drug_records", byConcept = TRUE, sampleSize = 100)

  expect_equal(result$n_different_days_supply_and_drug_dates, c(0, 0, 0, 0, 0, 1))

  expect_equal(result$n_days_supply_match_drug_dates, c(1, 1, 1, 1, 1, 0))

  expect_equal(result$n_missing_days_supply, c(0, 0, 0, 1, 0, 0))

  totalLength <- dim(testData$ingredient_drug_records %>% dplyr::collect())[1]

  expect_true(sum(
    result$n_different_days_supply_and_drug_dates, result$n_days_supply_match_drug_dates,
    result$n_missing_days_supply
  ) == totalLength)

  expect_equal(result$minimum_drug_exposure_days_supply, c(6, 1, 7, 4, 5, 3))
  expect_equal(result$n_records, c(1, 1, 1, 2, 1, 1))
  expect_equal(ncol(result), 22)
  expect_equal(result$n_person, c(1, 1, 1, 2, 1, 1))

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})
