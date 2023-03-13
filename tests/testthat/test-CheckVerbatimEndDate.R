getTestData <- function() {
  drug_exposure <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5"),
    drug_concept_id = c("1", "1", "2", "2", "3"),
    drug = c("a", "a", "b", "b", "c"),
    ingredient_concept_id = c("1", "1", "1", "1", "1"),
    ingredient = c("a", "a", "a", "a", "a"),
    person_id = c("1", "2", "3", "4", "5"),
    drug_exposure_end_date = c(
      as.Date("2011-01-01"), NA,
      as.Date("2013-01-01"), as.Date("2010-01-01"),
      as.Date("2014-01-01")),
    verbatim_end_date = c(
      as.Date("2011-01-01"), as.Date("2012-01-01"),
      as.Date("2019-12-31"), as.Date("2010-01-01"),
      NA))

  mockDrugExposure(drug_exposure = drug_exposure)
}

test_that("checkVerbatimEndDate overall", {
  testData <- getTestData()
  result <- checkVerbatimEndDate(testData, byConcept = FALSE) %>% dplyr::collect()

  expect_equal(nrow(result), 1)
  expect_equal(result$n_records, 5)
  expect_equal(result$n_not_missing_verbatim_end_date, 4)
  expect_equal(result$n_verbatim_end_date_and_drug_exposure_end_date_differ, 1)
  expect_equal(result$minimum_verbatim_end_date, as.Date("2010-01-01"))
  expect_equal(result$maximum_verbatim_end_date, as.Date("2019-12-31"))
})

test_that("checkVerbatimEndDate byConcept", {
  testData <- getTestData()

  result <- checkVerbatimEndDate(testData, byConcept = TRUE) %>% dplyr::collect()

  expect_equal(nrow(result), 3)
  expect_equal(result$n_records, c(2, 2, 1))
  expect_equal(result$n_not_missing_verbatim_end_date, c(2, 2, 0))
  expect_equal(result$n_verbatim_end_date_and_drug_exposure_end_date_differ, c(0, 1, 0))
  expect_equal(result$minimum_verbatim_end_date, c(as.Date("2011-01-01"), as.Date("2010-01-01"), NA))
  expect_equal(result$maximum_verbatim_end_date, c(as.Date("2012-01-01"), as.Date("2019-12-31"), NA))
})
