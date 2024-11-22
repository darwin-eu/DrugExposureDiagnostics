getTestData <- function() {
  ingredient_drug_records <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5"),
    person_id = c("1", "2", "3", "4", "5"),
    drug_concept_id = c("1", "4", "2", "2", "3"),
    drug = c("x", "iv", "xx", "xx", "xxx"),
    ingredient_concept_id = c("1", "1", "2", "2", "3"),
    ingredient = c("a", "a", "b", "b", "c"),
    drug_exposure_end_date = as.Date(c("2016-01-02", "2017-01-03", "2018-01-04", "2019-01-05", "2020-01-06")),
    verbatim_end_date = as.Date(c(NA, "2017-01-03", "2018-05-04", "2019-01-05", "2020-11-06"))
  )

  mockDrugExposure(ingredient_drug_records = ingredient_drug_records)
}

test_that("checkVerbatimEndDate overall", {
  testData <- getTestData()
  result <- checkVerbatimEndDate(testData, "ingredient_drug_records",
    byConcept = FALSE,
    sampleSize = 100
  ) %>%
    dplyr::collect() %>%
    dplyr::mutate(ingredient_concept_id = as.numeric(.data$ingredient_concept_id)) %>%
    dplyr::arrange(.data$ingredient_concept_id, dplyr::desc(.data$n_records))


  expect_equal(nrow(result), 3)
  expect_equal(result$n_records, c(2, 2, 1))
  expect_equal(result$n_not_missing_verbatim_end_date, c(1, 2, 1))
  expect_equal(result$n_verbatim_end_date_and_drug_exposure_end_date_differ, c(0, 1, 1))
  expect_equal(result$proportion_missing_verbatim_end_date, c(0.2, 0.0, 0.0))
  expect_equal(result$proportion_not_missing_verbatim_end_date, c(0.2, 0.4, 0.2))
  expect_equal(result$proportion_verbatim_end_date_equal_to_drug_exposure_end_date, c(0.2, 0.2, 0.0))
  expect_equal(result$proportion_verbatim_end_date_and_drug_exposure_end_date_differ, c(0.0, 0.2, 0.2))
  expect_equal(result$minimum_verbatim_end_date, as.Date(c("2017-01-03", "2018-05-04", "2020-11-06")))
  expect_equal(result$maximum_verbatim_end_date, as.Date(c("2017-01-03", "2019-01-05", "2020-11-06")))

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})

test_that("checkVerbatimEndDate byConcept", {
  testData <- getTestData()

  result <- checkVerbatimEndDate(testData, "ingredient_drug_records",
    byConcept = TRUE,
    sampleSize = 100
  ) %>%
    dplyr::collect() %>%
    dplyr::mutate(ingredient_concept_id = as.numeric(.data$ingredient_concept_id)) %>%
    dplyr::arrange(.data$ingredient_concept_id, dplyr::desc(.data$n_records))


  expect_equal(nrow(result), 4)
  expect_equal(result$n_records, c(1, 1, 2, 1))
  expect_equal(result$n_verbatim_end_date_and_drug_exposure_end_date_differ, c(0, 0, 1, 1))
  expect_equal(sum(result$n_records), sum(result$n_not_missing_verbatim_end_date) +
    sum(result$n_missing_verbatim_end_date))
  expect_equal(sum(result$n_records), sum(result$n_verbatim_end_date_and_drug_exposure_end_date_differ) +
    sum(result$n_verbatim_end_date_and_drug_exposure_end_date_differ) +
    sum(result$n_missing_verbatim_end_date))


  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})
