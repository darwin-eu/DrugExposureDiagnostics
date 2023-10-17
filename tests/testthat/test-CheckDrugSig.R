getTestData <- function() {
  drug_exposure <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5", "6", "7"),
    drug_concept_id = c("1", "1", "2", "2", "3", "1", "2"),
    drug = c("x", "x", "xx", "xx", "xxx", "x", "xx"),
    ingredient_concept_id = c("1", "1", "2", "2", "3", "2", "1"),
    ingredient = c("a", "a", "b", "b", "c", "b", "a"),
    person_id = c("1", "2", "3", "4", "5", "6", "7"),
    sig = c("sig1", "sig2", "sig3", "sig1", NA, "sig1", "sig2"))

  mockDrugExposure(drug_exposure = drug_exposure)
}

test_that("checkDrugSig overall", {
  testData <- getTestData()
  result <- checkDrugSig(testData, byConcept = FALSE) %>%
    dplyr::collect() %>%
    dplyr::mutate(ingredient_concept_id = as.numeric(.data$ingredient_concept_id)) %>%
    dplyr::arrange(.data$ingredient_concept_id, dplyr::desc(.data$n_records))

  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 4)
  expect_equal(result$ingredient_concept_id, c(1, 1, 2, 2, 3))
  expect_equal(result$sig, c("sig2", "sig1", "sig1", "sig3", NA))
  expect_equal(result$n_records, c(2, 1, 2, 1, 1))
})

test_that("checkDrugSig byConcept", {
  testData <- getTestData()
  result <- checkDrugSig(testData, byConcept = TRUE) %>%
    dplyr::collect() %>%
    dplyr::mutate(ingredient_concept_id = as.numeric(.data$ingredient_concept_id)) %>%
    dplyr::arrange(.data$ingredient_concept_id, dplyr::desc(.data$n_records))

  expect_equal(nrow(result), 7)
  expect_equal(ncol(result), 6)
  expect_equal(result$ingredient_concept_id, c(1, 1, 1, 2, 2, 2, 3))
  expect_equal(setdiff(result$sig, c("sig1", "sig2", "sig2", "sig1", "sig1", "sig3", NA)), character(0))
  expect_equal(result$n_records, c(1, 1, 1, 1, 1, 1, 1))
})
