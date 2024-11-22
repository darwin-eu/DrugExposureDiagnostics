getTestData <- function() {
  ingredient_drug_records <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5", "6"),
    person_id = c("1", "2", "3", "4", "5", "6"),
    drug_concept_id = c("1", "4", "2", "2", "3", "5"),
    drug = c("x", "iv", "xx", "xx", "xxx", "v"),
    ingredient_concept_id = c("1", "1", "2", "2", "3", "4"),
    ingredient = c("a", "a", "b", "b", "c", "d"),
    sig = c("sig1", "sig1", "sig1", NA, NA, "sig2")
  )

  mockDrugExposure(ingredient_drug_records = ingredient_drug_records)
}

test_that("checkDrugSig overall", {
  testData <- getTestData()
  result <- checkDrugSig(testData, "ingredient_drug_records", byConcept = FALSE, sampleSize = 100) %>%
    dplyr::collect() %>%
    dplyr::mutate(ingredient_concept_id = as.numeric(.data$ingredient_concept_id)) %>%
    dplyr::arrange(.data$ingredient_concept_id, dplyr::desc(.data$n_records), .data$sig)

  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 7)
  expect_equal(
    colnames(result),
    c(
      "ingredient_concept_id", "ingredient", "sig", "n_records", "n_sample", "n_person",
      "proportion_records"
    )
  )
  expect_equal(result$ingredient_concept_id, c(1, 2, 2, 3, 4))
  expect_equal(result$sig, c("sig1", "sig1", NA, NA, "sig2"))
  expect_equal(result$n_records, c(2, 1, 1, 1, 1))
  expect_equal(result$proportion_records, c(1 / 3, 1 / 6, 1 / 6, 1 / 6, 1 / 6))
  expect_equal(unique(result$n_sample), 100)

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})

test_that("checkDrugSig byConcept", {
  testData <- getTestData()
  result <- checkDrugSig(testData, "ingredient_drug_records", byConcept = TRUE, sampleSize = 100) %>%
    dplyr::collect() %>%
    dplyr::mutate(ingredient_concept_id = as.numeric(.data$ingredient_concept_id)) %>%
    dplyr::arrange(.data$ingredient_concept_id, dplyr::desc(.data$n_records), .data$sig)

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 9)
  expect_equal(colnames(result), c(
    "drug_concept_id", "drug", "ingredient_concept_id",
    "ingredient", "sig", "n_records", "n_sample", "n_person",
    "proportion_records"
  ))
  expect_equal(result$ingredient_concept_id, c(1, 1, 2, 2, 3, 4))
  expect_equal(result$sig, c("sig1", "sig1", "sig1", NA, NA, "sig2"))
  expect_equal(result$n_records, c(1, 1, 1, 1, 1, 1))
  expect_equal(result$proportion_records, c(1 / 6, 1 / 6, 1 / 6, 1 / 6, 1 / 6, 1 / 6))
  expect_equal(unique(result$n_sample), 100)

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})
