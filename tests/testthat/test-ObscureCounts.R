test_that("check for drugRoutes|drugSig|drugVerbatimEndDate|drugQuantity|drugSourceConcepts|drugTypes|drugExposureDuration|missingValues|drugDaysSupply", {
  table <- tibble::tibble(
    "drug_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "ingredient_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "drug_route_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "route_type" = c("1", "2", "3", "4", "5", "6"),
    "proportion_of_records_by_drug_type" = c(0.1, 0.1, 0.2, 0.5, 0.6, 0),
    "n_negative_days" = c(10, 12, 100, 5, 10, 10),
    "n_records" = c(10, 10, 100, 11, 16, 0),
    "n_sample" = c(100, 100, 100, 100, 100, 100),
    "n_person" = c(10, 10, 8, 5, 2, 0)
  )

  types <- c(
    "drugRoutes", "drugSig", "drugVerbatimEndDate", "drugQuantity", "drugSourceConcepts", "drugTypes", "drugExposureDuration",
    "drugDaysSupply"
  )
  lapply(types, FUN = function(type) {
    result <- obscureCounts(table, type, minCellCount = 10, substitute = NA)

    expect_equal(result$result_obscured, c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
    expect_equal(result$n_negative_days, c(10, 12, NA, NA, NA, 10))
    expect_equal(result$proportion_of_records_by_drug_type, c(0.1, 0.1, NA, NA, NA, 0))
    expect_equal(result$n_records, c(10, 10, NA, NA, NA, 0))
    expect_equal(result$n_sample, c(100, 100, NA, NA, NA, 100))
    expect_equal(result$n_person, c(10, 10, NA, NA, NA, 0))
    expect_equal(typeof(result$result_obscured), "logical")

    result <- obscureCounts(table, type, minCellCount = 0)
    expect_equal(result, table)
  })
})

test_that("check for missingValues", {
  table <- tibble::tibble(
    "drug_concept_id" = c("1", "2", "3", "4", "5", "6", "7", "8"),
    "ingredient_concept_id" = c("1", "2", "3", "4", "5", "6", "7", "8"),
    "drug_route_concept_id" = c("1", "2", "3", "4", "5", "6", "7", "8"),
    "n_records_not_missing_value" = c(10, 12, 100, 15, 10, 10, 9, 10),
    "n_records_missing_value" = c(10, 12, 100, 15, 10, 10, 10, 8),
    "n_records" = c(10, 10, 100, 11, 16, 0, 0, 0),
    "n_sample" = c(100, 100, 100, 100, 100, 100, 100, 100),
    "n_person" = c(10, 10, 8, 5, 2, 0, 0, 0)
  )

  result <- obscureCounts(table, "missingValues", minCellCount = 10, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(result$n_records_not_missing_value, c(10, 12, NA, NA, NA, 10, NA, NA))
  expect_equal(result$n_records_missing_value, c(10, 12, NA, NA, NA, 10, NA, NA))
  expect_equal(result$n_records, c(10, 10, NA, NA, NA, 0, NA, NA))
  expect_equal(result$n_sample, c(100, 100, NA, NA, NA, 100, NA, NA))
  expect_equal(result$n_person, c(10, 10, NA, NA, NA, 0, NA, NA))
  expect_equal(typeof(result$result_obscured), "logical")

  result <- obscureCounts(table, type, minCellCount = 0)
  expect_equal(result, table)
})

test_that("check for conceptSummary|diagnosticsSummary", {
  table <- tibble::tibble(
    "drug_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "ingredient_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "concept_id" = c("1", "2", "3", "4", "5", "6"),
    "concept_name" = c("a", "b", "c", "d", "e", "f"),
    "n_records" = c(10, 10, 100, 5, 6, 0),
    "n_patients" = c(10, 10, 100, 5, 2, 0)
  )

  types <- c("conceptSummary", "diagnosticsSummary")
  lapply(types, FUN = function(type) {
    result <- obscureCounts(table, type, minCellCount = 10, substitute = NA)

    expect_equal(result$result_obscured, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
    # only n should be obscured
    expect_equal(result$drug_concept_id, table$drug_concept_id)
    expect_equal(result$ingredient_concept_id, table$ingredient_concept_id)
    expect_equal(result$concept_id, table$concept_id)
    expect_equal(result$concept_name, table$concept_name)
    expect_equal(result$n_records, c(10, 10, 100, NA, NA, 0))
    expect_equal(result$n_patients, c(10, 10, 100, NA, NA, 0))
    expect_equal(typeof(result$result_obscured), "logical")
  })
})
