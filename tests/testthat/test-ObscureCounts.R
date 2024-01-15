test_that("check for drugRoutes|drugSig|drugVerbatimEndDate|drugQuantity|drugSourceConcepts|drugTypes|drugExposureDuration|missingValues|drugDose|drugDaysSupply", {
  table <- tibble::tibble(
    "drug_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "ingredient_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "drug_route_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "route_type" = c("1", "2", "3", "4", "5", "6"),
    "proportion_of_records_by_drug_type" = c(0.1, 0.1, 0.2, 0.5, 0.6, 0),
    "n_negative_days" = c(10, 12, 100, 5, 10, 10),
    "n_records" = c(10, 10, 100, 5, 6, 0),
    "n_sample" = c(100, 3, 100,100,100000,1000000),
    "n_patients" = c(10, 10, 100, 5, 2, 0)
  )

  types <- c("drugRoutes", "drugSig", "drugVerbatimEndDate", "drugQuantity", "drugSourceConcepts", "drugTypes", "drugExposureDuration",
             "missingValues", "drugDose", "drugDaysSupply")
  lapply(types, FUN = function(type) {
    result <- obscureCounts(table, type, minCellCount = 10, substitute = NA)

    expect_equal(result$result_obscured, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
    expect_equal(result$n_negative_days, c(10, 12, 100, NA, NA, 10))
    expect_equal(result$proportion_of_records_by_drug_type, c(0.1, 0.1, 0.2, NA, NA, 0))
    expect_equal(result$n_records, c(10, 10, 100, NA, NA, 0))
    expect_equal(result$n_sample, c(100, 3, 100,NA, NA,1000000))
    expect_equal(result$n_patients, c(10, 10, 100, NA, NA, 0))
    expect_equal(typeof(result$result_obscured),"logical")

    result <- obscureCounts(table, type, minCellCount = NULL)
    expect_equal(result, table)
  })
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
    expect_equal(typeof(result$result_obscured),"logical")
  })
})


test_that("check for drugIngredientOverview", {
  table <- tibble::tibble(
    "ingredient_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "ingredient" = c("test1", "test2", "test3", "test4", "test5", "test6"),
    "n_records" = c(9, 10, 100, 5, 6, 0),
    "n_people" = c(8, 10, 1, 5, 6, 0)
  )

  # drugIngredientOverview
  result <- obscureCounts(table, "drugIngredientOverview", minCellCount = 5, substitute = NA)
  expect_equal(result$result_obscured, c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(result$ingredient, c("test1", "test2", "test3", "test4", "test5", "test6"))
  expect_equal(result$n_records, c(9, 10, NA, 5, 6, 0))
  expect_equal(result$n_people, c(8, 10, NA, 5, 6, 0))
  expect_equal(typeof(result$result_obscured),"logical")

  result <- obscureCounts(table, "drugIngredientOverview", minCellCount = NULL)
  expect_equal(result, table)

  # drugIngredientPresence
  result <- obscureCounts(table, "drugIngredientPresence", minCellCount = 5, substitute = NA)
  expect_equal(result$result_obscured, c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(result$ingredient, c("test1", "test2", "test3", "test4", "test5", "test6"))
  expect_equal(result$n_records, c(9, 10, NA, 5, 6, 0))
  expect_equal(result$n_people, c(8, 10, NA, 5, 6, 0))
  expect_equal(typeof(result$result_obscured),"logical")

  result <- obscureCounts(table, "drugIngredientPresence", minCellCount = NULL)
  expect_equal(result, table)
})
