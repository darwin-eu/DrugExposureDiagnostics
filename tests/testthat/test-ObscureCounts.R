test_that("check for drugDailyDose|drugQuantity", {
  table <- tibble::tibble(
    "drug_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "ingredient_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "n_negative_days" = c(10, 2, 100, 5, 0, 0),
    "n_zero_days" = c(10, 2, 100, 5, 4, 0),
    "prop_negative_days" = c(0.1, 0.2, 0.1, 0.5, 0.4, 0),
    "prop_zero_days" = c(10, 2, 100, 5, 4, 0),
    "min_drug_daily_dose" = c(100, 10, 11, 12, 4, 0)
  )

  result <- obscureCounts(table, "drugDailyDose", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(result$n_negative_days, c(10, NA, 100, 5, NA, 0))
  expect_equal(result$n_zero_days, c(10, NA, 100, 5, NA, 0))
  expect_equal(result$prop_negative_days, c(0.1, NA, 0.1, 0.5, NA, 0))
  expect_equal(result$prop_zero_days, c(10, NA, 100, 5, NA, 0))
  expect_equal(result$min_drug_daily_dose, c(100, NA, 11, 12, NA, 0))

  result <- obscureCounts(table, "drugDailyDose", minCellCount = NULL)
  expect_equal(result, table)

  result <- obscureCounts(table, "drugQuantity", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(result$n_negative_days, c(10, NA, 100, 5, NA, 0))
  expect_equal(result$n_zero_days, c(10, NA, 100, 5, NA, 0))
  expect_equal(result$prop_negative_days, c(0.1, NA, 0.1, 0.5, NA, 0))
  expect_equal(result$prop_zero_days, c(10, NA, 100, 5, NA, 0))
  expect_equal(result$min_drug_daily_dose, c(100, NA, 11, 12, NA, 0))

  result <- obscureCounts(table, "drugQuantity", minCellCount = NULL)
  expect_equal(result, table)
})

test_that("check for drugExposureDuration", {
  table <- tibble::tibble(
    "drug_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "ingredient_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "n_negative_days" = c(10, 2, 100, 5, 0, 0)
  )

  result <- obscureCounts(table, "drugExposureDuration", minCellCount = NULL)
  expect_equal(result, table)
  result <- obscureCounts(table, "drugExposureDuration", minCellCount = 5, substitute = NA)
  expect_equal(result[,1:3], table)
})

test_that("check for drugDaysSupply", {
  table <- tibble::tibble(
    "drug_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "ingredient_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "different_days_supply" = c(10, 5, 100, 5, 5, 0),
    "match_days_supply" = c(10, 5, 100, 5, 6, 0),
    "missing_days_supply_or_dates" = c(1, 99, 100, 5, 5, 0),
    "prop_different_days_supply" = c(0.1, 0.2, 0.1, 0.5, 0.4, 0)
  )

  result <- obscureCounts(table, "drugDaysSupply", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(result$different_days_supply, c(NA, 5, 100, 5, 5, 0))
  expect_equal(result$match_days_supply, c(NA, 5, 100, 5, 6, 0))
  expect_equal(result$missing_days_supply_or_dates, c(NA, 99, 100, 5, 5, 0))
  expect_equal(result$prop_different_days_supply, c(NA, 0.2, 0.1, 0.5, 0.4, 0))

  result <- obscureCounts(table, "drugDaysSupply", minCellCount = NULL)
  expect_equal(result, table)
})

test_that("check for drugRoutes|drugSig|drugSourceConcepts|drugTypes|ingredientConcepts", {
  table <- tibble::tibble(
    "drug_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "ingredient_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "drug_route_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "route_type" = c("1", "2", "3", "4", "5", "6"),
    "n_records" = c(10, 10, 100, 5, 6, 0)
  )

  result <- obscureCounts(table, "drugRoutes", minCellCount = 10, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(result$n_records, c(10, 10, 100, NA, NA, 0))

  result <- obscureCounts(table, "drugRoutes", minCellCount = NULL)
  expect_equal(result, table)

  result <- obscureCounts(table, "ingredientConcepts", minCellCount = 10, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(result$n_records, c(10, 10, 100, NA, NA, 0))
})

test_that("check for ingredientConcepts", {
  table <- tibble::tibble(
    "drug_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "ingredient_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "concept_id" = c("1", "2", "3", "4", "5", "6"),
    "concept_name" = c("a", "b", "c", "d", "e", "f"),
    "n_records" = c(10, 10, 100, 5, 6, 0)
  )

  result <- obscureCounts(table, "ingredientConcepts", minCellCount = 10, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
  # only n should be obscured
  expect_equal(result$drug_concept_id, table$drug_concept_id)
  expect_equal(result$ingredient_concept_id, table$ingredient_concept_id)
  expect_equal(result$concept_id, table$concept_id)
  expect_equal(result$concept_name, table$concept_name)
  expect_equal(result$n_records, c(10, 10, 100, NA, NA, 0))

  result <- obscureCounts(table, "drugRoutes", minCellCount = NULL)
  expect_equal(result, table)
})

test_that("check for drugsMissing", {
  table <- tibble::tibble(
    "drug_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "variable" = c("1", "2", "3", "4", "5", "6"),
    "count_missing" = c(1, 10, 100, 5, 6, 0),
    "proportion_missing" = c(0.1, 0.1, 0.1, 0.5, 0.6, 0)
  )

  result <- obscureCounts(table, "drugsMissing", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(result$count_missing, c(NA, 10, 100, 5, 6, 0))
  expect_equal(result$proportion_missing, c(NA, 0.1, 0.1, 0.5, 0.6, 0))

  result <- obscureCounts(table, "drugsMissing", minCellCount = NULL)
  expect_equal(result, table)
})

test_that("check for drugVerbatimEndDate", {
  table <- tibble::tibble(
    "drug_concept_id" = c("1", "2", "3", "4", "5", "6"),
    "count" = c(6, 10, 100, 5, 6, 0),
    "count_not_null" = c(9, 10, 100, 5, 6, 0),
    "count_not_equal" = c(12, 10, 1, 5, 6, 0)
  )

  result <- obscureCounts(table, "drugVerbatimEndDate", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(result$count, c(6, 10, NA, 5, 6, 0))
  expect_equal(result$count_not_null, c(9, 10, NA, 5, 6, 0))
  expect_equal(result$count_not_equal, c(12, 10, NA, 5, 6, 0))

  result <- obscureCounts(table, "drugVerbatimEndDate", minCellCount = NULL)
  expect_equal(result, table)
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

  result <- obscureCounts(table, "drugIngredientOverview", minCellCount = NULL)
  expect_equal(result, table)

  # drugIngredientPresence
  result <- obscureCounts(table, "drugIngredientPresence", minCellCount = 5, substitute = NA)
  expect_equal(result$result_obscured, c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(result$ingredient, c("test1", "test2", "test3", "test4", "test5", "test6"))
  expect_equal(result$n_records, c(9, 10, NA, 5, 6, 0))
  expect_equal(result$n_people, c(8, 10, NA, 5, 6, 0))

  result <- obscureCounts(table, "drugIngredientPresence", minCellCount = NULL)
  expect_equal(result, table)
})
