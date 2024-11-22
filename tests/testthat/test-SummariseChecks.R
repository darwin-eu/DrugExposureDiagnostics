test_that("summariseChecks works", {
  resultList <- list()

  # create input data
  ingredientConceptIds <- c("1125315", "1139042")
  ingredients <- c("acetaminophen", "acetylcysteine")
  resultList$conceptSummary <- data.frame(
    ingredient = ingredients,
    ingredient_concept_id = ingredientConceptIds,
    dose_form = c("injection", NA),
    n_records = c(123, 299),
    n_patients = c(123, 200)
  )
  resultList$drugRoutesOverall <- data.frame(
    route_type = c("Route1", "Route2", "Route3"),
    route_type_id = c(1, 2, 3),
    n_records = c(1, 2, 5)
  )
  resultList$drugTypesOverall <- data.frame(
    drug_type = c("Type1", "Type2", "Type3"),
    drug_type_id = c(1, 2, 3),
    n_records = c(1, 2, 0)
  )
  resultList$drugExposureDurationOverall <- data.frame(
    ingredient_concept_id = ingredientConceptIds,
    n_negative_days = c(10, 20),
    proportion_negative_days = c(0.1, 0.2),
    median_drug_exposure_days = c(5, 6),
    q05_drug_exposure_days = c(1, 2),
    q95_drug_exposure_days = c(15, 19)
  )
  resultList$drugDose <- data.frame(
    ingredient_concept_id = c(rep("1125315", 12), rep("1139042", 12)),
    ingredient = c(rep("Acetaminophen", 12), rep("Acetylcysteine", 12)),
    group_name = c(rep("ingredient_name", 24)),
    strata_name = c(rep(c(rep("overall", 6), rep("unit", 6)), 2)),
    strata_level = c(rep(c(rep("NA", 6), rep("milligram", 6)), 2)),
    variable_name = c(rep(c("number_records", rep("daily_dose", 5)), 4)),
    estimate_name = c(rep(c(
      "count", "count_missing", "percentage_missing",
      "q05", "median", "q95"
    ), 4)),
    estimate_type = c(rep(c(
      "integer", "integer", "percentage",
      "numeric", "numeric", "numeric"
    ), 4)),
    estimate_value = c(
      10, 5, 50, 2, 6, 10, 8, 2, 0.25, 1, 2, 4,
      10, 1, 0.1, 2, 4, 8, 5, 1, 0.2, 1, 5, 10
    )
  )
  resultList$drugQuantity <- data.frame(
    ingredient_concept_id = ingredientConceptIds,
    median_drug_exposure_quantity = c(10, 12),
    q05_drug_exposure_quantity = c(1, 2),
    q95_drug_exposure_quantity = c(15, 19)
  )

  resultList$missingValuesOverall <- data.frame(
    ingredient_concept_id = c(rep(ingredientConceptIds[1], 4), rep(ingredientConceptIds[2], 4)),
    ingredient = c(rep(ingredients[1], 4), rep(ingredients[2], 4)),
    variable = rep(c("n_missing_quantity", "n_missing_drug_exposure_start_date",
                 "n_missing_drug_exposure_end_date", "n_missing_days_supply"), 2),
    n_records_missing_value = c(10, 0, NA, 2, NA, 0, 0, 0),
    proportion_records_missing_value = c(4, 0, 0, 1, 0, 0, 0, 0)
  )

  result <- DrugExposureDiagnostics:::summariseChecks(resultList)

  expect_equal(
    names(result),
    c(
      "ingredient", "ingredient_concept_id", "n_records", "n_patients",
      "proportion_of_records_by_drug_type",
      "proportion_of_records_by_route_type",
      "proportion_of_records_with_dose_form",
      "missing_quantity_exp_start_end_days_supply",
      "n_dose_and_missingness",
      "median_daily_dose_q05_q95",
      "median_quantity_q05_q95",
      "median_drug_exposure_days_q05_q95",
      "proportion_of_records_with_negative_drug_exposure_days"
    )
  )

  expect_equal(result$ingredient, ingredients)
  expect_equal(result$ingredient_concept_id, ingredientConceptIds)
  expect_equal(result$n_records, c(123, 299))
  expect_equal(unique(result$proportion_of_records_by_drug_type), c("Type1 (1, 33.3%);Type2 (2, 66.7%);Type3 (0, 0%)"))
  expect_equal(unique(result$proportion_of_records_by_route_type), c("Route1 (1, 12.5%);Route2 (2, 25%);Route3 (5, 62.5%)"))
  expect_equal(result$proportion_of_records_with_dose_form, c("123 (100%)", "0 (0%)"))
  expect_equal(result$missing_quantity_exp_start_end_days_supply, c("10 (4%), 0 (0%), 0 (0%), 2 (1%)", "0 (0%), 0 (0%), 0 (0%), 0 (0%)"))
  expect_equal(result$median_daily_dose_q05_q95, c("6 (2-10) [milligram]", "4 (2-8) [milligram]"))
  expect_equal(result$median_quantity_q05_q95, c("10 (1-15)", "12 (2-19)"))
  expect_equal(result$median_drug_exposure_days_q05_q95, c("5 (1-15)", "6 (2-19)"))
  expect_equal(result$proportion_of_records_with_negative_drug_exposure_days, c("10 (10%)", "20 (20%)"))
})

test_that("summariseChecks partial inputs: summary, quantity and dose", {
  resultList <- list()

  # create input data
  ingredientConceptIds <- c("1125315", "1139042")
  ingredients <- c("acetaminophen", "acetylcysteine")
  resultList$conceptSummary <- data.frame(
    ingredient = ingredients,
    ingredient_concept_id = ingredientConceptIds,
    dose_form = c("injection", NA),
    n_records = c(123, 299),
    n_patients = c(123, 200)
  )
  resultList$drugDose <- data.frame(
    ingredient_concept_id = c(rep("1125315", 12), rep("1139042", 12)),
    ingredient = c(rep("Acetaminophen", 12), rep("Acetylcysteine", 12)),
    group_name = c(rep("ingredient_name", 24)),
    strata_name = c(rep(c(rep("overall", 6), rep("unit", 6)), 2)),
    strata_level = c(rep(c(rep("NA", 6), rep("milligram", 6)), 2)),
    variable_name = c(rep(c("number_records", rep("daily_dose", 5)), 4)),
    estimate_name = c(rep(c(
      "count", "count_missing", "percentage_missing",
      "q05", "median", "q95"
    ), 4)),
    estimate_type = c(rep(c(
      "integer", "integer", "percentage",
      "numeric", "numeric", "numeric"
    ), 4)),
    estimate_value = c(
      10, 5, 50, 2, 6, 10, 8, 2, 0.25, 1, 2, 4,
      10, 1, 0.1, 2, 4, 8, 5, 1, 0.2, 1, 5, 10
    )
  )
  resultList$drugQuantity <- data.frame(
    ingredient_concept_id = ingredientConceptIds,
    median_drug_exposure_quantity = c(10, 12),
    q05_drug_exposure_quantity = c(1, 2),
    q95_drug_exposure_quantity = c(15, 19)
  )

  result <- DrugExposureDiagnostics:::summariseChecks(resultList)

  expect_equal(
    names(result),
    c(
      "ingredient", "ingredient_concept_id", "n_records", "n_patients",
      "proportion_of_records_with_dose_form",
      "n_dose_and_missingness",
      "median_daily_dose_q05_q95",
      "median_quantity_q05_q95"
    )
  )

  expect_equal(result$ingredient, ingredients)
  expect_equal(result$ingredient_concept_id, ingredientConceptIds)
  expect_equal(result$n_records, c(123, 299))
  expect_equal(result$proportion_of_records_with_dose_form, c("123 (100%)", "0 (0%)"))
  expect_equal(result$n_dose_and_missingness, c("10 (5, 50%)", "10 (1, 0.1%)"))
  expect_equal(result$median_daily_dose_q05_q95, c("6 (2-10) [milligram]", "4 (2-8) [milligram]"))
  expect_equal(result$median_quantity_q05_q95, c("10 (1-15)", "12 (2-19)"))
})

test_that("summariseChecks partial inputs: summary and quantity", {
  resultList <- list()

  # create input data
  ingredientConceptIds <- c("1125315", "1139042")
  ingredients <- c("acetaminophen", "acetylcysteine")
  resultList$conceptSummary <- data.frame(
    ingredient = ingredients,
    ingredient_concept_id = ingredientConceptIds,
    dose_form = c("injection", NA),
    n_records = c(123, 299),
    n_patients = c(123, 200)
  )
  resultList$drugQuantity <- data.frame(
    ingredient_concept_id = ingredientConceptIds,
    median_drug_exposure_quantity = c(10, 12),
    q05_drug_exposure_quantity = c(1, 2),
    q95_drug_exposure_quantity = c(15, 19)
  )

  result <- DrugExposureDiagnostics:::summariseChecks(resultList)

  expect_equal(
    names(result),
    c(
      "ingredient", "ingredient_concept_id", "n_records", "n_patients",
      "proportion_of_records_with_dose_form", "median_quantity_q05_q95"
    )
  )

  expect_equal(result$ingredient, ingredients)
  expect_equal(result$ingredient_concept_id, ingredientConceptIds)
  expect_equal(result$n_records, c(123, 299))
  expect_equal(result$proportion_of_records_with_dose_form, c("123 (100%)", "0 (0%)"))
})

test_that("summariseChecks partial inputs with only empty ingredient: summary of missing and quantity and dose", {
  cdm <- mockDrugExposure()

  empty_ing <- executeChecks(cdm = cdm, ingredients = c(36854851), checks = c("missing", "quantity", "dose", "diagnosticsSummary"))

  expect_equal(nrow(empty_ing$diagnosticsSummary), 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("summariseChecks partial inputs with combination of not empty and empty ingredient: summary of missing and quantity and dose", {
  cdm <- mockDrugExposure()

  empty_ing <- executeChecks(cdm = cdm, ingredients = c(1125315, 36854851), checks = c("missing", "quantity", "dose", "diagnosticsSummary"))

  expect_equal(empty_ing$diagnosticsSummary$ingredient, "acetaminophen")
  expect_equal(nrow(empty_ing$diagnosticsSummary), 1)
  expect_equal(empty_ing$diagnosticsSummary$missing_quantity_exp_start_end_days_supply, "0 (0%), 0 (0%), 0 (0%), 0 (0%)")

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("summariseChecks empty/wrong inputs", {
  expect_error(summariseChecks(list()))
  expect_error(summariseChecks(NULL))
})
