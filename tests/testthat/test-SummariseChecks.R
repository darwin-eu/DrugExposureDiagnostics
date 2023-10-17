test_that("summariseChecks works", {
  resultList <- list()

  # create input data
  ingredientConceptIds <- c("1125315", "1139042")
  ingredients <- c("acetaminophen", "acetylcysteine")
  resultList$conceptSummary <- data.frame(ingredient = ingredients,
                                          ingredient_concept_id = ingredientConceptIds,
                                          dose_form = c(11, 13),
                                          n_records = c(123, 299),
                                          n_patients = c(123, 200))
  resultList$drugRoutesOverall <-  data.frame(route_type = c("Route1", "Route2", "Route3"),
                                              route_type_id = c(1, 2, 3),
                                              n_records = c(1, 2, 5))
  resultList$drugTypesOverall <- data.frame(drug_type = c("Type1", "Type2", "Type3"),
                                            drug_type_id = c(1, 2, 3),
                                            n_records = c(1, 2, 0))
  resultList$drugExposureDurationOverall <- data.frame(ingredient_concept_id = ingredientConceptIds,
                                                       n_negative_days = c(10, 20),
                                                       proportion_negative_days = c(0.1, 0.2),
                                                       median_drug_exposure_days = c(5, 6),
                                                       q05_drug_exposure_days = c(1, 2),
                                                       q95_drug_exposure_days = c(15, 19))
  resultList$drugDose <- data.frame(ingredient_concept_id = ingredientConceptIds,
                                    missing_days_supply_or_dates = c(12, 1),
                                    proportion_of_records_missing_days_supply_or_dates = c(0.12, 0.01),
                                    missing_denominator_unit_concept_id = c(20, 50),
                                    proportion_of_records_missing_denominator_unit_concept_id = c(0.2, 0.5),
                                    missing_or_null_amount_value = c(19, 11),
                                    proportion_of_records_missing_or_null_amount_value = c(0.19, 0.11),
                                    missing_or_null_quantity = c(8, 4),
                                    proportion_missing_or_null_quantity = c(0.08, 0.04),
                                    median_amount_value = c(5, 8),
                                    q05_amount_value = c(2, 4),
                                    q95_amount_value = c(9, 10))
  resultList$drugQuantity <- data.frame(ingredient_concept_id = ingredientConceptIds,
                                        median_drug_exposure_quantity = c(10, 12),
                                        q05_drug_exposure_quantity = c(1, 2),
                                        q95_drug_exposure_quantity = c(15, 19))

  result <- DrugExposureDiagnostics:::summariseChecks(resultList)

  expect_equal(
    names(result),
    c("ingredient", "ingredient_concept_id", "n_records", "n_patients",
      "proportion_of_records_by_drug_type",
      "proportion_of_records_by_route_type",
      "proportion_of_records_with_dose_form",
      "proportion_of_records_missing_denominator_unit_concept_id",
      "median_amount_value_q05_q95",
      "median_quantity_q05_q95",
      "median_drug_exposure_days_q05_q95",
      "proportion_of_records_with_negative_drug_exposure_days"
    ))

  expect_equal(result$ingredient, ingredients)
  expect_equal(result$ingredient_concept_id, ingredientConceptIds)
  expect_equal(result$n_records, c(123, 299))
  expect_equal(unique(result$proportion_of_records_by_drug_type), c("Type1 (1, 33.3%);Type2 (2, 66.7%);Type3 (0, 0%)"))
  expect_equal(unique(result$proportion_of_records_by_route_type), c("Route1 (1, 12.5%);Route2 (2, 25%);Route3 (5, 62.5%)"))
  expect_equal(result$proportion_of_records_with_dose_form, c("11 (8.9%)", "13 (4.3%)"))
  expect_equal(result$proportion_of_records_missing_denominator_unit_concept_id, c("20 (20%)", "50 (50%)"))
  expect_equal(result$median_amount_value_q05_q95, c("5 (2-9) [19, 19%]", "8 (4-10) [11, 11%]"))
  expect_equal(result$median_quantity_q05_q95, c("10 (1-15) [8, 8%]", "12 (2-19) [4, 4%]"))
  expect_equal(result$median_drug_exposure_days_q05_q95, c("5 (1-15) [12, 12%]", "6 (2-19) [1, 1%]"))
  expect_equal(result$proportion_of_records_with_negative_drug_exposure_days, c("10 (10%)", "20 (20%)"))
})

test_that("summariseChecks partial inputs: summary, quantity and dose", {
  resultList <- list()

  # create input data
  ingredientConceptIds <- c("1125315", "1139042")
  ingredients <- c("acetaminophen", "acetylcysteine")
  resultList$conceptSummary <- data.frame(ingredient = ingredients,
                                          ingredient_concept_id = ingredientConceptIds,
                                          dose_form = c(11, 13),
                                          n_records = c(123, 299),
                                          n_patients = c(123, 200))
  resultList$drugDose <- data.frame(ingredient_concept_id = ingredientConceptIds,
                                    missing_days_supply_or_dates = c(12, 1),
                                    proportion_of_records_missing_days_supply_or_dates = c(0.12, 0.01),
                                    missing_denominator_unit_concept_id = c(20, 50),
                                    proportion_of_records_missing_denominator_unit_concept_id = c(0.2, 0.5),
                                    missing_or_null_amount_value = c(19, 11),
                                    proportion_of_records_missing_or_null_amount_value = c(0.19, 0.11),
                                    missing_or_null_quantity = c(8, 4),
                                    proportion_missing_or_null_quantity = c(0.08, 0.04),
                                    median_amount_value = c(5, 8),
                                    q05_amount_value = c(2, 4),
                                    q95_amount_value = c(9, 10))
  resultList$drugQuantity <- data.frame(ingredient_concept_id = ingredientConceptIds,
                                        median_drug_exposure_quantity = c(10, 12),
                                        q05_drug_exposure_quantity = c(1, 2),
                                        q95_drug_exposure_quantity = c(15, 19))

  result <- DrugExposureDiagnostics:::summariseChecks(resultList)

  expect_equal(
    names(result),
    c("ingredient", "ingredient_concept_id", "n_records", "n_patients",
      "proportion_of_records_with_dose_form",
      "proportion_of_records_missing_denominator_unit_concept_id",
      "median_amount_value_q05_q95",
      "median_quantity_q05_q95"
    ))

  expect_equal(result$ingredient, ingredients)
  expect_equal(result$ingredient_concept_id, ingredientConceptIds)
  expect_equal(result$n_records, c(123, 299))
  expect_equal(result$proportion_of_records_with_dose_form, c("11 (8.9%)", "13 (4.3%)"))
  expect_equal(result$proportion_of_records_missing_denominator_unit_concept_id, c("20 (20%)", "50 (50%)"))
  expect_equal(result$median_amount_value_q05_q95, c("5 (2-9) [19, 19%]", "8 (4-10) [11, 11%]"))
  expect_equal(result$median_quantity_q05_q95, c("10 (1-15) [8, 8%]", "12 (2-19) [4, 4%]"))
})

test_that("summariseChecks partial inputs: summary and quantity", {
  resultList <- list()

  # create input data
  ingredientConceptIds <- c("1125315", "1139042")
  ingredients <- c("acetaminophen", "acetylcysteine")
  resultList$conceptSummary <- data.frame(ingredient = ingredients,
                                          ingredient_concept_id = ingredientConceptIds,
                                          dose_form = c(11, 13),
                                          n_records = c(123, 299),
                                          n_patients = c(123, 200))
  resultList$drugQuantity <- data.frame(ingredient_concept_id = ingredientConceptIds,
                                        median_drug_exposure_quantity = c(10, 12),
                                        q05_drug_exposure_quantity = c(1, 2),
                                        q95_drug_exposure_quantity = c(15, 19))

  result <- DrugExposureDiagnostics:::summariseChecks(resultList)

  expect_equal(
    names(result),
    c("ingredient", "ingredient_concept_id", "n_records", "n_patients",
      "proportion_of_records_with_dose_form"
    ))

  expect_equal(result$ingredient, ingredients)
  expect_equal(result$ingredient_concept_id, ingredientConceptIds)
  expect_equal(result$n_records, c(123, 299))
  expect_equal(result$proportion_of_records_with_dose_form, c("11 (8.9%)", "13 (4.3%)"))
})

test_that("summariseChecks empty/wrong inputs", {
  expect_error(summariseChecks(list()))
  expect_error(summariseChecks(NULL))
})
