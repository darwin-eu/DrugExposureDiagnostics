getInputDb <- function() {
  cdm <- mockDrugExposure()

  cdm[["ingredient_concepts"]] <- ingredientDescendantsInDb(
    cdm,
    ingredient = 1125315,
    drugRecordsTable = "drug_exposure",
    tablePrefix = NULL,
    verbose = FALSE
  )

  cdm[["ingredient_drug_records"]] <- getDrugRecords(
    cdm,
    ingredient = 1125315,
    includedConceptsTable = "ingredient_concepts",
    drugRecordsTable = "drug_exposure",
    tablePrefix = NULL,
    verbose = FALSE
    )

  return(cdm)
}

test_that("checkDrugDose overall", {
  testData <- getInputDb()
  result <- checkDrugDose(testData, "ingredient_drug_records","drug_strength" , byConcept = FALSE)

  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 33)
  expect_equal(colnames(result), c(
    "ingredient_concept_id", "ingredient",
    "n_records", "missing_days_supply_or_dates",
    "proportion_of_records_missing_days_supply_or_dates", "missing_or_null_quantity",
    "proportion_missing_or_null_quantity", "missing_denominator_unit_concept_id",
    "proportion_of_records_missing_denominator_unit_concept_id", "missing_or_null_amount_value",
    "proportion_of_records_missing_or_null_amount_value", "q05_quantity",
    "q10_quantity", "q15_quantity", "q20_quantity", "q25_quantity",
    "median_quantity", "q75_quantity", "q80_quantity", "q85_quantity",
    "q90_quantity", "q95_quantity", "q05_amount_value", "q10_amount_value",
    "q15_amount_value", "q20_amount_value", "q25_amount_value", "median_amount_value",
    "q75_amount_value", "q80_amount_value", "q85_amount_value", "q90_amount_value",
    "q95_amount_value"
  ))
  #check the values
  expect_true(result$ingredient_concept_id==1125315)
  expect_true(result$ingredient=="Acetaminophen")
  expect_true(result$missing_days_supply_or_dates==14)
  expect_true(result$proportion_of_records_missing_days_supply_or_dates==
                (result$missing_days_supply_or_dates/result$n_records))
  expect_true(result$missing_or_null_quantity==result$n_records)
})

test_that("checkDrugDose by Concept", {
  testData <- getInputDb()
  result <- checkDrugDose(testData, "ingredient_drug_records","drug_strength" , byConcept = TRUE)

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 35)
  expect_equal(colnames(result), c(
    "drug_concept_id","drug", "ingredient_concept_id", "ingredient",
    "n_records", "missing_days_supply_or_dates",
    "proportion_of_records_missing_days_supply_or_dates", "missing_or_null_quantity",
    "proportion_missing_or_null_quantity", "missing_denominator_unit_concept_id",
    "proportion_of_records_missing_denominator_unit_concept_id", "missing_or_null_amount_value",
    "proportion_of_records_missing_or_null_amount_value", "q05_quantity",
    "q10_quantity", "q15_quantity", "q20_quantity", "q25_quantity",
    "median_quantity", "q75_quantity", "q80_quantity", "q85_quantity",
    "q90_quantity", "q95_quantity", "q05_amount_value", "q10_amount_value",
    "q15_amount_value", "q20_amount_value", "q25_amount_value", "median_amount_value",
    "q75_amount_value", "q80_amount_value", "q85_amount_value", "q90_amount_value",
    "q95_amount_value"
  ))
  #check the values
  expect_true(result$drug_concept_id[1]==	1127078)
  expect_true(result$ingredient_concept_id[1]==	1125315)
  expect_true(result$ingredient[1]=="Acetaminophen")
  expect_true(result$missing_days_supply_or_dates[1]== 4)
  expect_true(result$proportion_of_records_missing_days_supply_or_dates[1]==
                (result$missing_days_supply_or_dates[1]/result$n_records[1]))
  expect_true(result$n_records[1]==result$missing_or_null_quantity[1])
})
