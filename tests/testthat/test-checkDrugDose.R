getInputDb <- function() {

  ingredient_drug_records <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5", "6", "7"),
    person_id = c("1", "2", "3", "4", "5", "6", "7"),
    drug_concept_id = c("40162522", "40162522", "1127078", "1127078", "1127078", "1127433", "1127433"),
    drug = c("x", "xxxx", "xx", "xx", "xxx", "x", "xx"),
    ingredient_concept_id = rep(1125315, 7),
    ingredient = rep("acetaminophen", 7),
    drug_exposure_start_date = as.Date(c("2016-01-01","2017-01-01",NA,"2019-01-01","2020-01-01","2021-01-01","2022-01-01")),
    drug_exposure_end_date = as.Date(c("2016-01-02","2017-01-03",NA,"2019-01-05","2020-01-06","2021-01-07","2022-01-08")),
    days_supply = c(1,3,NA,4,5,6,7),
    quantity = c(10,20,1,NA,2,3,NA)
  )

  mockDrugExposure(ingredient_drug_records = ingredient_drug_records)

}

test_that("checkDrugDose overall", {
  testData <- getInputDb()
  result <- checkDrugDose(testData, "ingredient_drug_records", "drug_strength", byConcept = FALSE,
                          sampleSize = 100)

  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 34)
  expect_equal(colnames(result), c(
    "ingredient_concept_id", "ingredient",
    "n_records","n_sample", "missing_days_supply_or_dates",
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
  expect_true(result$ingredient=="acetaminophen")
  expect_true(result$missing_days_supply_or_dates == 1)
  expect_true(result$proportion_of_records_missing_days_supply_or_dates==
                (result$missing_days_supply_or_dates/result$n_records))
  expect_true(result$missing_or_null_quantity==2)

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})

test_that("checkDrugDose by Concept", {
  testData <- getInputDb()
  result <- checkDrugDose(testData, "ingredient_drug_records","drug_strength" , byConcept = TRUE,
                          sampleSize = 100)

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 36)
  expect_equal(colnames(result), c(
    "drug_concept_id","drug", "ingredient_concept_id", "ingredient",
    "n_records","n_sample", "missing_days_supply_or_dates",
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
  expect_true(result$ingredient[1]=="acetaminophen")
  expect_true(result$missing_days_supply_or_dates[1]== 1)
  expect_true(result$proportion_of_records_missing_days_supply_or_dates[1]==
                (result$missing_days_supply_or_dates[1]/result$n_records[1]))
  expect_true(result$n_records[1]==2)

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})
