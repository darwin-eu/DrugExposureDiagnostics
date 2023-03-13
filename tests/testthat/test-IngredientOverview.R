getInputDb <- function() {
  drugExposure <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
    drug_concept_id = c("1", "2", "3", "4", "1", "2", "3", "4", "1", "2", "3", "2"),
    ingredient_concept_id = c("1", "1", "1", "4", "4", "2", "1", "2", "3", "4", "1", "2"),
    ingredient = c("acetaminophen", "acetaminophen", "acetaminophen", "hydrocodone",
                   "hydrocodone", "codeine", "acetaminophen", "codeine", "acetaminophenPlus",
                   "hydrocodone", "acetaminophen", "codeine"),
    person_id = rep(c("1", "1", "3", "4", "5", "6"), 2),
    drug_exposure_start_date = rep(as.Date("2010-01-01"), 12),
    drug_exposure_end_date = c(as.Date("2010-01-11"), as.Date("2010-01-11"), as.Date("2010-01-11"),
                               as.Date("2011-01-11"), as.Date("2010-01-12"), as.Date("2010-01-11"),
                               as.Date("2010-04-10"), as.Date("2010-01-06"), as.Date("2010-01-11"),
                               as.Date("2010-01-11"), as.Date("2010-01-01"), as.Date("2010-01-11")),
    days_supply = c(11, 11, 11, 376, 12, 11, 100, 6, 11, 11, 0, 11),
    sig = c(rep("verbatim instruction", 10), "", "verbatim instruction"),
    quantity = c(10, 10, 10, 50, 0, 10, 100, 20, 10, 50, 0, 10))

  drugStrength <- tibble::tibble(
    drug_concept_id = c("1", "2", "3", "4"),
    amount_unit = c("mg", "mg", "mg", "mg"),
    amount_value = c(10, 10, 10, 10),
    denominator_unit = c("mL", "mL", "mL", ""),
    denominator_value = c(1, 2, 3, 1),
    numerator_value = c(1, 1, 1, 1),
    numerator_unit = c("mg", "mg", "mg", "mg"))

  cdm <- mockDrugExposure(drug_exposure = drugExposure,
                          drug_strength = drugStrength)
  return(cdm)
}

test_that("getIngredientOverview", {
  testDb <- getInputDb()
  result <- getIngredientOverview(testDb,
                                  drugRecordsTable = "drug_exposure",
                                  drugStrengthTable = "drug_strength") %>% dplyr::collect()

  expect_equal(nrow(result), 9)
  expect_equal(ncol(result), 9)
  expect_equal(colnames(result), c("ingredient_concept_id", "ingredient",
                                   "drug_exposure_days", "days_supply", "quantity",
                                   "sig", "strength", "n_records",
                                   "n_people"))
  expect_equal(result$strength, c("1.0 mg/mL", "10.0 mg", "1.0 mg/mL",
                                  "1.0 mg/mL", "1.0 mg/mL", "10.0 mg",
                                  "1.0 mg/mL", "1.0 mg/mL", "1.0 mg/mL"))
  expect_equal(result$n_records, c(3, 1, 1, 2, 1, 1, 1, 1, 1))
  expect_equal(result$n_people, c(2, 1, 1, 1, 1, 1, 1, 1, 1))
})

test_that("getIngredientOverview error cases", {
  testDb <- getInputDb()

  expect_error(getIngredientPresence(cdm = null))
  expect_error(getIngredientPresence(cdm = testDb, drugRecordsTable = "test"))
  expect_error(getIngredientPresence(cdm = testDb,
                                     drugRecordsTable = "drug_exposure",
                                     drugStrengthTable = "test"))
})

test_that("getIngredientsPresence", {
  testDb <- getInputDb()
  result <- getIngredientPresence(testDb,
                                  drugRecordsTable = "drug_exposure",
                                  drugStrengthTable = "drug_strength") %>% dplyr::collect()

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 10)
  expect_equal(colnames(result), c("ingredient_concept_id", "ingredient", "days_supply_specified", "quantity_specified",
                                   "drug_exposure_end_date_specified", "strength_specified", "sig_specified",
                                   "drug_exposure_days_specified", "n_records", "n_people"))
  expect_equal(result$strength_specified, rep("Yes", 6))
  expect_equal(result$n_records, c(4, 2, 1, 3, 1, 1))
  expect_equal(result$n_people, c(2, 1, 1, 2, 1, 1))
})

test_that("getIngredientsPresence error cases", {
  testDb <- getInputDb()

  expect_error(getIngredientPresence(cdm = null))
  expect_error(getIngredientPresence(cdm = testDb, drugRecordsTable = "test"))
  expect_error(getIngredientPresence(cdm = testDb,
                                     drugRecordsTable = "drug_exposure",
                                     drugStrengthTable = "test"))
})
