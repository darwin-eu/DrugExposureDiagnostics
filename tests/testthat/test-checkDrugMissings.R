getTestData <- function() {
  ingredient_drug_records <- tibble::tibble(
    drug_exposure_id = c(1, 2, 3, 0, 5),
    drug_concept_id = c(0, 0, 2, 2, 3),
    drug = c("x", "x", "x", "x", "x"),
    ingredient_concept_id = c(1, 1, 1, 1, 1),
    ingredient = c("a", "a", "a", "a", "a"),
    person_id = c(1, 2, 3, 4, 0),
    drug_type_concept_id = c(1, 2, 3, 4, 0),
    route_concept_id = c(1, 2, 3, 4, 0),
    lot_number  = seq(1, 5),
    provider_id = seq(1, 5),
    visit_occurrence_id= seq(1, 5),
    visit_detail_id= seq(1, 5),
    drug_source_value= seq(1, 5),
    drug_source_concept_id= seq(1, 5),
    route_source_value= seq(1, 5),
    dose_unit_source_value= seq(1, 5),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"), as.Date("2011-01-01"),
      as.Date("2012-01-01"), NA,
      as.Date("2013-01-01")),
    drug_exposure_end_date = c(
      as.Date("2011-01-01"), NA,
      as.Date("2013-01-01"), as.Date("2010-01-01"),
      as.Date("2014-01-01")),
    verbatim_end_date = c(
      as.Date("2011-01-01"), NA,
      as.Date("2013-01-01"), as.Date("2010-01-01"),
      as.Date("2014-01-01")),
    days_supply = c(NA, NA, NA, NA, 366),
    quantity = c(10, 20, 30, 40, 50),
    stop_reason = rep("", 5),
    refills = rep(1, 5),
    sig = rep("", 5))

  mockDrugExposure(ingredient_drug_records = ingredient_drug_records)
}

test_that("getDrugMissings overall", {
  testData <- getTestData()
  result <- getDrugMissings(testData, "ingredient_drug_records", byConcept = FALSE,
                            sampleSize = 100)

  expect_equal(nrow(result), 15)
  expect_equal(ncol(result), 8)
  expect_equal(colnames(result), c("ingredient_concept_id", "ingredient", "variable",
                                   "n_records", "n_sample", "n_records_not_missing_value",
                                   "n_records_missing_value", "proportion_records_missing_value"))
  missingDaysSupply <- result %>% dplyr::filter(variable == "n_missing_days_supply")
  expect_equal(missingDaysSupply %>% dplyr::pull(n_records_missing_value), 4)
  expect_equal(missingDaysSupply %>% dplyr::pull(proportion_records_missing_value), 0.8)

  missingDrugConcept <- result %>% dplyr::filter(variable == "n_missing_drug_concept_id")
  expect_equal(missingDrugConcept %>% dplyr::pull(n_records_missing_value), 2)
  expect_equal(missingDrugConcept %>% dplyr::pull(proportion_records_missing_value), 0.4)

  missingDrugType <- result %>% dplyr::filter(variable == "n_missing_drug_type_concept_id")
  expect_equal(missingDrugType %>% dplyr::pull(n_records_missing_value), 1)
  expect_equal(missingDrugType %>% dplyr::pull(proportion_records_missing_value), 0.2)

  missingRoute <- result %>% dplyr::filter(variable == "n_missing_route_concept_id")
  expect_equal(missingRoute %>% dplyr::pull(n_records_missing_value), 1)
  expect_equal(missingRoute %>% dplyr::pull(proportion_records_missing_value), 0.2)

  missingDrugExposure <- result %>% dplyr::filter(variable == "n_missing_drug_exposure_id")
  expect_equal(missingDrugExposure %>% dplyr::pull(n_records_missing_value), 1)
  expect_equal(missingDrugExposure %>% dplyr::pull(proportion_records_missing_value), 0.2)

  missingPerson <- result %>% dplyr::filter(variable == "n_missing_person_id")
  expect_equal(missingPerson %>% dplyr::pull(n_records_missing_value), 1)
  expect_equal(missingPerson %>% dplyr::pull(proportion_records_missing_value), 0.2)

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})

test_that("getDrugMissings byConcept", {
  testData <- getTestData()
  result <- getDrugMissings(testData, "ingredient_drug_records", byConcept = TRUE,
                            sampleSize = 100)

  expect_equal(nrow(result), 45)
  expect_equal(ncol(result), 10)
  expect_equal(colnames(result), c("drug_concept_id", "drug",
                                   "ingredient_concept_id", "ingredient", "variable",
                                   "n_records", "n_sample", "n_records_not_missing_value",
                                   "n_records_missing_value", "proportion_records_missing_value"))

  missingDaysSupply <- result %>%
    dplyr::filter(variable == "n_missing_days_supply") %>%
    dplyr::arrange(drug_concept_id)

  expect_equal(missingDaysSupply %>% dplyr::pull(n_records_missing_value), c(2, 2, 0))
  expect_equal(missingDaysSupply %>% dplyr::pull(proportion_records_missing_value), c(1, 1, 0))

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)
})
