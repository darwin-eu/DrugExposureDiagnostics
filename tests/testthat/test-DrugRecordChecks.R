getTestData <- function() {
  drug_exposure <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5"),
    drug_concept_id = c("1", "1", "2", "2", "3"),
    drug = c("x", "x", "xx", "xx", "xxx"),
    ingredient_concept_id = c("1", "1", "2", "2", "2"),
    ingredient = c("a", "a", "b", "b", "b"),
    person_id = c("1", "2", "3", "4", "5"),
    drug_type_concept_id = c("1", "2", "3", "4", "5"),
    route_concept_id = c("1", "2", "3", "4", "5"),
    lot_number  = c("1", "2", "3", "4", "5"),
    provider_id = c("1", "2", "3", "4", "5"),
    visit_occurrence_id= c("1", "2", "3", "4", "5"),
    visit_detail_id= c("1", "2", "3", "4", "5"),
    drug_source_value= c("1", "2", "3", "4", "5"),
    drug_source_concept_id= c("1", "2", "3", "4", "5"),
    route_source_value= c("1", "2", "3", "4", "5"),
    dose_unit_source_value= c("1", "2", "3", "4", "5"),
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
    days_supply = c(NA,NA,NA,NA,366),
    quantity = c(10, 20, 30, 40, 50),
    stop_reason = rep("", 5),
    refills = rep(1, 5),
    sig = rep("", 5))

  mockDrugExposure(drug_exposure = drug_exposure)
}


test_that("summariseDrugExposureDuration test", {
  testData <- getTestData()
  result <- summariseDrugExposureDuration(testData, "drug_exposure", byConcept = FALSE)

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 15)
  expect_equal(colnames(result), c("ingredient_concept_id", "ingredient", "n_records", "n_non_negative_days", "n_negative_days",
                                   "proportion_negative_days", "minimum_drug_exposure_days", "q05_drug_exposure_days",
                                   "q10_drug_exposure_days", "q25_drug_exposure_days", "median_drug_exposure_days",
                                   "q75_drug_exposure_days", "q90_drug_exposure_days", "q95_drug_exposure_days",
                                   "maximum_drug_exposure_days"))

  resultByconcept <- summariseDrugExposureDuration(testData, "drug_exposure", byConcept = TRUE)
  expect_equal(nrow(resultByconcept), 3)
  expect_equal(resultByconcept$n_records, c(2,2,1))
})
