test_that("check if it can detect missing", {
  drug_exposure <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5"),
    drug_concept_id = c("1", "1", "2", "2", "3"),
    drug = c("a", "a", "b", "b", "c"),
    ingredient_concept_id = c("1", "1", "2", "2", "3"),
    ingredient = c("a", "a", "b", "b", "c"),
    person_id = c("1", "2", "3", "4", "5"),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"), as.Date("2011-01-01"),
      as.Date("2012-01-01"), NA,
      as.Date("2013-01-01")),
    drug_exposure_end_date = c(
      as.Date("2011-01-01"), NA,
      as.Date("2013-01-01"), as.Date("2010-01-01"),
      as.Date("2014-01-01")),
    days_supply = c(NA,365,366,365,366))
  #correct one should be c(na,na,366,na,365)
  #expect output: different_days_supply: 0,0,1
  #expect output: missing_days_supply_or_dates: 2,1,0
  #expect output: match_days_supply: 0,1,0

  testData <- mockDrugExposure(drug_exposure = drug_exposure)

  result <- checkDaysSupply(testData, byConcept = TRUE)

  expect_true(identical(as.integer(result$n_different_days_supply_and_drug_dates), as.integer(list(0,0,1))))

  expect_true(identical(as.integer(result$n_missing_days_supply_or_drug_dates), as.integer(c(2,1,0))))

  expect_true(identical(as.integer(result$n_days_supply_match_drug_dates), as.integer(c(0,1,0))))

  totalLength <- dim(testData$drug_exposure %>% dplyr::collect())[1]

  expect_true(sum(result$n_different_days_supply_and_drug_dates, result$n_days_supply_match_drug_dates,
                  result$n_missing_days_supply_or_drug_dates) == totalLength)

  expect_true(sum(result[result$drug_concept_id == 1, ]$n_different_days_supply_and_drug_dates,
                  result[result$drug_concept_id == 1, ]$n_days_supply_match_drug_dates,
                  result[result$drug_concept_id == 1, ]$proportion_missing_days_supply_or_drug_dates) == 1)

  expect_true(sum(result[result$drug_concept_id == 2, ]$proportion_different_days_supply_and_drug_dates,
                  result[result$drug_concept_id == 2, ]$proportion_days_supply_match_drug_dates,
                  result[result$drug_concept_id == 2, ]$proportion_missing_days_supply_or_drug_dates) == 1)

  expect_true(sum(result[result$drug_concept_id == 3, ]$proportion_different_days_supply_and_drug_dates,
                  result[result$drug_concept_id == 3, ]$proportion_days_supply_match_drug_dates,
                  result[result$drug_concept_id == 3, ]$proportion_missing_days_supply_or_drug_dates) == 1)

})

test_that("check if summarise days_supply works", {
  drug_exposure <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5", "5"),
    drug_concept_id = c("1", "1", "2", "2", "3", "4"),
    drug = c("a", "a", "b", "b", "c", "d"),
    person_id = c("1", "2", "3", "4", "5", "5"),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"), as.Date("2011-01-01"),
      as.Date("2012-01-01"), NA,
      as.Date("2013-01-01"),as.Date("2014-01-01")),
    drug_exposure_end_date = c(
      as.Date("2011-01-01"), NA,
      as.Date("2013-01-01"), as.Date("2010-01-01"),
      as.Date("2014-01-01"), as.Date("2015-01-04")),
    days_supply = c(NA,365,366,365,366, 368))

  testData <- mockDrugExposure(drug_exposure = drug_exposure)

  result <- summariseDaysSupply(testData)

  expect_equal(result$minimum_drug_exposure_days_supply, 365)
  expect_equal(result$n_records, 6)
  expect_equal(ncol(result), 12)
  expect_equal(result$n_people, 5)
})
