test_that("check summary calculation", {
  drug_exposure <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5"),
    drug_concept_id = c("1", "1", "2", "2", "3"),
    drug = c("x", "x", "xx", "xx", "xxx"),
    ingredient_concept_id = c("1", "1", "1", "1", "1"),
    ingredient = c("a", "a", "a", "a", "a"),
    person_id = c("1", "2", "3", "4", "5"),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"), as.Date("2011-01-01"),
      as.Date("2012-01-01"), NA,
      as.Date("2013-01-01")),
    drug_exposure_end_date = c(
      as.Date("2011-01-01"), NA,
      as.Date("2013-01-01"), as.Date("2010-01-01"),
      as.Date("2014-01-01")),
    days_supply = c(NA,365,366,365,366),
    quantity = c(10, 20, 30, 40, 50))

  testData <- mockDrugExposure(drug_exposure = drug_exposure)

  summarisedData <- summariseQuantity(testData)
  expect_true(identical(summarisedData$minimum_drug_exposure_quantity, c(10,30,50)))
  expect_true(identical(summarisedData$maximum_drug_exposure_quantity, c(20,40,50)))
  expect_equal(length(summarisedData$q05_drug_exposure_quantity)[1],3)

  #test that byConcept gives 1 result
  summarisedData <- summariseQuantity(testData, byConcept = F)
  expect_equal(nrow(summarisedData)[1], 1)
})





