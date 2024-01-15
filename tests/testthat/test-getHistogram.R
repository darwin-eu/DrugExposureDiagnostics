getInputDb <- function() {
  drugExposure <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
    drug_concept_id = c("1", "2", "3", "4", "5", "6", "1", "2", "3", "4", "5", "6"),
    ingredient_concept_id = c("1", "2", "3", "4", "5", "6", "1", "2", "3", "4", "5", "6"),
    ingredient = c("a", "b", "c", "d", "e", "f", "a", "b", "c", "d", "e", "f"),
    person_id = rep(c("1", "2", "3", "4", "5", "6"), 2),
    drug_exposure_start_date = rep(as.Date("2010-01-01"), 12),
    drug_exposure_end_date = c(as.Date("2010-01-11"), as.Date("2011-01-11"), as.Date("2011-01-11"),
                               as.Date("2011-01-11"), as.Date("2010-01-12"), as.Date("2010-01-02"),
                               as.Date("2010-04-10"), as.Date("2010-01-06"), as.Date("2010-01-11"),
                               as.Date("2010-01-10"), as.Date("2010-02-03"), as.Date("2010-01-10")),
    days_supply = c(11, 11, 11, 11, 12, 2, 100,6, 11, 10, 34, 10),
    quantity = c(100, 20, 10, 50, 0, 10, 100, 20, 10, 50, 0, 10))

   mockDrugExposure(drug_exposure = drugExposure)

}

test_that("test histogram plotting", {
  cdm <- getInputDb()
  resultDaysSupply <- createHistogram(cdm, "drug_exposure", type = "days_supply")
  expect_equal(resultDaysSupply$counts, c(10,1,0,0,1))
  expect_equal(resultDaysSupply$xname, "days_supply")
  resultQuantity <- createHistogram(cdm, "drug_exposure", type = "quantity")
  expect_equal(resultQuantity$counts, c(8,0,2,0,2))
  resultDuration <- createHistogram(cdm, "drug_exposure", type = "duration")

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
  })

test_that("test save load fucntionality for histograms", {
  cdm <- getInputDb()
  resultDaysSupply <- createHistogram(cdm, "drug_exposure", type = "days_supply")
  df <- hist2DataFrame(resultDaysSupply)
  reloadedHist <- dataFrame2Hist(df)
  expect_equal(resultDaysSupply, reloadedHist)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test NA input", {
  cdm <- getInputDb()
  cdm$drug_exposure <- cdm$drug_exposure %>% dplyr::mutate(days_supply = NA)
  resultDaysSupply <- createHistogram(cdm, "drug_exposure", type = "days_supply")
  expect_equal(resultDaysSupply, NULL)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

