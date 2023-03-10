test_that("execute all checks, default ingredient, verbose", {

  cdm <- getEunomiaCdm()

  result <- executeChecks(cdm, verbose = TRUE)

  # checks
  expect_equal(length(result), 27)
  expect_equal(nrow(result$conceptSummary), 2)
  expect_true(any(grepl("Acetaminophen", result$conceptSummary$drug)))
  # check that all colnames don't have any uppercase characters (this might not be supported on some databases)
  allColnames <- unlist(lapply(names(result), FUN = function(name)  {
    lapply(colnames(result[[name]]), FUN = function(colname) {colname} )
  }))
  expect_true(all(unlist(lapply(allColnames, FUN = function(colName) {
    unlist(gregexpr("[A-Z]", colName))
  })) == -1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("execute all checks, given ingredient", {
  cdm <- getEunomiaCdm(1125315)

  result <- executeChecks(cdm, 1125315) #acetaminophen

  # checks
  expect_equal(length(result), 27)
  expect_equal(nrow(result$conceptSummary), 2)
  expect_true(all(grepl("Acetaminophen", result$conceptSummary$drug)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("execute some checks, given ingredient", {
  cdm <- getEunomiaCdm(1125315)

  result <- executeChecks(cdm = cdm, ingredients = 1125315, checks = c("missing", "verbatimEndDate")) #acetaminophen

  # checks
  expect_equal(length(result), 8)
  expect_equal(names(result), c("conceptSummary", "missingValuesOverall",
                                "missingValuesByConcept", "drugVerbatimEndDate",
                                "drugVerbatimEndDateByConcept", "drugIngredientOverview",
                                "drugIngredientPresence", "diagnostics_summary"))
  expect_equal(nrow(result$conceptSummary), 2)
  expect_true(all(grepl("Acetaminophen", result$conceptSummary$drug)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("execute all checks: expected errors", {
  cdm <- getEunomiaCdm(1125315)

  expect_error(executeChecks(cdm="a", 1125315))
  expect_error(executeChecks(cdm, "a"))
  expect_error(executeChecks(cdm, 33)) # not an ingredient

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("each check", {
  cdm <- getEunomiaCdm(1125315)

  concepts_db<-ingredientDescendantsInDb(cdm, 1125315) #acetaminophen

  expect_true(nrow(concepts_db %>%
                     dplyr::collect())>0)

  # add to ref
  cdm[["ingredient_concepts"]]<-concepts_db

  # get the relevant drug records
  cdm[["ingredient_drug_records"]]<-getDrugRecords(cdm, 1125315, "ingredient_concepts")

  # summarise missing
  getDrugMissings(cdm, "ingredient_drug_records", byConcept=TRUE)
  getDrugMissings(cdm, "ingredient_drug_records", byConcept=FALSE)

  # summarise concept related info
  getDrugTypes(cdm, "ingredient_drug_records", byConcept=TRUE)
  getDrugTypes(cdm, "ingredient_drug_records", byConcept=FALSE)

  getDrugRoutes(cdm, "ingredient_drug_records", byConcept=TRUE)
  getDrugRoutes(cdm, "ingredient_drug_records", byConcept=FALSE)

  getDrugSourceConcepts(cdm, "ingredient_drug_records", byConcept=TRUE)
  getDrugSourceConcepts(cdm, "ingredient_drug_records", byConcept=FALSE)

  # summarise dates
  summariseDrugExposureDuration(cdm, "ingredient_drug_records", byConcept=TRUE)
  summariseDrugExposureDuration(cdm, "ingredient_drug_records", byConcept=FALSE)

  # need to add checks for the above summaries .....


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("each check: expected errors", {
  cdm <- getEunomiaCdm(1125315)

  expect_error(ingredientDescendantsInDb(cdm, "a"))
  expect_error(ingredientDescendantsInDb(cdm, 33))

  concepts_db<-ingredientDescendantsInDb(cdm, 1125315) #acetaminophen
  # add to ref
  cdm[["ingredient_concepts"]]<-concepts_db

  # get the relevant drug records
  expect_error(getDrugRecords(cdm, "does_not_exist"))

  cdm[["ingredient_drug_records"]] <- getDrugRecords(cdm, 1125315, "ingredient_concepts")

  expect_error(getDrugMissings(cdm, "does_not_exist"))
  expect_error(getDrugTypes(cdm, "does_not_exist"))
  expect_error(getDrugRoutes(cdm, "does_not_exist"))
  expect_error(getDrugSourceConcepts(cdm, "does_not_exist"))
  expect_error(summariseDrugExposureDuration(cdm, "does_not_exist"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("get number of Days_supply that are incorrect", {
  cdm <- getEunomiaCdm(1125315)

  cdm[["ingredient_concepts"]] <- ingredientDescendantsInDb(cdm, 1125315) #acetaminophen
  cdm[["ingredient_drug_records"]] <- getDrugRecords(cdm, 1125315, "ingredient_concepts")

  result <- checkDaysSupply(cdm, drugRecordsTable = "ingredient_drug_records")

  data <- cdm[["ingredient_drug_records"]] %>% dplyr::pull("drug_concept_id")

  expect_equal(nrow(result), length(unique(data)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("sampling", {
  cdm <- getEunomiaCdm(1125315)

  result <- executeChecks(cdm = cdm, ingredients = 1125315, #acetaminophen
                          sample = 50)

  # sampling shouldn't affect ingredient concepts
  expect_true(sum(result$conceptSummary$n_records) > 50)
  # checks where sampling should have been implemented
  expect_true(max(result$missingValuesOverall$n_records_missing_value, na.rm = TRUE) <= 50)
  expect_true(max(result$drugTypesByConcept$n_records, na.rm = TRUE) <= 50)

  expect_message(executeChecks(cdm = cdm, ingredients = 1125315, #acetaminophen
                               sample = 50, earliestStartDate = Sys.Date()))

  expect_error(executeChecks(cdm = cdm, ingredients = 1125315, #acetaminophen
                               sample = 50, earliestStartDate = -1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("summary", {
  cdm <- getEunomiaCdm(1125315)
  result <- executeChecks(cdm = cdm, ingredients = 1125315)

  expect_equal(
    names(result$diagnostics_summary),
    c("ingredient_concept_id","ingredient",
      "n_records",
      "proportion_of_records_with_dose_form",
      "proportion_of_records_by_route_type",
      "proportion_of_records_by_drug_type",
      "proportion_of_records_with_negative_drug_exposure_days",
      "median_drug_exposure_days_q05_q95",
      "proportion_of_records_missing_days_supply_or_dates",
      "median_quantity_q05_q95",
      "proportion_of_records_missing_denominator_unit_concept_id",
      "median_amount_value_q05_q95",
      "result_obscured"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
