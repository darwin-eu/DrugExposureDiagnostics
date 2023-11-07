getTestData <- function(ingredientId) {
  drug_exposure <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5"),
    drug_concept_id = c(ingredientId, "40162522", "1127078", "1127433", "40229134"),
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

  concept = dplyr::tibble(
    concept_id = c(as.numeric(ingredientId), 40162522, 1127078, 1127433, 40229134, 19133768),
    concept_name = c("ingredient1", "drug2", "drug3", "drug4", "drug5", "milligram"),
    domain_id = paste0("Drug", seq(6)),
    vocabulary_id = c(rep("RxNorm", 5), "Unit"),
    standard_concept = "S",
    concept_class_id = rep("Ingredient", 6)
  )

  cdm <- mockDrugExposure(drug_exposure = drug_exposure, concept = concept)
  cdm[["ingredient_concepts"]] <- ingredientDescendantsInDb(cdm = cdm, ingredient = ingredientId, drugRecordsTable = "drug_exposure")
  return(cdm)
}

test_that("getDrugRecords", {
  ingredientId <- 1125315
  cdm <- getTestData(ingredientId)
  result <- getDrugRecords(cdm = cdm, ingredient = ingredientId, includedConceptsTable = "ingredient_concepts") %>%
    dplyr::collect()

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 24)
  expect_equal(colnames(result), c("drug_exposure_id", "drug_concept_id", "person_id",
                                   "drug_type_concept_id",  "route_concept_id",
                                   "lot_number", "provider_id", "visit_occurrence_id",
                                   "visit_detail_id", "drug_source_value", "drug_source_concept_id",
                                   "route_source_value", "dose_unit_source_value", "drug_exposure_start_date",
                                   "drug_exposure_end_date", "verbatim_end_date", "days_supply",
                                   "quantity", "stop_reason", "refills", "sig", "drug", "ingredient_concept_id",
                                   "ingredient"))

  # non exising ingredient should deliver no results
  result <- getDrugRecords(cdm = cdm, ingredient = 123456, includedConceptsTable = "ingredient_concepts") %>%
    dplyr::collect()
  expect_equal(nrow(result), 0)
})

test_that("getDrugRecords invalid inputs", {
  ingredientId <- "1125315"
  cdm <- getTestData(ingredientId)
  expect_error(getDrugRecords(cdm = NULL, ingredient = ingredientId, includedConceptsTable = "ingredient_concepts"))
  expect_error(getDrugRecords(cdm = cdm, ingredient = NULL, includedConceptsTable = "ingredient_concepts"))
  expect_error(getDrugRecords(cdm = cdm, ingredient = ingredientId, includedConceptsTable = "ingredient_concepts_none"))
})
