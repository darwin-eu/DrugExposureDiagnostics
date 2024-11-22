getTestData <- function() {
  ingredient_drug_records <- tibble::tibble(
    drug_exposure_id = c("1", "2", "3", "4", "5"),
    drug_concept_id = c("1", "2", "3", "4", "5"),
    drug = c("x", "x", "x", "x", "x"),
    ingredient_concept_id = c("1", "1", "1", "1", "1"),
    ingredient = c("a", "a", "a", "a", "a"),
    person_id = c("1", "2", "3", "4", "5"),
    drug_type_concept_id = rep("38000177", 5),
    route_concept_id = c("1", "1", "3", "3", "3"),
    lot_number = c("1", "2", "3", "4", "5"),
    provider_id = c("1", "2", "3", "4", "5"),
    visit_occurrence_id = c("1", "2", "3", "4", "5"),
    visit_detail_id = c("1", "2", "3", "4", "5"),
    drug_source_value = c("1", "2", "3", "4", "5"),
    drug_source_concept_id = c("1", "2", "3", "4", "5"),
    route_source_value = c("1", "2", "3", "4", "5"),
    dose_unit_source_value = c("1", "2", "3", "4", "5"),
    drug_exposure_start_date = c(
      as.Date("2010-01-01"), as.Date("2011-01-01"),
      as.Date("2012-01-01"), NA,
      as.Date("2013-01-01")
    ),
    drug_exposure_end_date = c(
      as.Date("2011-01-01"), NA,
      as.Date("2013-01-01"), as.Date("2010-01-01"),
      as.Date("2014-01-01")
    ),
    verbatim_end_date = c(
      as.Date("2011-01-01"), NA,
      as.Date("2013-01-01"), as.Date("2010-01-01"),
      as.Date("2014-01-01")
    ),
    days_supply = c(NA, NA, NA, NA, 366),
    quantity = c(10, 20, 30, 40, 50),
    stop_reason = rep("", 5),
    refills = rep(1, 5),
    sig = rep("", 5)
  )

  mockDrugExposure(ingredient_drug_records = ingredient_drug_records)
}

test_that("getDrugTypes", {
  cdm <- getTestData()

  result <- getDrugTypes(cdm, "ingredient_drug_records", byConcept = FALSE) %>%
    dplyr::collect()

  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 8)
  expect_equal(colnames(result), c(
    "ingredient_concept_id",
    "ingredient", "drug_type_concept_id", "drug_type",
    "n_records", "n_sample", "n_person",
    "proportion_records"
  ))
  expect_equal(result$drug_type, c("Prescription written"))
  expect_equal(result$proportion_records, c(1 / 1))

  # byConcept
  result <- getDrugTypes(cdm, "ingredient_drug_records", byConcept = TRUE) %>%
    dplyr::collect()

  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 10)
  expect_equal(colnames(result), c(
    "drug_concept_id", "drug", "ingredient_concept_id",
    "ingredient", "drug_type_concept_id", "drug_type",
    "n_records", "n_sample", "n_person",
    "proportion_records"
  ))
  expect_true(all(result$drug_type %in% c("Prescription written")))
  expect_equal(result$proportion_records, c(1 / 5, 1 / 5, 1 / 5, 1 / 5, 1 / 5))
})
