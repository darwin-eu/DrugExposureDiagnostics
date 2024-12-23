# execute checks function with some defaults
executeChecksMock <- function(cdm,
                              ingredients = c(1125315),
                              subsetToConceptId = NULL,
                              checks = c("missing", "exposureDuration", "quantity"),
                              sample = 10000,
                              tablePrefix = NULL,
                              earliestStartDate = "2010-01-01",
                              verbose = FALSE,
                              minCellCount = 5,
                              outputFolder = NULL,
                              databaseId = CDMConnector::cdmName(cdm)) {
  executeChecks(cdm,
                ingredients = ingredients,
                subsetToConceptId = subsetToConceptId,
                checks = checks,
                sample = sample,
                tablePrefix = tablePrefix,
                earliestStartDate = earliestStartDate,
                verbose = verbose,
                minCellCount = minCellCount,
                outputFolder = outputFolder,
                databaseId = databaseId)
}

test_that("execute default checks, default ingredient, verbose", {
  cdm <- mockDrugExposure()

  result <- executeChecksMock(cdm, verbose = TRUE)

  # checks
  expect_equal(length(result), 8)
  expect_equal(nrow(result$conceptSummary), 6)
  expect_equal(ncol(result$conceptSummary), 26)
  expect_equal(colnames(result$conceptSummary), c(
    "drug_concept_id", "drug", "ingredient_concept_id",
    "ingredient", "n_records", "n_patients", "domain_id", "vocabulary_id", "concept_class_id",
    "standard_concept", "concept_code", "valid_start_date", "valid_end_date", "invalid_reason", "amount_value",
    "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id", "numerator_unit", "denominator_value",
    "denominator_unit_concept_id", "denominator_unit", "box_size", "amount_unit", "dose_form",
    "result_obscured"
  ))

  expect_true(any(grepl("acetaminophen", result$conceptSummary$drug)))
  # check that all colnames don't have any uppercase characters (this might not be supported on some databases)
  allColnames <- unlist(lapply(names(result), FUN = function(name) {
    lapply(colnames(result[[name]]), FUN = function(colname) {
      colname
    })
  }))
  expect_true(all(unlist(lapply(allColnames, FUN = function(colName) {
    unlist(gregexpr("[A-Z]", colName))
  })) == -1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("execute all checks, given ingredient", {
  cdm <- mockDrugExposure()

  result <- executeChecksMock(
    cdm = cdm,
    ingredients = 1125315, # acetaminophen
    checks = c(
      "missing", "exposureDuration", "type", "route",
      "sourceConcept", "daysSupply", "verbatimEndDate",
      "dose", "sig", "quantity", "diagnosticsSummary"
    )
  )
  # checks
  expect_equal(length(result), 21)
  expect_equal(nrow(result$conceptSummary), 6)
  expect_true(any(grepl("acetaminophen", result$conceptSummary$drug)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("execute some checks, given ingredient", {
  cdm <- mockDrugExposure()

  result <- executeChecksMock(cdm = cdm, ingredients = 1125315, checks = c("missing", "verbatimEndDate")) # acetaminophen

  # checks
  expect_equal(length(result), 6)
  expect_equal(names(result), c(
    "conceptSummary", "missingValuesOverall", "missingValuesByConcept",
    "drugVerbatimEndDate", "drugVerbatimEndDateByConcept", "metadata"
  ))
  expect_equal(nrow(result$conceptSummary), 6)
  expect_true(any(grepl("acetaminophen", result$conceptSummary$drug)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("execute all checks: expected errors", {
  cdm <- mockDrugExposure()

  expect_error(executeChecksMock(cdm = "a", 1125315))
  expect_error(executeChecksMock(cdm, "a"))
  expect_error(executeChecksMock(cdm, 33)) # not an ingredient

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("each check", {
  cdm <- mockDrugExposure()

  concepts_db <- ingredientDescendantsInDb(cdm, 1125315) # acetaminophen

  expect_true(nrow(concepts_db %>%
    dplyr::collect()) > 0)

  # add to ref
  cdm[["ingredient_concepts"]] <- concepts_db

  # get the relevant drug records
  cdm[["ingredient_drug_records"]] <- getDrugRecords(cdm, 1125315, "ingredient_concepts")

  # summarise missing
  getDrugMissings(cdm, "ingredient_drug_records", byConcept = TRUE)
  getDrugMissings(cdm, "ingredient_drug_records", byConcept = FALSE)

  # summarise concept related info
  getDrugTypes(cdm, "ingredient_drug_records", byConcept = TRUE)
  getDrugTypes(cdm, "ingredient_drug_records", byConcept = FALSE)

  getDrugRoutes(cdm, "ingredient_drug_records", byConcept = TRUE)
  getDrugRoutes(cdm, "ingredient_drug_records", byConcept = FALSE)

  getDrugSourceConcepts(cdm, "ingredient_drug_records")

  # summarise dates
  summariseDrugExposureDuration(cdm, "ingredient_drug_records", byConcept = TRUE)
  summariseDrugExposureDuration(cdm, "ingredient_drug_records", byConcept = FALSE)

  # need to add checks for the above summaries .....


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("each check: expected errors", {
  cdm <- mockDrugExposure()

  expect_error(ingredientDescendantsInDb(cdm, "a"))
  expect_error(ingredientDescendantsInDb(cdm, 33))

  concepts_db <- ingredientDescendantsInDb(cdm, 1125315) # acetaminophen
  # add to ref
  cdm[["ingredient_concepts"]] <- concepts_db

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
  cdm <- mockDrugExposure()

  cdm[["ingredient_concepts"]] <- ingredientDescendantsInDb(cdm, 1125315) # acetaminophen
  cdm[["ingredient_drug_records"]] <- getDrugRecords(cdm, 1125315, "ingredient_concepts")

  result <- checkDaysSupply(cdm, drugRecordsTable = "ingredient_drug_records")

  data <- cdm[["ingredient_drug_records"]] %>% dplyr::pull("drug_concept_id")

  expect_equal(nrow(result), length(unique(data)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("sampling", {
  cdm <- mockDrugExposure()

  result <- executeChecksMock(
    cdm = cdm, ingredients = 1125315, # acetaminophen
    sample = 50
  )

  # sampling shouldn't affect ingredient concepts
  expect_true(sum(result$conceptSummary$n_records) > 50)
  # checks where sampling should have been implemented
  expect_true(max(result$missingValuesOverall$n_records_missing_value, na.rm = TRUE) <= 50)

  expect_message(executeChecksMock(
    cdm = cdm, ingredients = 1125315, # acetaminophen
    sample = 50, earliestStartDate = Sys.Date()
  ))

  expect_error(executeChecksMock(
    cdm = cdm, ingredients = 1125315, # acetaminophen
    sample = 50, earliestStartDate = -1
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("summary", {
  cdm <- mockDrugExposure()
  result <- executeChecksMock(cdm = cdm, ingredients = 1125315, checks = c(
    "missing", "type",
    "exposureDuration", "dose", "route", "quantity", "diagnosticsSummary"
  ))

  expect_equal(
    names(result$diagnosticsSummary),
    c(
      "ingredient", "ingredient_concept_id", "n_records", "n_patients",
      "proportion_of_records_by_drug_type",
      "proportion_of_records_by_route_type",
      "proportion_of_records_with_dose_form",
      "missing_quantity_exp_start_end_days_supply",
      "n_dose_and_missingness",
      "median_daily_dose_q05_q95",
      "median_quantity_q05_q95",
      "median_drug_exposure_days_q05_q95",
      "proportion_of_records_with_negative_drug_exposure_days",
      "result_obscured"
    )
  )

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("subset on specific concepts", {
  cdm <- mockDrugExposure()
  result_all <- executeChecksMock(
    cdm = cdm,
    ingredients = 1125315,
    subsetToConceptId = NULL
  )

  expect_true(all(result_all$conceptSummary$drug_concept_id %in%
    c(40162522, 19133768, 1127433, 1127078, 40229134, 40231925)))


  result_subset <- executeChecksMock(
    cdm = cdm,
    ingredients = 1125315,
    subsetToConceptId = 40162522
  )
  expect_true(all(result_subset$conceptSummary$drug_concept_id %in%
    c(40162522)))

  result_subset2 <- executeChecksMock(
    cdm = cdm,
    ingredients = 1125315,
    subsetToConceptId = c(40162522, 1127078)
  )

  expect_true(all(result_subset2$conceptSummary$drug_concept_id %in%
    c(40162522, 1127078)))

  # check exclude
  result_subset3 <- executeChecksMock(
    cdm = cdm,
    ingredients = 1125315,
    subsetToConceptId = c(-40162522, -19133768, -1127433, -40229134, -40231925)
  )

  expect_true(all(result_subset3$conceptSummary$drug_concept_id == 1127078))

  # combine exclude and include
  result_subset4 <- executeChecksMock(
    cdm = cdm,
    ingredients = 1125315,
    subsetToConceptId = c(-40162522, -19133768, -1127433, -40229134, -40231925, 1127078)
  )

  expect_true(identical(result_subset3, result_subset4))

  # exclude and include same ID -> error
  expect_error(executeChecksMock(
    cdm = cdm,
    ingredients = 1125315,
    subsetToConceptId = c(-40162522, 40162522)
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("obscuring results by minCellCount", {
  cdm <- mockDrugExposure()
  result_all <- executeChecksMock(
    cdm = cdm,
    ingredients = 1125315,
    minCellCount = 13
  )

  summary <- result_all$conceptSummary %>% dplyr::arrange(ingredient_concept_id, drug_concept_id)
  expect_equal(summary$n_records, c(19, NA, 14, 18, NA, NA))
  expect_equal(summary$n_patients, c(13, NA, 13, 15, NA, NA))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("use tablePrefix", {
  cdm <- mockDrugExposure()
  expect_no_error(executeChecksMock(
    cdm = cdm,
    ingredients = 1125315,
    tablePrefix = "pre"
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("empty drug_strength table gives an error", {
  cdm <- mockDrugExposure()
  cdm$drug_strength <- cdm$drug_strength %>%
    dplyr::filter(ingredient_concept_id != 1125315)

  expect_error(executeChecksMock(
    cdm = cdm,
    ingredients = 1125315
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("results from multiple ingredients should be joined also if there is one in between that doesn't exist", {
  cdm <- mockDrugExposure()

  result <- executeChecksMock(
    cdm = cdm,
    ingredients = c(1125315, 36854851, 1125315)
  )
  # should have 2 times result for 1125315
  summary <- result$conceptSummary
  expect_equal(nrow(summary), 12)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("sampleSize is null, no sampling must take place, all data from ingredient_drug_records must be used", {
  ingredients <- c(1125315)

  cdm <- mockDrugExposure()

  selection <- cdm$drug_exposure %>%
    dplyr::inner_join(cdm$drug_strength,
      by = "drug_concept_id"
    ) %>%
    dplyr::filter(.data$ingredient_concept_id == 1125315) %>%
    dplyr::filter(.data$drug_exposure_start_date > "2010-01-01")


  expectedLength <- selection %>%
    dplyr::tally() %>%
    dplyr::pull(.data$n)

  result <- executeChecksMock(
    cdm = cdm,
    checks = c("missing", "exposureDuration", "type", "route", "dose", "quantity", "diagnosticsSummary"),
    ingredients = ingredients,
    sample = NULL
  )

  for (colName in names(result)) {
    if ("n_sample" %in% names(result[[colName]])) {
      nSampleValues <- result[[colName]][["n_sample"]]
      resultObscuredValues <- result[[colName]][["result_obscured"]]

      expect_true("result_obscured" %in% names(result[[colName]]))
      expect_equal(length(nSampleValues), length(resultObscuredValues))

      for (i in seq_along(nSampleValues)) {
        if (resultObscuredValues[i] == FALSE) {
          expect_equal(nSampleValues[i], expectedLength, info = paste("Issue in ", colName))
        }
      }
    }
  }

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("package version and name are included in the results", {
  ingredients <- c(1125315)

  cdm <- mockDrugExposure()

  result <- executeChecksMock(
    cdm = cdm,
    ingredients = ingredients,
    sample = NULL
  )


  expect_true("metadata" %in% names(result))
  expect_equal(c(
    "cdm_name", "cdm_source_name", "cdm_description",
    "cdm_documentation_reference", "cdm_version",
    "cdm_holder", "cdm_release_date",
    "vocabulary_version", "person_count",
    "observation_period_count", "earliest_observation_period_start_date",
    "latest_observation_period_end_date",
    "snapshot_date", "package_version"
  ), names(result[["metadata"]]))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
