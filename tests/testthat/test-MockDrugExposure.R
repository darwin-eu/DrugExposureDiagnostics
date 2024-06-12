test_that("check working example with defaults", {
  db <- mockDrugExposure()

  cdmCheck <- inherits(db, "cdm_reference")
  expect_true(cdmCheck)

  expect_true(nrow(db$drug_exposure %>%
                     dplyr::collect()) >= 1)
  expect_true(nrow(db$concept %>%
                     dplyr::collect()) >= 1)
  expect_true(nrow(db$concept_ancestor %>%
                     dplyr::collect()) >= 1)
  expect_true(nrow(db$drug_strength %>%
                     dplyr::collect()) >= 1)


  ancestor_conceptDbNames <- c(
    "ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation",
    "max_levels_of_separation"
  )
  ancestor_conceptNamesCheck <- all(ancestor_conceptDbNames %in%
                            names(db$concept_ancestor %>%
                                    utils::head(1) %>%
                                    dplyr::collect() %>%
                                    dplyr::rename_with(tolower)))

  drugstrengthNames <- c(
    "drug_concept_id",
    "ingredient_concept_id",
    "amount_value",
    "amount_unit_concept_id",
    "numerator_value",
    "numerator_unit_concept_id",
    "denominator_value",
    "denominator_unit_concept_id",
    "valid_start_date",
    "valid_end_date",
    "invalid_reason"
  )
  drugstrengthNamesCheck <- all(drugstrengthNames %in%
                               names(db$drug_strength %>%
                                       utils::head(1) %>%
                                       dplyr::collect() %>%
                                       dplyr::rename_with(tolower)))
  expect_true(drugstrengthNamesCheck)

  conceptNames <- c(
    "concept_id",
    "concept_name",
    "domain_id",
    "vocabulary_id",
    "concept_class_id",
    "standard_concept",
    "concept_code",
    "valid_start_date",
    "valid_end_date",
    "invalid_reason"
  )
  conceptNamesCheck <- all(conceptNames %in%
                                  names(db$concept %>%
                                          utils::head(1) %>%
                                          dplyr::collect() %>%
                                          dplyr::rename_with(tolower)))
  expect_true(conceptNamesCheck)

  drug_exposureNames <- c(
    "drug_exposure_id",
    "person_id",
    "drug_concept_id",
    "drug_exposure_start_date",
    "drug_exposure_end_date",
    "drug_type_concept_id ",
    "stop_reason",
    "refills",
    "quantity",
    "days_supply",
    "sig",
    "route_concept_id",
    "lot_number",
    "provider_id",
    "visit_occurrence_id",
    "drug_source_value",
    "drug_source_concept_id",
    "route_source_value",
    "dose_unit_source_value"
  )
  drug_exposureNamesCheck <- all(drug_exposureNames %in%
                             names(db$drug_exposure %>%
                                     utils::head(1) %>%
                                     dplyr::collect() %>%
                                     dplyr::rename_with(tolower)))
  expect_true(conceptNamesCheck)



  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})

test_that("check working example with drug_strength", {
  drug_strength <- tibble::tibble(
    drug_concept_id = "1",
    ingredient_concept_id = "1",
    valid_start_date = c(
      as.Date("2010-02-05")
    ),
    valid_end_date = c(
      as.Date("2010-02-05")
    )
  )

  db <- mockDrugExposure(drug_strength = drug_strength)

  expect_true(nrow(db$drug_strength %>%
                     dplyr::collect()) == 1)

  drug_strengthDbNames <- c(
    "drug_concept_id", "ingredient_concept_id",
    "ingredient_concept_id", "valid_start_date"
  )
  drug_strengthCheck <- all(drug_strengthDbNames %in%
                             names(db$drug_strength %>%
                                     utils::head(1) %>%
                                     dplyr::collect() %>%
                                     dplyr::rename_with(tolower)))
  expect_true(drug_strengthCheck)

  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})

test_that("check working example with drug drug_exposure", {
  drug_exposure <- tibble::tibble(
    drug_exposure_id = "1",
    person_id = c("1"),
    drug_concept_id = c("1"),
    drug_type_concept_id = c("1"),
    drug_exposure_start_date = c(
      as.Date("2010-02-05")
    ),
    drug_exposure_end_date = c(
      as.Date("2010-02-05")
    )
  )

  db <- mockDrugExposure(drug_exposure = drug_exposure)

  expect_true(nrow(db$drug_exposure %>%
                     dplyr::collect()) == 1)

  drug_exposureDbNames <- c(
    "drug_exposure_id", "drug_exposure_start_date",
    "drug_exposure_end_date"
  )
  drug_exposureCheck <- all(drug_exposureDbNames %in%
                              names(db$drug_exposure %>%
                                      utils::head(1) %>%
                                      dplyr::collect() %>%
                                      dplyr::rename_with(tolower)))
  expect_true(drug_exposureCheck)


  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})

test_that("check working example with drug_exposure options", {
  db <- mockDrugExposure(drug_exposure_size = 1, patient_size = 1)
  expect_true(nrow(db$drug_exposure %>%
                     dplyr::collect()) == 1)
  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
  db <- mockDrugExposure(drug_exposure_size = 2, patient_size = 1)
  expect_true(nrow(db$drug_exposure %>%
                     dplyr::collect()) == 2)
  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})

test_that("check working example with drug_strength default options", {
  db <- mockDrugExposure(drug_exposure_size = 20,
                         patient_size = 10, amount_val = c(100,200), denom_unit = NA, num_unit= NA)
  expect_true(nrow(db$drug_strength %>%
                     dplyr::select("amount_value")%>%dplyr::distinct()%>%dplyr::collect()) == 2)
  expect_true(nrow(db$drug_strength %>%
                     dplyr::select("denominator_value")%>%dplyr::distinct()%>%dplyr::collect()) == 1)
  expect_true(nrow(db$drug_strength %>%
                     dplyr::select("numerator_value")%>%dplyr::distinct()%>%dplyr::collect()) == 1)

  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})


test_that("check working example with drug_strength num_denom only", {
  db <- mockDrugExposure(amount_val = NA, amount_unit = NA)
  expect_true(nrow(db$drug_strength %>%
                        dplyr::select("amount_value")%>%dplyr::distinct()%>%dplyr::collect()) == 1)
  expect_true(nrow(db$drug_strength %>%
                     dplyr::select("denominator_value")%>%dplyr::distinct()%>%dplyr::collect()) == 1)
  expect_true(nrow(db$drug_strength %>%
                     dplyr::select("numerator_value")%>%dplyr::distinct()%>%dplyr::collect()) == 1)

  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})

test_that("check expected errors", {

    testthat::expect_error(
    mockDrugExposure(drug_exposure_size = "x")
  )
  testthat::expect_error(
    mockDrugExposure(patient_size = "x")
  )
  testthat::expect_error(
    mockDrugExposure(den_val = "x")
  )
  testthat::expect_error(
    mockDrugExposure(patient_size = -1)
  )

})
