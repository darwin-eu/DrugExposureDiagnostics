# Copyright 2024 DARWIN EU®
#
# This file is part of DrugExposureDiagnostics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Check missings in drug exposure records
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable modified drug exposure table
#' @param byConcept by individual drug Concept
#' @param sampleSize the sample size given in execute checks
#'
#' @return a table with a summary of missing records
getDrugMissings <- function(cdm,
                            drugRecordsTable = "ingredient_drug_records",
                            byConcept = TRUE,
                            sampleSize = 10000) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(
    cdm = cdm, tableName = drugRecordsTable,
    messageStore = errorMessage
  )
  checkLogical(byConcept, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (isTRUE(byConcept)) {
    grouping <- c("drug_concept_id","drug",
                  "ingredient_concept_id", "ingredient")
  } else {
    grouping <- c("ingredient_concept_id","ingredient")
  }

  records <- cdm[[drugRecordsTable]]

  # summarise missings
  summMissings <- records %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(
      n_records = as.integer(dplyr::n()),
      n_sample = .env$sampleSize,
      n_missing_drug_exposure_start_date =
        sum(dplyr::case_when(
          is.na(.data$drug_exposure_start_date) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_drug_exposure_end_date =
        sum(dplyr::case_when(
          is.na(.data$drug_exposure_end_date) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_verbatim_end_date =
        sum(dplyr::case_when(
          is.na(.data$verbatim_end_date) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_drug_type_concept_id =
        sum(dplyr::case_when(
          is.na(.data$drug_type_concept_id) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_stop_reason =
        sum(dplyr::case_when(
          is.na(.data$stop_reason) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_refills =
        sum(dplyr::case_when(
          is.na(.data$refills) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_quantity =
        sum(dplyr::case_when(
          is.na(.data$quantity) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_days_supply =
        sum(dplyr::case_when(
          is.na(.data$days_supply) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_sig =
        sum(dplyr::case_when(
          is.na(.data$sig) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_route_concept_id =
        sum(dplyr::case_when(
          is.na(.data$route_concept_id) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_lot_number =
        sum(dplyr::case_when(
          is.na(.data$lot_number) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_provider_id =
        sum(dplyr::case_when(
          is.na(.data$provider_id) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_visit_occurrence_id =
        sum(dplyr::case_when(
          is.na(.data$visit_occurrence_id) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_visit_detail_id =
        sum(dplyr::case_when(
          is.na(.data$visit_detail_id) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_drug_source_value =
        sum(dplyr::case_when(
          is.na(.data$drug_source_value) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_drug_source_concept_id =
        sum(dplyr::case_when(
          is.na(.data$drug_source_concept_id) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_route_source_value =
        sum(dplyr::case_when(
          is.na(.data$route_source_value) ~ 1, TRUE ~ 0), na.rm = T),
      n_missing_dose_unit_source_value =
        sum(dplyr::case_when(
          is.na(.data$dose_unit_source_value) ~ 1, TRUE ~ 0), na.rm = T)
    ) %>%
    dplyr::collect() %>%
    tidyr::pivot_longer(!tidyselect::any_of(c("drug_concept_id", "drug",
                                              "ingredient_concept_id",
                                              "ingredient","n_records","n_sample")),
                        names_to = "variable",
                        values_to = "n_records_missing_value")

  # add prop
  summMissings <- summMissings %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::relocate("n_records", .after = "variable") %>%
    dplyr::relocate("n_sample", .after = "n_records") %>%
    dplyr::mutate(n_records_not_missing_value = .data$n_records - .data$n_records_missing_value) %>%
    dplyr::mutate(proportion_records_missing_value = .data$n_records_missing_value / .data$n_records) %>%
    dplyr::relocate("n_records_missing_value", .after = "n_records_not_missing_value")

  summMissings$total_by_concept <- NULL

  return(summMissings)
}

#' Get drug exposure record types
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable modified drug exposure table
#' @param byConcept by individual drug Concept
#' @param sampleSize the sample size given in execute checks
#'
#' @return a table with the drug exposure record types
getDrugTypes <- function(cdm,
                         drugRecordsTable = "drug_exposure",
                         byConcept = TRUE,
                         sampleSize = 10000) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(cdm = cdm, tableName = drugRecordsTable, messageStore = errorMessage)
  checkLogical(byConcept, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (isTRUE(byConcept)) {
    grouping <- c("drug_concept_id", "drug",
                  "ingredient_concept_id",
                  "ingredient","drug_type_concept_id")
  } else {
    grouping <- c("ingredient_concept_id","ingredient",
                  "drug_type_concept_id")
  }

  summ <- cdm[[drugRecordsTable]] %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(n_records = as.integer(dplyr::n()),
                     n_sample = .env$sampleSize) %>%
    CDMConnector::computeQuery() %>%
    dplyr::left_join(cdm$concept %>%
                       dplyr::rename("drug_type_concept_id" = "concept_id",
                                     "drug_type" = "concept_name") %>%
                       dplyr::select("drug_type_concept_id", "drug_type"),
                     by = "drug_type_concept_id") %>%
    dplyr::select(tidyselect::any_of(
      c("drug_concept_id", "drug",
        "ingredient_concept_id", "ingredient",
        "drug_type_concept_id",
         "drug_type", "n_records")
    ))

  return(summ)
}

#' Get drug exposure route types
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable modified drug exposure table
#' @param byConcept by individual drug Concept
#' @param sampleSize the sample size given in execute checks
#'
#' @return a table with the drug exposure route types
getDrugRoutes <- function(cdm,
                          drugRecordsTable = "drug_exposure",
                          byConcept = TRUE,
                          sampleSize = 10000) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(cdm = cdm, tableName = drugRecordsTable, messageStore = errorMessage)
  checkLogical(byConcept, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (isTRUE(byConcept)) {
    grouping <- c("drug_concept_id", "drug",
                  "ingredient_concept_id",
                  "ingredient", "route_concept_id")
  } else {
    grouping <- c("ingredient_concept_id", "ingredient",
                  "route_concept_id")
  }

  summ <- cdm[[drugRecordsTable]] %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(n_records = as.integer(dplyr::n()),
                     n_sample = .env$sampleSize) %>%
    CDMConnector::computeQuery() %>%
    dplyr::left_join(cdm$concept %>%
                       dplyr::rename("route_concept_id" = "concept_id",
                                     "route_type" = "concept_name") %>%
                       dplyr::select("route_concept_id", "route_type"),
                     by = "route_concept_id") %>%
    dplyr::select(tidyselect::any_of(
      c("drug_concept_id", "drug",
        "ingredient_concept_id", "ingredient",
        "route_concept_id",
        "route_type", "n_records")
    ))

  return(summ)
}

#' Check drug exposure source types
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable modified drug exposure table
#' @param byConcept by individual drug Concept
#' @param sampleSize the sample size given in execute checks
#'
#' @return a table with the drug source concepts
getDrugSourceConcepts <- function(cdm,
                                  drugRecordsTable = "drug_exposure",
                                  byConcept = TRUE,
                                  sampleSize = 10000) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(cdm = cdm, tableName = drugRecordsTable, messageStore = errorMessage)
  checkLogical(byConcept, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (isTRUE(byConcept)) {
    grouping <- c("drug_concept_id", "drug",
                  "ingredient_concept_id",
                  "ingredient", "drug_source_concept_id")
  } else {
    grouping <- c("ingredient_concept_id", "ingredient",
                  "drug_source_concept_id")
  }

summ <- cdm[[drugRecordsTable]] %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(n_records = as.integer(dplyr::n()),
                     n_sample = .env$sampleSize) %>%
    CDMConnector::computeQuery() %>%
    dplyr::left_join(cdm$concept %>%
                       dplyr::rename("drug_source_concept_id" = "concept_id",
                                     "drug_source" = "concept_name") %>%
                       dplyr::select("drug_source_concept_id", "drug_source"),
                     by = "drug_source_concept_id") %>%
    dplyr::select(tidyselect::any_of(
      c("drug_concept_id", "drug",
        "ingredient_concept_id", "ingredient",
        "drug_source_concept_id",
        "drug_source", "n_records")
    ))

  return(summ)
}

#' Summarise drug exposure record durations
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable modified drug exposure table
#' @param byConcept by individual drug Concept
#' @param sampleSize the sample size given in execute checks
#'
#' @return a table with the drug exposure record durations
summariseDrugExposureDuration <- function(cdm,
                                          drugRecordsTable = "ingredient_drug_records",
                                          byConcept = TRUE,
                                          sampleSize = 10000) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(
    cdm = cdm, tableName = drugRecordsTable,
    messageStore = errorMessage
  )
  checkLogical(byConcept, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (isTRUE(byConcept)) {
    grouping <- c("drug_concept_id", "drug",
                  "ingredient_concept_id",
                  "ingredient")
  } else {
    grouping <- c("ingredient_concept_id",  "ingredient")
  }

  records <- cdm[[drugRecordsTable]]

  recordDays <- records %>%
    dplyr::select(
      "drug_concept_id",
      "drug",
      "ingredient_concept_id",
      "ingredient",
      "drug_exposure_start_date",
      "drug_exposure_end_date"
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      drug_exposure_days =
        as.numeric(difftime(.data$drug_exposure_end_date,
                            .data$drug_exposure_start_date,
                            units = "days"
        )) + 1
    )

  summ <- recordDays %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(
      n_records = as.integer(dplyr::n()),
      n_sample = .env$sampleSize,
      n_non_negative_days = sum(.data$drug_exposure_days >= 0, na.rm = T),
      n_negative_days = sum(.data$drug_exposure_days < 0, na.rm = T),
      proportion_negative_days = sum(.data$drug_exposure_days < 0, na.rm = T) / dplyr::n(),
      minimum_drug_exposure_days = min(.data$drug_exposure_days, na.rm = T),
      q05_drug_exposure_days = stats::quantile(
        .data$drug_exposure_days,
        0.05, na.rm = T
      ),
      q10_drug_exposure_days = stats::quantile(
        .data$drug_exposure_days,
        0.10, na.rm = T
      ),
      q25_drug_exposure_days = stats::quantile(
        .data$drug_exposure_days,
        0.25, na.rm = T
      ),
      median_drug_exposure_days = stats::median(.data$drug_exposure_days, na.rm = T),
      q75_drug_exposure_days = stats::quantile(
        .data$drug_exposure_days,
        0.75, na.rm = T
      ),
      q90_drug_exposure_days = stats::quantile(
        .data$drug_exposure_days,
        0.90, na.rm = T
      ),
      q95_drug_exposure_days = stats::quantile(
        .data$drug_exposure_days,
        0.95, na.rm = T
      ),
      maximum_drug_exposure_days = max(.data$drug_exposure_days, na.rm = T)
    )

  return(summ)
}
