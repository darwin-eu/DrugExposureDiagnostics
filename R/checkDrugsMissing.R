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
#' @param drugRecordsTable a modified version of the drug exposure table, default "ingredient_drug_records"
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
    grouping <- c(
      "drug_concept_id", "drug",
      "ingredient_concept_id", "ingredient"
    )
  } else {
    grouping <- c("ingredient_concept_id", "ingredient")
  }

  records <- cdm[[drugRecordsTable]]

  # summarise missings
  summMissings <- records %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(
      n_records = as.integer(dplyr::n()),
      n_sample = .env$sampleSize,
      n_person = dplyr::n_distinct(.data$person_id),
      n_missing_drug_exposure_id =
        sum(dplyr::case_when(
          .data$drug_exposure_id == 0 ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_person_id =
        sum(dplyr::case_when(
          is.na(.data$person_id) |
            .data$person_id == 0 ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_drug_concept_id =
        sum(dplyr::case_when(
          is.na(.data$drug_concept_id) |
            .data$drug_concept_id == 0 ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_drug_exposure_start_date =
        sum(dplyr::case_when(
          is.na(.data$drug_exposure_start_date) ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_drug_exposure_end_date =
        sum(dplyr::case_when(
          is.na(.data$drug_exposure_end_date) ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_verbatim_end_date =
        sum(dplyr::case_when(
          is.na(.data$verbatim_end_date) ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_drug_type_concept_id =
        sum(dplyr::case_when(
          is.na(.data$drug_type_concept_id) |
            .data$drug_type_concept_id == 0 ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_quantity =
        sum(dplyr::case_when(
          is.na(.data$quantity) ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_days_supply =
        sum(dplyr::case_when(
          is.na(.data$days_supply) ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_sig =
        sum(dplyr::case_when(
          is.na(.data$sig) ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_route_concept_id =
        sum(dplyr::case_when(
          is.na(.data$route_concept_id) |
            .data$route_concept_id == 0 ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_drug_source_value =
        sum(dplyr::case_when(
          is.na(.data$drug_source_value) ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_drug_source_concept_id =
        sum(dplyr::case_when(
          is.na(.data$drug_source_concept_id) ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_route_source_value =
        sum(dplyr::case_when(
          is.na(.data$route_source_value) ~ 1, TRUE ~ 0
        ), na.rm = T),
      n_missing_dose_unit_source_value =
        sum(dplyr::case_when(
          is.na(.data$dose_unit_source_value) ~ 1, TRUE ~ 0
        ), na.rm = T)
    ) %>%
    dplyr::collect() %>%
    tidyr::pivot_longer(
      !tidyselect::any_of(c(
        "drug_concept_id", "drug",
        "ingredient_concept_id",
        "ingredient", "n_records",
        "n_sample", "n_person"
      )),
      names_to = "variable",
      values_to = "n_records_missing_value"
    )

  # add prop
  summMissings <- summMissings %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::relocate("n_records", .after = "variable") %>%
    dplyr::relocate("n_sample", .after = "n_records") %>%
    dplyr::relocate("n_person", .after = "n_sample") %>%
    dplyr::mutate(n_records_not_missing_value = .data$n_records - .data$n_records_missing_value) %>%
    dplyr::mutate(proportion_records_missing_value = .data$n_records_missing_value / .data$n_records) %>%
    dplyr::relocate("n_records_missing_value", .after = "n_records_not_missing_value")

  summMissings$total_by_concept <- NULL

  return(summMissings)
}
