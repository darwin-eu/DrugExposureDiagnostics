# Copyright 2022 DARWIN EU®
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

#' Get a detailed ingredient overview. The record count and patient count
#' will be returned for an unique combination of data elements.
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable drug exposure table
#' @param drugStrengthTable drug strength table
#'
#' @return a table with the stats
getIngredientOverview <- function(cdm,
                                  drugRecordsTable = "drug_exposure",
                                  drugStrengthTable = "drug_strength") {

  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(cdm = cdm, tableName = drugRecordsTable, messageStore = errorMessage)
  checkTableExists(cdm = cdm, tableName = drugStrengthTable, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  records <- getDuration(cdm, drugRecordsTable, colName = "drug_exposure_days") %>%
    dplyr::left_join(
      cdm[[drugStrengthTable]] %>%
        dplyr::select("drug_concept_id", "amount_value", "amount_unit",
                      "numerator_value", "numerator_unit", "denominator_unit") %>%
        dplyr::distinct(),
      by = c("drug_concept_id")) %>%
    dplyr::mutate(strength = dplyr::if_else(.data$denominator_unit == "",
                                            paste(as.character(.data$amount_value),
                                                  as.character(.data$amount_unit)),
                                            paste(as.character(.data$numerator_value),
                                                  paste(as.character(.data$numerator_unit),
                                                        as.character(.data$denominator_unit), sep = "/"))))
  # get stats
  grouping <- c("ingredient_concept_id", "ingredient", "drug_exposure_days", "days_supply", "quantity", "sig", "strength" )
  return(records %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(
      n_records = dplyr::n(),
      n_people = dplyr::count(dplyr::distinct(.data$person_id)),
      .groups = "drop"))
}

#' Get a presence overview for the ingredient. The record count and patient count
#' will be returned for a bit set.
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable drug exposure table
#' @param drugStrengthTable drug strength table
#'
#' @return a table with the bit set
getIngredientPresence <- function(cdm,
                                  drugRecordsTable = "drug_exposure",
                                  drugStrengthTable = "drug_strength") {

  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(cdm = cdm, tableName = drugRecordsTable, messageStore = errorMessage)
  checkTableExists(cdm = cdm, tableName = drugStrengthTable, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  records <- cdm[[drugRecordsTable]] %>%
    dplyr::left_join(
      cdm[[drugStrengthTable]] %>%
        dplyr::select("drug_concept_id", "amount_value", "numerator_value", "denominator_unit") %>%
        dplyr::distinct(),
      by = c("drug_concept_id")) %>%
    dplyr::mutate(
      drug_exposure_days = !!CDMConnector::datediff(start = "drug_exposure_start_date",
                                               end = "drug_exposure_end_date",
                                               interval = "day") + 1) %>%
    dplyr::mutate(strength = dplyr::if_else(.data$denominator_unit == "",
                                            .data$amount_value,
                                            .data$numerator_value)) %>%
    dplyr::mutate(days_supply_specified = dplyr::if_else(is.na(.data$days_supply) | .data$days_supply == 0, "No", "Yes")) %>%
    dplyr::mutate(quantity_specified = dplyr::if_else(is.na(.data$quantity) | .data$quantity == 0, "No", "Yes")) %>%
    dplyr::mutate(drug_exposure_end_date_specified = dplyr::if_else(is.na(.data$drug_exposure_end_date), "No", "Yes")) %>%
    dplyr::mutate(strength_specified = dplyr::if_else(is.na(.data$strength) | .data$strength <= 0, "No", "Yes")) %>%
    dplyr::mutate(sig_specified = dplyr::if_else(is.na(.data$sig) | .data$sig == "", "No", "Yes")) %>%
    dplyr::mutate(drug_exposure_days_specified = dplyr::if_else(.data$drug_exposure_days == 0, "No", "Yes"))

  # get stats
  grouping <- c("ingredient_concept_id", "ingredient", "days_supply_specified", "quantity_specified", "drug_exposure_end_date_specified",
                "strength_specified", "sig_specified", "drug_exposure_days_specified")
  records %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(
      n_records = dplyr::n(),
      n_people = dplyr::count(dplyr::distinct(.data$person_id)),
      .groups = "drop")
}
