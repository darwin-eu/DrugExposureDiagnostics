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

#' Get a summary of the daily drug dose
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable modified drug exposure table
#' @param drugStrengthTable drug strength table
#' @param byConcept whether to get result by drug concept
#' @param sampleSize the sample size given in execute checks
#'
#' @return a table with the stats about the daily dose
checkDrugDose <- function(cdm,
                             drugRecordsTable = "ingredient_drug_records",
                             drugStrengthTable = "drug_strength",
                             byConcept = TRUE,
                             sampleSize = sampleSize) {

  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(cdm = cdm, tableName = drugRecordsTable,
                   messageStore = errorMessage)
  checkTableExists(cdm = cdm, tableName = drugStrengthTable,
                   messageStore = errorMessage)
  checkLogical(byConcept, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  records <- cdm[[drugRecordsTable]] %>%
    dplyr::select(
      "drug_concept_id",
      "drug",
      "ingredient_concept_id",
      "ingredient",
      "drug_exposure_start_date",
      "drug_exposure_end_date",
      "days_supply",
      "quantity") %>%
        dplyr::left_join((dplyr::select(
          cdm[[drugStrengthTable]],
          "drug_concept_id",
          "denominator_unit_concept_id",
          "amount_value")), by = "drug_concept_id") %>%
        dplyr::collect()

  if (isTRUE(byConcept)) {
    grouping <- c("drug_concept_id", "drug", "ingredient_concept_id","ingredient")
  } else {
    grouping <- c("ingredient_concept_id","ingredient")
  }

  records <- records %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(n_records = as.integer(dplyr::n()),
                     n_sample = .env$sampleSize,
                     missing_days_supply_or_dates = sum(.data$days_supply == 0 | is.na(.data$drug_exposure_start_date) | is.na(.data$drug_exposure_end_date)),
                     proportion_of_records_missing_days_supply_or_dates = .data$missing_days_supply_or_dates / dplyr::n(),
                     missing_or_null_quantity = sum(.data$quantity == 0 | is.na(.data$quantity)),
                     proportion_missing_or_null_quantity  = .data$missing_or_null_quantity / dplyr::n(),
                     missing_denominator_unit_concept_id = sum(is.na(.data$denominator_unit_concept_id)),
                     proportion_of_records_missing_denominator_unit_concept_id = .data$missing_denominator_unit_concept_id / dplyr::n(),
                     missing_or_null_amount_value = sum(.data$amount_value == 0 | is.na(.data$amount_value)),
                     proportion_of_records_missing_or_null_amount_value  = .data$missing_or_null_amount_value / dplyr::n(),
                     q05_quantity = stats::quantile(.data$quantity, 0.05, na.rm = T),
                     q10_quantity = stats::quantile(.data$quantity, 0.10, na.rm = T),
                     q15_quantity = stats::quantile(.data$quantity, 0.15, na.rm = T),
                     q20_quantity = stats::quantile(.data$quantity, 0.20, na.rm = T),
                     q25_quantity = stats::quantile(.data$quantity, 0.25, na.rm = T),
                     median_quantity = stats::median(.data$quantity, na.rm = T),
                     q75_quantity = stats::quantile(.data$quantity, 0.75, na.rm = T),
                     q80_quantity = stats::quantile(.data$quantity, 0.80, na.rm = T),
                     q85_quantity = stats::quantile(.data$quantity, 0.85, na.rm = T),
                     q90_quantity = stats::quantile(.data$quantity, 0.90, na.rm = T),
                     q95_quantity = stats::quantile(.data$quantity, 0.95, na.rm = T),
                     q05_amount_value = stats::quantile(.data$amount_value, 0.05, na.rm = T),
                     q10_amount_value = stats::quantile(.data$amount_value, 0.10, na.rm = T),
                     q15_amount_value = stats::quantile(.data$amount_value, 0.15, na.rm = T),
                     q20_amount_value = stats::quantile(.data$amount_value, 0.20, na.rm = T),
                     q25_amount_value = stats::quantile(.data$amount_value, 0.25, na.rm = T),
                     median_amount_value = stats::median(.data$amount_value, na.rm = T),
                     q75_amount_value = stats::quantile(.data$amount_value, 0.75, na.rm = T),
                     q80_amount_value = stats::quantile(.data$amount_value, 0.80, na.rm = T),
                     q85_amount_value = stats::quantile(.data$amount_value, 0.85, na.rm = T),
                     q90_amount_value = stats::quantile(.data$amount_value, 0.90, na.rm = T),
                     q95_amount_value = stats::quantile(.data$amount_value, 0.95, na.rm = T)
                     )

  return(records)
}
