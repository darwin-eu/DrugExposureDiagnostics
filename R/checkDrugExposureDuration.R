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

#' Summarise drug exposure record durations
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable a modified version of the drug exposure table, default "ingredient_drug_records"
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
      "drug_exposure_end_date",
      "person_id"
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
      n_person = dplyr::n_distinct(.data$person_id),
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
