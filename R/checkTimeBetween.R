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

#' Check time in between drug records per person and report the summary
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable a modified version of the drug exposure table, default "ingredient_drug_records"
#' @param byConcept whether to get result by drug concept
#' @param sampleSize the sample size given in execute checks
#'
#' @return a table with the stats about the time between
summariseTimeBetween <- function(cdm,
                                 drugRecordsTable = "ingredient_drug_records",
                                 byConcept = TRUE,
                                 sampleSize = 10000) {
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
      "ingredient_concept_id",
      "ingredient"
    )
  } else {
    grouping <- c("ingredient_concept_id", "ingredient")
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
    )

  summ <- recordDays %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(grouping, "person_id")))) %>%
    dplyr::arrange(.data$person_id, .data$drug_exposure_start_date) %>%
    dplyr::mutate(prev_drug_exposure_start_date = dplyr::lag(.data$drug_exposure_start_date)) %>%
    dplyr::mutate(time_between_days = !!CDMConnector::datediff(start = "prev_drug_exposure_start_date",
                                                               end = "drug_exposure_start_date",
                                                               interval = "day")) %>%
    dplyr::select(-.data$prev_drug_exposure_start_date) %>%
    dplyr::filter(!is.na(.data$time_between_days)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(
      n_records = as.integer(dplyr::n()),
      n_sample = .env$sampleSize,
      n_person = dplyr::n_distinct(.data$person_id),
      minimum_time_between_days = min(.data$time_between_days, na.rm = T),
      q05_time_between_days = stats::quantile(
        .data$time_between_days,
        0.05,
        na.rm = T
      ),
      q10_time_between_days = stats::quantile(
        .data$time_between_days,
        0.10,
        na.rm = T
      ),
      q25_time_between_days = stats::quantile(
        .data$time_between_days,
        0.25,
        na.rm = T
      ),
      median_time_between_days = stats::median(.data$time_between_days, na.rm = T),
      q75_time_between_days = stats::quantile(
        .data$time_between_days,
        0.75,
        na.rm = T
      ),
      q90_time_between_days = stats::quantile(
        .data$time_between_days,
        0.90,
        na.rm = T
      ),
      q95_time_between_days = stats::quantile(
        .data$time_between_days,
        0.95,
        na.rm = T
      ),
      maximum_time_between_days = max(.data$time_between_days, na.rm = T)
    ) %>%
    dplyr::collect()

  return(summ)
}
