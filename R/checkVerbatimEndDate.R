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

#' Check the verbatim_end_date field
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable drug exposure table
#' @param byConcept whether to get result by concept
#'
#' @return a table with the stats about the verbatim_end_date
checkVerbatimEndDate <- function(cdm,
                                 drugRecordsTable = "drug_exposure",
                                 byConcept = TRUE) {

  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(cdm = cdm, tableName = drugRecordsTable,
                   messageStore = errorMessage)
  checkLogical(byConcept, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (isTRUE(byConcept)) {
    grouping <- c("drug_concept_id", "drug",
                  "ingredient_concept_id",
                  "ingredient")
  } else {
    grouping <- c("ingredient_concept_id", "ingredient")
  }

  records <- cdm[[drugRecordsTable]]

  # get stats
  records %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(
      minimum_verbatim_end_date = min(.data$verbatim_end_date, na.rm = T),
      maximum_verbatim_end_date = max(.data$verbatim_end_date, na.rm = T),
      n_records = as.integer(dplyr::n()),
      n_missing_verbatim_end_date = sum(dplyr::case_when(is.na(.data$verbatim_end_date) ~ 1, TRUE ~ 0), na.rm = T),
      n_not_missing_verbatim_end_date = as.integer(dplyr::n() - sum(dplyr::case_when(is.na(.data$verbatim_end_date) ~ 1, TRUE ~ 0), na.rm = T)),
      n_verbatim_end_date_equal_to_drug_exposure_end_date = as.integer(dplyr::n() -
        sum(dplyr::case_when(.data$drug_exposure_end_date != .data$verbatim_end_date ~ 1, TRUE ~ 0), na.rm = T)),
      n_verbatim_end_date_and_drug_exposure_end_date_differ =
        sum(dplyr::case_when(.data$drug_exposure_end_date != .data$verbatim_end_date ~ 1, TRUE ~ 0), na.rm = T))
}
