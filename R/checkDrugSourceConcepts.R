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

#' Check drug exposure source types
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable modified drug exposure table
#' @param sampleSize the sample size given in execute checks
#'
#' @return a table with the drug source concepts
getDrugSourceConcepts <- function(cdm,
                                  drugRecordsTable = "ingredient_drug_records",
                                  sampleSize = 10000) {
  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(cdm = cdm, tableName = drugRecordsTable, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  grouping <- c(
    "drug_concept_id", "drug",
    "ingredient_concept_id", "ingredient",
    "drug_source_concept_id", "drug_source_value"
  )

  total <- cdm[[drugRecordsTable]] %>%
    dplyr::summarise(total = dplyr::n()) %>%
    dplyr::pull()

  summ <- cdm[[drugRecordsTable]] %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(
      n_records = as.integer(dplyr::n()),
      n_sample = .env$sampleSize,
      n_person = dplyr::n_distinct(.data$person_id)
    ) %>%
    dplyr::compute() %>%
    dplyr::mutate(proportion_records = .data$n_records / .env$total) %>%
    dplyr::left_join(
      cdm$concept %>%
        dplyr::rename(
          "drug_source_concept_id" = "concept_id",
          "drug_source" = "concept_name"
        ) %>%
        dplyr::select("drug_source_concept_id", "drug_source"),
      by = "drug_source_concept_id"
    ) %>%
    dplyr::select(tidyselect::any_of(
      c(
        "ingredient_concept_id", "ingredient", "drug_concept_id", "drug",
        "drug_source_concept_id", "drug_source",
        "drug_source_value", "n_records", "n_sample", "n_person",
        "proportion_records"
      )
    ))

  return(summ)
}
