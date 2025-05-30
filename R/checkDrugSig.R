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

#' Check the drug sig field; this is the verbatim instruction for the drug as
#' written by the provider.
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable a modified version of the drug exposure table, default "ingredient_drug_records"
#' @param byConcept whether to get result by drug concept
#' @param sampleSize the sample size given in execute checks
#'
#' @return a table with a summary of the sig values
checkDrugSig <- function(cdm,
                         drugRecordsTable = "ingredient_drug_records",
                         byConcept = TRUE,
                         sampleSize = 10000) {
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(
    cdm = cdm, tableName = drugRecordsTable,
    messageStore = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  if (isTRUE(byConcept)) {
    grouping <- c(
      "drug_concept_id", "drug",
      "ingredient_concept_id",
      "ingredient", "sig"
    )
  } else {
    grouping <- c(
      "ingredient_concept_id",
      "ingredient", "sig"
    )
  }

  total <- cdm[[drugRecordsTable]] %>%
    dplyr::summarise(total = dplyr::n()) %>%
    dplyr::pull()

  records <- cdm[[drugRecordsTable]] %>%
    dplyr::select(
      "drug_concept_id",
      "drug",
      "ingredient_concept_id",
      "ingredient",
      "sig",
      "person_id"
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(
      n_records = as.integer(dplyr::n()),
      n_sample = .env$sampleSize,
      n_person = dplyr::n_distinct(.data$person_id)
    ) %>%
    dplyr::compute() %>%
    dplyr::mutate(proportion_records = .data$n_records / .env$total) %>%
    dplyr::select(tidyselect::any_of(
      c(
        "drug_concept_id", "drug",
        "ingredient_concept_id",
        "ingredient", "sig",
        "n_records", "n_sample", "n_person", "proportion_records"
      )
    ))

  return(records)
}
