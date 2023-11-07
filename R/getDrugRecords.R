# Copyright 2022 DARWIN EU®
#
# This file is part of IncidencePrevalence
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

#' Drug exposure records for ingredients of interest
#'
#' @param cdm CDMConnector reference object
#' @param ingredient Concept ID for ingredient of interest
#' @param includedConceptsTable includedConceptsTable
#' @param drugRecordsTable drugRecordsTable, default "drug_exposure"
#' @param tablePrefix The stem for the permanent tables that will
#' be created when running the diagnostics. Permanent tables will be created using
#' this prefix, and any existing tables that start with this will be at risk of
#' being dropped or overwritten. If NULL, temporary tables will be
#' used throughout.
#' @param verbose verbose
#'
#' @return a table containing drug exposure records
getDrugRecords <- function(cdm,
                           ingredient,
                           includedConceptsTable,
                           drugRecordsTable = "drug_exposure",
                           tablePrefix = NULL,
                           verbose = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertNumeric(ingredient, add = errorMessage)
  checkTableExists(
    cdm = cdm, tableName = includedConceptsTable,
    messageStore = errorMessage
  )
  checkTableExists(
    cdm = cdm, tableName = drugRecordsTable,
    messageStore = errorMessage
  )
  checkLogical(verbose, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  records <- cdm[[drugRecordsTable]] %>%
    dplyr::inner_join(cdm[[includedConceptsTable]] %>%
      dplyr::select("concept_id","concept_name", "ingredient_concept_id", "ingredient"),
    by = c("drug_concept_id" = "concept_id")) %>%
    dplyr::filter(.data$ingredient_concept_id == .env$ingredient) %>%
    dplyr::rename("drug" = "concept_name")

  # store result
  records <- computeDBQuery(table = records,
                            tablePrefix = tablePrefix,
                            tableName = "_DED_drug_records",
                            cdm = cdm)
  return(records)
}
