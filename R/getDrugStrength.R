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

#' Drug strength records for ingredients of interest
#'
#' @param cdm CDMConnector reference object
#' @param ingredient ingredient concept ID for ingredient of interest
#' @param includedConceptsTable table name for the concept ids, names and units
#' @param drugStrengthTable table name for drug strength, default "drug_strength"
#' @param tablePrefix The stem for the permanent tables that will
#' be created when running the diagnostics. Permanent tables will be created using
#' this prefix, and any existing tables that start with this will be at risk of
#' being dropped or overwritten. If NULL, temporary tables will be
#' used throughout.
#' @param verbose verbose
#'
#' @return a table containing drug strength records
getDrugStrength <- function(cdm,
                            ingredient,
                            includedConceptsTable = "ingredient_concepts",
                            drugStrengthTable = "drug_strength",
                            tablePrefix = NULL,
                            verbose = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(
    cdm = cdm, tableName = includedConceptsTable,
    messageStore = errorMessage
  )
  checkTableExists(
    cdm = cdm, tableName = drugStrengthTable,
    messageStore = errorMessage
  )
  checkLogical(verbose, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  records <- cdm[[drugStrengthTable]] %>%
    dplyr::filter(.data$ingredient_concept_id == .env$ingredient) %>%
    dplyr::inner_join(cdm[[includedConceptsTable]] %>%
                        dplyr::select("concept_id", "numerator_unit",
                                      "denominator_unit", "amount_unit"),
                      by = c("drug_concept_id" = "concept_id")
    )
  # store result
  records <- computeDBQuery(table = records,
                            tablePrefix = tablePrefix,
                            tableName = "_DED_drug_strength",
                            cdm = cdm)
  return(records)
}
