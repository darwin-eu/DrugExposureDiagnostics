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

#' Check the drug sig field; this is the verbatim instruction for the drug as
#' written by the provider.
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable drug exposure table
#' @param byConcept whether to get result by drug concept
#'
#' @return a table with a summary of the sig values
checkDrugSig <- function(cdm,
                         drugRecordsTable = "drug_exposure",
                         byConcept = TRUE)
{
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(
    cdm = cdm, tableName = drugRecordsTable,
    messageStore = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  records <- cdm[[drugRecordsTable]] %>%
    dplyr::select(
      "drug_concept_id", "drug",
      "ingredient_concept_id",
      "ingredient",
      "sig")

  if (isTRUE(byConcept)) {
    grouping <- c("drug_concept_id", "drug",
                  "ingredient_concept_id",
                  "ingredient", "sig")
  } else {
    grouping <- c("ingredient_concept_id",
                  "ingredient", "sig")
  }

  records <- records %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::tally(name = "n_records") %>%
    dplyr::select(tidyselect::any_of(
      c("drug_concept_id", "drug",
        "ingredient_concept_id",
        "ingredient", "sig", "n_records")))

  return(records)
}
