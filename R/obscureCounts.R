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


#' Obscure the small number of counts
#'
#' @param table the table as a tibble
#' @param tableName the table name
#' @param minCellCount the minimum number of counts that will be displayed. If 0 all results will be reported.
#' @param substitute the substitute value if values will be obscured
#'
#' @return the input table with results obscured if minCellCount applies
obscureCounts <- function(table,
                          tableName,
                          minCellCount = 5,
                          substitute = NA) {
  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(table, add = errorMessage)
  checkmate::assertTRUE(is.numeric(minCellCount), add = errorMessage)
  checkmate::assertTRUE(is.numeric(substitute) || is.na(substitute), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (minCellCount > 0) {
    # initialise result_obscured as FALSE
    table$result_obscured <- FALSE

    colNames <- setdiff(colnames(table), c(
      "drug_concept_id", "drug",
      "ingredient_concept_id", "ingredient", "result_obscured"
    ))
    checkColNames <- NULL
    if (grepl("drugRoutes|drugSig|drugVerbatimEndDate|drugQuantity|drugSourceConcepts|drugTypes|drugExposureDuration|drugDaysSupply", tableName)) {
      checkColNames <- c("n_records", "n_person")
    } else if (grepl("missingValues", tableName)) {
      checkColNames <- c("n_records", "n_person", "n_records_not_missing_value", "n_records_missing_value")
    } else if (grepl("diagnosticsSummary|conceptSummary", tableName)) {
      checkColNames <- colNames <- c("n_records", "n_patients")
    }

    # if any count col is less than minCellCount and larger than zero, replace colNames with substitute
    if (!is.null(checkColNames) && nrow(table) > 0) {
      table <- table %>%
        dplyr::rowwise() %>%
        dplyr::mutate(result_obscured = any(dplyr::across(dplyr::all_of(checkColNames), ~ (. < minCellCount & . > 0)))) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::all_of(colNames)), ~ ifelse(result_obscured, substitute, .))
    }
  }

  return(table)
}
