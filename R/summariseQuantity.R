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


#' Summarise the quantity column of the drug_exposure table
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable modified drug exposure table
#' @param byConcept whether to get result by drug concept
#' @param sampleSize the sample size given in execute checks
#'
#' @return a table with the summarized quantity result
summariseQuantity <- function(cdm,
                              drugRecordsTable = "ingredient_drug_records",
                              byConcept = TRUE,
                              sampleSize = sampleSize){
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
                  "ingredient_concept_id", "ingredient")
  } else {
    grouping <- c("ingredient_concept_id", "ingredient")
  }

  records <- cdm[[drugRecordsTable]]

  recordQuantity <- records %>%
    dplyr::select(
      "drug_concept_id",
      "drug",
      "ingredient_concept_id",
      "ingredient",
      "quantity"
    ) %>%
    dplyr::collect()


  summ <- recordQuantity %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(
      n_records = as.integer(dplyr::n()),
      n_sample = .env$sampleSize,
      minimum_drug_exposure_quantity =  min(.data$quantity, na.rm = T),
      q05_drug_exposure_quantity = stats::quantile(
        .data$quantity,
        0.05, na.rm = T
      ),
      q10_drug_exposure_quantity = stats::quantile(
        .data$quantity,
        0.10, na.rm = T
      ),
      q25_drug_exposure_quantity = stats::quantile(
        .data$quantity,
        0.25, na.rm = T
      ),
      median_drug_exposure_quantity = stats::median(.data$quantity, na.rm = T),
      q75_drug_exposure_quantity = stats::quantile(
        .data$quantity,
        0.75, na.rm = T
      ),
      q90_drug_exposure_quantity = stats::quantile(
        .data$quantity,
        0.90, na.rm = T
      ),
      q95_drug_exposure_quantity = stats::quantile(
        .data$quantity,
        0.95, na.rm = T
      ),
      maximum_drug_exposure_quantity = max(.data$quantity, na.rm = T)
    )

  return(summ)
}
