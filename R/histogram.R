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

#' create a histogram for one of days_supply, duration, quantity
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable drug exposure table
#' @param type specify whether to plot days_supply, duration or quantity
#'
#' @return object containing a histogram
createHistogram <- function(cdm,
                            drugRecordsTable = "drug_exposure",
                            type) {
  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkTableExists(cdm = cdm, tableName = drugRecordsTable, messageStore = errorMessage)
  drugExposureColnames <- colnames(cdm[[drugRecordsTable]])
  checkmate::assertTRUE(all(c("person_id", "quantity", "drug_concept_id", "ingredient_concept_id",
                              "ingredient", "drug_exposure_start_date", "drug_exposure_end_date",
                              "days_supply")
                            %in% drugExposureColnames), add = errorMessage)
  checkmate::assertFALSE(c("duration") %in% drugExposureColnames, add = errorMessage)
  checkmate::assertTRUE(type %in% c("days_supply", "duration", "quantity"))
  checkmate::reportAssertions(collection = errorMessage)

  if (type == "duration"){
    cdm[[drugRecordsTable]] <- getDuration(cdm, drugRecordsTable)
  }
  histData <- cdm[[drugRecordsTable]] %>%
    dplyr::select(dplyr::all_of(c(type, "ingredient_concept_id", "ingredient"))) %>%
    dplyr::collect()

  histogram <- NULL
  if (nrow(histData) > 0) {
    tryCatch({
      histogram <- hist(histData[[type]],
                        plot = FALSE)
      histogram$xname <- type
      histogram$ingredient_concept_id <- histData %>%
        dplyr::pull("ingredient_concept_id") %>%
        unique()
      histogram$ingredient <- histData %>%
        dplyr::pull("ingredient") %>%
        unique()
    },
    error = function(message) {
      warning(glue::glue("histogram could not be created for {type}"))
    })
  }
  return(histogram)
}
