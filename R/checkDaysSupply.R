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


#' Check if Days_supply is the same as datediff(drug_exp_start_date,drug_exp_end_date)
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable drug exposure table
#' @param byConcept whether to get result by concept
#'
#' @return a table with the stats of days supply compared to start and end date
checkDaysSupply <- function(cdm,
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
      "drug_concept_id",
      "drug",
      "ingredient_concept_id",
      "ingredient",
      "drug_exposure_start_date",
      "drug_exposure_end_date",
      "days_supply")

  records <- records %>% dplyr::mutate(
    n = dplyr::if_else(days_supply == as.integer(
      !!CDMConnector::datediff(start = "drug_exposure_start_date",
                               end = "drug_exposure_end_date",
                               interval = "day")), 0, 1)) %>%
    dplyr::collect()

  if (isTRUE(byConcept)) {
    grouping <- c("drug_concept_id","drug",
                  "ingredient_concept_id", "ingredient")
  } else {
    grouping <- c("ingredient_concept_id", "ingredient")
  }

  records <- records %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) %>%
    dplyr::summarise(n_records = dplyr::n(),
                     n_different_days_supply_and_drug_dates  = sum(.data$n, na.rm = T),
                     n_days_supply_match_drug_dates = sum(.data$n == 0, na.rm = T),
                     n_missing_days_supply_or_drug_dates = sum(is.na(.data$n)),
                     proportion_different_days_supply_and_drug_dates = .data$n_different_days_supply_and_drug_dates / dplyr::n(),
                     proportion_days_supply_match_drug_dates = .data$n_days_supply_match_drug_dates / dplyr::n(),
                     proportion_missing_days_supply_or_drug_dates = .data$n_missing_days_supply_or_drug_dates / dplyr::n())

  return(records)

}

#' Create a summary of the days_supply field
#'
#' @param cdm CDMConnector reference object
#' @param drugRecordsTable table name containing the drug exposure records
#'
#' @return a table with the days supply summary
summariseDaysSupply <- function (cdm, drugRecordsTable = "drug_exposure"){
    errorMessage <- checkmate::makeAssertCollection()
    checkDbType(cdm = cdm, messageStore = errorMessage)
    checkTableExists(
      cdm = cdm, tableName = drugRecordsTable,
      messageStore = errorMessage
    )
    checkmate::reportAssertions(collection = errorMessage)

    records <- cdm[[drugRecordsTable]]

    daysSupply <- records %>%
      dplyr::select(
        "person_id",
        "drug_concept_id",
        "days_supply"
      ) %>%
      dplyr::collect()


    summ <- daysSupply %>%
      dplyr::summarise(
        n_records = dplyr::n(),
        minimum_drug_exposure_days_supply =  min(.data$days_supply, na.rm = T),
        maximum_drug_exposure_days_supply = max(.data$days_supply, na.rm = T),
        n_records_with_days_supply = sum(!is.na(.data$days_supply)),
        n_people = length(unique(.data$person_id)),
        q05_drug_exposure_days_supply = stats::quantile(
          .data$days_supply,
          0.05, na.rm = T
        ),
        q10_drug_exposure_days_supply = stats::quantile(
          .data$days_supply,
          0.10, na.rm = T
        ),
        q25_drug_exposure_days_supply = stats::quantile(
          .data$days_supply,
          0.25, na.rm = T
        ),
        median_drug_exposure_days_supply = stats::median(.data$days_supply, na.rm = T),
        q75_drug_exposure_days_supply = stats::quantile(
          .data$days_supply,
          0.75, na.rm = T
        ),
        q90_drug_exposure_days_supply = stats::quantile(
          .data$days_supply,
          0.90, na.rm = T
        ),
        q95_drug_exposure_days_supply = stats::quantile(
          .data$days_supply,
          0.95, na.rm = T
        ),
      )

    return(summ)
}
