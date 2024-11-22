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

#' Get a summary of the daily drug dose
#'
#' @param cdm CDMConnector reference object
#' @param ingredientConceptId ingredient
#' @param sampleSize Maximum number of records of an ingredient to estimate dose
#'  coverage. If an ingredient has more, a random sample equal to `sampleSize`
#'  will be considered. If NULL, all records will be used.
#' @param minCellCount minimum number of events to report- results
#' lower than this will be obscured. If NULL all results will be reported.
#'
#' @return a table with the stats about the daily dose
checkDrugDose <- function(cdm,
                          ingredientConceptId,
                          sampleSize = NULL,
                          minCellCount = 5) {
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(is.numeric(minCellCount), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  patterns <- utils::read.csv(system.file("pattern_assessment_for_dose_final.csv",
    package = "DrugUtilisation"
  ))

  records <- DrugUtilisation::summariseDoseCoverage(
    cdm = cdm,
    ingredientConceptId = ingredientConceptId,
    estimates = c(
      "count_missing", "percentage_missing", "mean", "sd",
      "q05", "q25", "median", "q75", "q95", "min", "max"
    )
  ) %>%
    dplyr::filter(.data$group_level != "NA") %>%
    omopgenerics::suppress(minCellCount) %>%
    dplyr::mutate(pattern_id = as.numeric(gsub("[^0-9]", "", .data$strata_level))) %>%
    dplyr::left_join(
      DrugUtilisation::patternTable(cdm = cdm) %>%
        dplyr::select(
          "numerator_unit_concept_id", "amount_unit_concept_id",
          "denominator_unit_concept_id", "denominator_numeric",
          "pattern_id"
        ) %>%
        dplyr::filter(!is.na(.data$pattern_id)) %>%
        dplyr::mutate(denominator_value = as.character(dplyr::if_else(.data$denominator_numeric == 1, "",
          dplyr::if_else(.data$denominator_numeric == 0, "missing", "FLAG")
        ))),
      by = c("pattern_id")
    ) %>%
    dplyr::left_join(
      patterns %>%
        dplyr::select(
          "amount_unit_concept_id", "amount_unit",
          "numerator_unit_concept_id", "numerator_unit",
          "denominator_unit_concept_id", "denominator_unit",
          "denominator"
        ) %>%
        dplyr::mutate(denominator_value = dplyr::case_when(
          is.na(.data$denominator) ~ "missing",
          .data$denominator == "numeric" ~ ""
        )),
      by = c(
        "numerator_unit_concept_id", "amount_unit_concept_id",
        "denominator_unit_concept_id", "denominator_value"
      )
    ) %>%
    dplyr::mutate(pattern_id_name = dplyr::case_when(
      is.na(.data$amount_unit) ~ paste(.data$numerator_unit, .data$denominator_value, .data$denominator_unit, sep = "_"),
      !is.na(.data$amount_unit) ~ paste0("fixed_amount_", .data$amount_unit)
    )) %>%
    dplyr::mutate(
      pattern_name = gsub("[^a-zA-Z ]", "", .data$strata_level)
    ) %>%
    dplyr::mutate(
      pattern_name = paste(.data$pattern_name, .data$pattern_id_name, sep = " ")
    ) %>%
    dplyr::filter(
      !grepl("NA NA_NA_NA", .data$pattern_name)
    ) %>%
    dplyr::mutate(
      pattern_name = gsub(" NA_NA_NA", "", .data$pattern_name)
    ) %>%
    dplyr::select(
      "result_id", "cdm_name", "group_name",
      "group_level", "strata_name", "strata_level",
      "variable_name", "variable_level", "estimate_name",
      "estimate_type", "estimate_value", "additional_name",
      "additional_level", "pattern_name"
    ) %>%
    dplyr::mutate(
      ingredient = .data$group_level,
      ingredient_concept_id = .env$ingredientConceptId
    )
  return(records)
}
