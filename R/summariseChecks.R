# Copyright 2023 DARWIN EU®
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

#' Create a summary about the diagnostics results
#'
#' @param resultList a list with the diagnostics results
#'
#' @return a table containing the diagnostics summary
summariseChecks <- function(resultList) {
  # total by ingredient
  diagnostics_summary <- resultList$conceptSummary %>%
    dplyr::group_by(.data$ingredient, .data$ingredient_concept_id) %>%
    dplyr::summarise(
      n_records = sum(.data$n_records),
      .groups = "drop"
    )

  # proportion with rxnorm dose form
  totN <- sum(resultList$conceptSummary$n_records)
  withDoseN <- resultList$conceptSummary %>%
    dplyr::filter(!is.na(.data$dose_form)) %>%
    dplyr::count() %>%
    dplyr::pull()
  diagnostics_summary$proportion_of_records_with_dose_form <- withDoseN

  # proportion of available route_type
  if (!is.null(resultList$drugRoutesOverall)) {
    totN <- as.numeric(sum(resultList$drugRoutesOverall$n_records))
    routes <- resultList$drugRoutesOverall %>%
      dplyr::mutate(route_type_n = paste0(
        .data$route_type,
        " (", .data$n_records / .env$totN, ")"
      ))
    diagnostics_summary$proportion_of_records_by_route_type <- paste(routes$route_type_n, collapse = ";")
  }

  # drug type
  if (!is.null(resultList$drugTypesOverall)) {
    totN <- as.numeric(sum(resultList$drugTypesOverall$n_records))
    drugTypes <- resultList$drugTypesOverall %>%
      dplyr::mutate(drug_type_n = paste0(
        .data$drug_type,
        " (", .data$n_records / .env$totN, ")"
      ))
    diagnostics_summary$proportion_of_records_by_drug_type <- paste(drugTypes$drug_type_n, collapse = ";")
  }

  # duration
  if (!is.null(resultList$drugExposureDurationOverall)) {
    diagnostics_summary <- diagnostics_summary %>% dplyr::left_join(
      resultList$drugExposureDurationOverall %>%
        dplyr::mutate(median_drug_exposure_days_q05_q95 = paste0(
          .data$median_drug_exposure_days,
          " (", .data$q05_drug_exposure_days,
          " to ",
          .data$q95_drug_exposure_days,
          ")"
        )) %>%
        dplyr::select(
          "ingredient_concept_id",
          "proportion_negative_days",
          "median_drug_exposure_days_q05_q95"
        ) %>%
        dplyr::rename("proportion_of_records_with_negative_drug_exposure_days" =
                        "proportion_negative_days"),
      by = "ingredient_concept_id"
    )
  }

  # days supply
  if (!is.null(resultList$drugDose)) {
    diagnostics_summary <- diagnostics_summary %>% dplyr::left_join(
      resultList$drugDose %>%
        dplyr::select(
          "ingredient_concept_id",
          "proportion_of_records_missing_days_supply_or_dates"
        ),
      by = "ingredient_concept_id"
    ) %>%
      dplyr::mutate(proportion_of_records_missing_days_supply_or_dates =
                      round(.data$proportion_of_records_missing_days_supply_or_dates,4))
  }

  # quantity
  if (!is.null(resultList$drugQuantity)) {
    diagnostics_summary <- diagnostics_summary %>% dplyr::left_join(
      resultList$drugQuantity %>%
        dplyr::mutate(median_quantity_q05_q95 = paste0(
          .data$median_drug_exposure_quantity,
          " (", .data$q05_drug_exposure_quantity,
          " to ",
          .data$q95_drug_exposure_quantity,
          ")"
        )) %>%
        dplyr::select(
          "ingredient_concept_id",
          "median_quantity_q05_q95"
        ),
      by = "ingredient_concept_id"
    )
  }

  if (!is.null(resultList$drugDose)) {
    # missing denominator
    diagnostics_summary <- diagnostics_summary %>% dplyr::left_join(
      resultList$drugDose %>%
        dplyr::select(
          "ingredient_concept_id",
          "proportion_of_records_missing_denominator_unit_concept_id"
        ),
      by = "ingredient_concept_id"
    )

    # value
    diagnostics_summary <- diagnostics_summary %>% dplyr::left_join(
      resultList$drugDose %>%
        dplyr::mutate(median_amount_value_q05_q95 = paste0(
          .data$median_amount_value,
          " (", .data$q05_amount_value,
          " to ",
          .data$q95_amount_value,
          ")"
        )) %>%
        dplyr::select(
          "ingredient_concept_id",
          "median_amount_value_q05_q95"
        ),
      by = "ingredient_concept_id"
    )
  }

  diagnostics_summary <- diagnostics_summary %>%
    dplyr::relocate("ingredient_concept_id")

  return(diagnostics_summary)
}
