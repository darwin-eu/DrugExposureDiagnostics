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

#' Create a summary about the diagnostics results
#'
#' @param resultList a list with the diagnostics results
#'
#' @return a table containing the diagnostics summary
summariseChecks <- function(resultList) {
  # total by ingredient
  diagnosticsSummary <- resultList$conceptSummary %>%
    dplyr::group_by(.data$ingredient, .data$ingredient_concept_id) %>%
    dplyr::summarise(
      n_records = sum(.data$n_records),
      n_patients = sum(.data$n_patients),
      n_dose_form = sum(as.numeric(.data$dose_form)),
      .groups = "drop"
    )

  # proportion with rxnorm dose form per ingredient
  diagnosticsSummary <- diagnosticsSummary %>%
    dplyr::mutate(n_dose_form = ifelse(is.na(.data$n_dose_form), 0, .data$n_dose_form)) %>%
    dplyr::mutate(proportion_of_records_with_dose_form =
                    glue::glue("{.data$n_dose_form} ({round(100 * .data$n_dose_form / .data$n_records, 1)}%)")) %>%
    dplyr::select(-.data$n_dose_form)

  # proportion of available route_type
  if (!is.null(resultList$drugRoutesOverall)) {
    totN <- as.numeric(sum(resultList$drugRoutesOverall$n_records))
    routes <- resultList$drugRoutesOverall %>%
      dplyr::mutate(route_type_n = glue::glue("
        {.data$route_type} ({n_records}, {round(100 * .data$n_records / .env$totN, 1)}%)"))
    diagnosticsSummary$proportion_of_records_by_route_type <- paste(routes$route_type_n, collapse = ";")
  }

  # drug type
  if (!is.null(resultList$drugTypesOverall)) {
    totN <- as.numeric(sum(resultList$drugTypesOverall$n_records))
    drugTypes <- resultList$drugTypesOverall %>%
      dplyr::mutate(drug_type_n = glue::glue("
        {.data$drug_type} ({n_records}, {round(100 * .data$n_records / .env$totN, 1)}%)"))
    diagnosticsSummary$proportion_of_records_by_drug_type <- paste(drugTypes$drug_type_n, collapse = ";")
  }

  # duration
  if (!is.null(resultList$drugExposureDurationOverall) && !is.null(resultList$drugDose)) {
    diagnosticsSummary <- diagnosticsSummary %>% dplyr::left_join(
      resultList$drugExposureDurationOverall %>%
        dplyr::left_join(
          resultList$drugDose %>%
            dplyr::select(
              "ingredient_concept_id",
              "missing_days_supply_or_dates",
              "proportion_of_records_missing_days_supply_or_dates"
        )) %>%
        dplyr::mutate(median_drug_exposure_days_q05_q95 = glue::glue(paste(
          "{.data$median_drug_exposure_days} ({.data$q05_drug_exposure_days}-{.data$q95_drug_exposure_days})",
          "[{.data$missing_days_supply_or_dates}, {round(100 * .data$proportion_of_records_missing_days_supply_or_dates, 1)}%]")))
        %>%
        dplyr::mutate(proportion_negative_days = glue::glue("
          {.data$n_negative_days} ({round(100 * .data$proportion_negative_days, 1)}%)")) %>%
        dplyr::select(
          "ingredient_concept_id",
          "proportion_negative_days",
          "median_drug_exposure_days_q05_q95"
        ) %>%
        dplyr::rename("proportion_of_records_with_negative_drug_exposure_days" = "proportion_negative_days"),
      by = "ingredient_concept_id"
    )
  }

  # quantity
  if (!is.null(resultList$drugQuantity) && !is.null(resultList$drugDose)) {
    diagnosticsSummary <- diagnosticsSummary %>% dplyr::left_join(
      resultList$drugQuantity %>%
        dplyr::left_join(
          resultList$drugDose %>%
            dplyr::select(
              "ingredient_concept_id",
              "missing_or_null_quantity",
              "proportion_missing_or_null_quantity"
        )) %>%
        dplyr::mutate(median_quantity_q05_q95 = glue::glue(paste(
          "{.data$median_drug_exposure_quantity} ({.data$q05_drug_exposure_quantity}-{.data$q95_drug_exposure_quantity})",
          "[{.data$missing_or_null_quantity}, {round(100 * .data$proportion_missing_or_null_quantity, 1)}%]")))
        %>%
        dplyr::select(
          "ingredient_concept_id",
          "median_quantity_q05_q95"
        ),
      by = "ingredient_concept_id"
    )
  }

  if (!is.null(resultList$drugDose)) {
    # missing denominator
    diagnosticsSummary <- diagnosticsSummary %>% dplyr::left_join(
      resultList$drugDose %>%
        dplyr::select(
          "ingredient_concept_id",
          "missing_denominator_unit_concept_id",
          "proportion_of_records_missing_denominator_unit_concept_id"
        ) %>%
        dplyr::mutate(proportion_of_records_missing_denominator_unit_concept_id =
                        glue::glue(paste("{.data$missing_denominator_unit_concept_id}",
                                         "({round(100 * .data$proportion_of_records_missing_denominator_unit_concept_id, 1)}%)"))) %>%
        dplyr::select(-.data$missing_denominator_unit_concept_id),
      by = "ingredient_concept_id"
    )

    # amount value
    diagnosticsSummary <- diagnosticsSummary %>% dplyr::left_join(
      resultList$drugDose %>%
        dplyr::mutate(median_amount_value_q05_q95 = glue::glue(paste(
        "{.data$median_amount_value} ({.data$q05_amount_value}-{.data$q95_amount_value})",
        "[{.data$missing_or_null_amount_value}, {round(100 * .data$proportion_of_records_missing_or_null_amount_value, 1)}%]"))) %>%
        dplyr::select(
          "ingredient_concept_id",
          "median_amount_value_q05_q95"
        ),
      by = "ingredient_concept_id"
    )
  }

  diagnosticsSummary <- diagnosticsSummary %>%
    dplyr::select(tidyselect::any_of(c("ingredient", "ingredient_concept_id", "n_records", "n_patients",
                                       "proportion_of_records_by_drug_type",
                                       "proportion_of_records_by_route_type",
                                       "proportion_of_records_with_dose_form",
                                       "proportion_of_records_missing_denominator_unit_concept_id",
                                       "median_amount_value_q05_q95",
                                       "median_quantity_q05_q95",
                                       "median_drug_exposure_days_q05_q95",
                                       "proportion_of_records_with_negative_drug_exposure_days")))
  return(diagnosticsSummary)
}
