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
  dose_form_table <- resultList$conceptSummary %>%
    dplyr::group_by(.data$ingredient, .data$ingredient_concept_id) %>%
    dplyr::mutate(present_dose_form = dplyr::if_else(!is.na(.data$dose_form), 1, NA)) %>%
    dplyr::filter(!is.na(.data$present_dose_form)) %>%
    dplyr::group_by(.data$ingredient, .data$ingredient_concept_id) %>%
    dplyr::summarise(
      n_dose_form = sum(.data$n_records))

  diagnosticsSummary <- resultList$conceptSummary %>%
    dplyr::group_by(.data$ingredient, .data$ingredient_concept_id) %>%
    dplyr::mutate(total_records = sum(.data$n_records),
                  present_dose_form = dplyr::if_else(!is.na(.data$dose_form),1,NA)) %>%
    dplyr::summarise(
      n_records = sum(.data$n_records),
      n_patients = sum(.data$n_patients)
    ) %>%
    dplyr::left_join(dose_form_table, by = c("ingredient","ingredient_concept_id"))

  # proportion with rxnorm dose form per ingredient
  diagnosticsSummary <- diagnosticsSummary %>%
    dplyr::mutate(n_dose_form = ifelse(is.na(.data$n_dose_form), 0, .data$n_dose_form)) %>%
    dplyr::mutate(proportion_of_records_with_dose_form =
                    glue::glue("{.data$n_dose_form} ({round(100 * .data$n_dose_form / .data$n_records, 1)}%)")) %>%
    dplyr::select(-.data$n_dose_form)

  # missingness
  if (!is.null(resultList$missingValuesOverall) && nrow(resultList$missingValuesOverall >1)) {
    diagnosticsSummary <- diagnosticsSummary %>%
      dplyr::left_join(
        resultList$missingValuesOverall %>% dplyr::ungroup() %>%
          dplyr::select("ingredient_concept_id", "variable", "n_records",
                        "proportion_records_missing_value") %>%
          dplyr::filter(.data$variable %in% c("n_missing_quantity", "n_missing_drug_exposure_start_date",
                                        "n_missing_drug_exposure_end_date", "n_missing_days_supply")
          ) %>% dplyr::mutate(missing =
            glue::glue("
        {.data$n_records} ({.data$proportion_records_missing_value}%)")
            ) %>% tidyr::pivot_wider(names_from = .data$variable, values_from = .data$missing) %>%
      dplyr::mutate(missing_quantity_exp_start_end_days_supply =
                      glue::glue("
        {.data$n_missing_quantity}, {.data$n_missing_drug_exposure_start_date}, {.data$n_missing_drug_exposure_end_date}, {.data$n_missing_days_supply}")
                    ) %>%
          dplyr::select("missing_quantity_exp_start_end_days_supply","ingredient_concept_id"),
      by = "ingredient_concept_id"
      )
  }

  # drug type
  if (!is.null(resultList$drugTypesOverall)) {
    totN <- as.numeric(sum(resultList$drugTypesOverall$n_records))
    drugTypes <- resultList$drugTypesOverall %>%
      dplyr::mutate(drug_type_n = glue::glue("
        {.data$drug_type} ({n_records}, {round(100 * .data$n_records / .env$totN, 1)}%)"))
    diagnosticsSummary$proportion_of_records_by_drug_type <- paste(drugTypes$drug_type_n, collapse = ";")
  }

  # proportion of available route_type
  if (!is.null(resultList$drugRoutesOverall)) {
    totN <- as.numeric(sum(resultList$drugRoutesOverall$n_records))
    routes <- resultList$drugRoutesOverall %>%
      dplyr::mutate(route_type_n = glue::glue("
        {.data$route_type} ({n_records}, {round(100 * .data$n_records / .env$totN, 1)}%)"))
    diagnosticsSummary$proportion_of_records_by_route_type <- paste(routes$route_type_n, collapse = ";")
  }

  # duration
  if (!is.null(resultList$drugExposureDurationOverall)) {
    diagnosticsSummary <- diagnosticsSummary %>% dplyr::left_join(
      resultList$drugExposureDurationOverall %>%
        dplyr::mutate(median_drug_exposure_days_q05_q95 = glue::glue(paste(
          "{.data$median_drug_exposure_days} ({.data$q05_drug_exposure_days}-{.data$q95_drug_exposure_days})")))
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
  if (!is.null(resultList$drugQuantity)) {
    diagnosticsSummary <- diagnosticsSummary %>% dplyr::left_join(
      resultList$drugQuantity %>%
        dplyr::mutate(median_quantity_q05_q95 = glue::glue(paste(
          "{.data$median_drug_exposure_quantity} ({.data$q05_drug_exposure_quantity}-{.data$q95_drug_exposure_quantity})")))
        %>%
        dplyr::select(
          "ingredient_concept_id",
          "median_quantity_q05_q95"
        ),
      by = "ingredient_concept_id"
    )
  }

  # dose count, missingness count. If result is empty, there will be just 2 rows
  if (!is.null(resultList$drugDose) && nrow(resultList$drugDose) > 2) {
    diagnosticsSummary <- diagnosticsSummary %>% dplyr::left_join(
      resultList$drugDose %>%
        dplyr::select(
          "ingredient_concept_id",
          "strata_name",
          "estimate_name",
          "estimate_value",
          "group_name"
        ) %>% dplyr::mutate (
          estimate_value = round(as.numeric(.data$estimate_value),1)
          ) %>%
        dplyr::filter(.data$strata_name == "overall",
                      .data$estimate_name %in% c("count", "count_missing", "percentage_missing")
        ) %>%  tidyr::pivot_wider(names_from = .data$estimate_name, values_from = .data$estimate_value) %>%
        dplyr::mutate(n_dose_and_missingness =
                        glue::glue(paste("{.data$count} ({.data$count_missing}, {.data$percentage_missing}%)"))) %>%
        dplyr::select("ingredient_concept_id","n_dose_and_missingness"),
      by = "ingredient_concept_id"
    )

    # get available units
    units <- resultList$drugDose %>%
      dplyr::select(
        "ingredient_concept_id",
        "strata_name",
        "strata_level"
      ) %>%
      dplyr::filter(.data$strata_name == "unit") %>%
      dplyr::group_by(.data$ingredient_concept_id) %>%
      dplyr::distinct(.data$strata_level) %>%
      tidyr::pivot_wider(names_from = .data$strata_level,values_from = .data$strata_level, names_prefix = "unit_") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(unit = paste(
        dplyr::c_across(dplyr::starts_with("unit_")), collapse = "//")) %>%
      dplyr::ungroup()


    # dose value
    diagnosticsSummary <- diagnosticsSummary %>% dplyr::left_join(
      resultList$drugDose %>%
        dplyr::select(
          "ingredient_concept_id",
          "strata_name",
          "strata_level",
          "estimate_name",
          "estimate_value"
        ) %>%
        dplyr::filter(.data$strata_name == "overall",
                      .data$estimate_name %in% c("q05","median","q95")
        ) %>%
        dplyr::mutate (
          estimate_value = round(as.numeric(.data$estimate_value),4)
        ) %>%
        tidyr::pivot_wider(names_from = .data$estimate_name, values_from = .data$estimate_value) %>%
        dplyr::left_join(
          units %>%
            dplyr::select("ingredient_concept_id","unit"), by = "ingredient_concept_id"
        ) %>%
        dplyr::mutate(median_daily_dose_q05_q95 =
                        glue::glue(paste("{.data$median} ({.data$q05}-{.data$q95}) [{.data$unit}]"))) %>%
        dplyr::select("ingredient_concept_id","median_daily_dose_q05_q95"),
      by = "ingredient_concept_id"
    )
  }

  diagnosticsSummary <- diagnosticsSummary %>%
    dplyr::select(tidyselect::any_of(c("ingredient", "ingredient_concept_id", "n_records", "n_patients",
                                       "proportion_of_records_by_drug_type",
                                       "proportion_of_records_by_route_type",
                                       "proportion_of_records_with_dose_form",
                                       "missing_quantity_exp_start_end_days_supply",
                                       "n_dose_and_missingness",
                                       "median_daily_dose_q05_q95",
                                       "median_quantity_q05_q95",
                                       "median_drug_exposure_days_q05_q95",
                                       "proportion_of_records_with_negative_drug_exposure_days")))
  return(diagnosticsSummary)
}
