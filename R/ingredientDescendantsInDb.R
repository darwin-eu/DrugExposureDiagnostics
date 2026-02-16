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

#' Get the descendants for the given ingredients
#'
#' @param cdm CDMConnector reference object
#' @param ingredient ingredient concept id for ingredient of interest
#' @param drugRecordsTable table name of the drug exposure records, default "drug_exposure"
#' @param tablePrefix The stem for the permanent tables that will
#' be created when running the diagnostics. Permanent tables will be created using
#' this prefix, and any existing tables that start with this will be at risk of
#' being dropped or overwritten. If NULL, temporary tables will be
#' used throughout.
#' @param verbose if verbose set to TRUE, the function will output extra messages
#'
#' @return temp table with concepts used
ingredientDescendantsInDb <- function(cdm,
                                      ingredient,
                                      drugRecordsTable = "drug_exposure",
                                      tablePrefix = NULL,
                                      verbose = FALSE) {
  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, messageStore = errorMessage)
  checkTableExists(
    cdm = cdm, tableName = drugRecordsTable,
    messageStore = errorMessage
  )
  checkLogical(verbose, messageStore = errorMessage)
  checkIngredientInTable(cdm = cdm, conceptId = ingredient, tableName = "drug_strength", messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (verbose == TRUE) {
    start_collect <- Sys.time()
    message("Progress: getting descendant concepts of ingredient")
  }
  dbConceptsTable <- cdm$concept_ancestor %>%
    dplyr::filter(.data$ancestor_concept_id == .env$ingredient) %>%
    dplyr::select("descendant_concept_id") %>%
    dplyr::rename("concept_id" = "descendant_concept_id") %>%
    dplyr::left_join(cdm$concept,
      by = "concept_id"
    )

  # store result
  dbConceptsTable <- computeDBQuery(
    table = dbConceptsTable,
    tablePrefix = tablePrefix,
    tableName = "_DED_concepts_1",
    cdm = cdm
  )

  if (verbose == TRUE) {
    message("Progress: adding drug strength info")
  }
  dbConceptsTable <- dbConceptsTable %>%
    dplyr::left_join(
      cdm$drug_strength %>%
        dplyr::filter(.data$ingredient_concept_id == .env$ingredient) %>%
        dplyr::select(!c(
          "valid_start_date",
          "valid_end_date",
          "invalid_reason"
        )),
      by = c("concept_id" = "drug_concept_id")
    )
  # store result
  dbConceptsTable <- computeDBQuery(
    table = dbConceptsTable,
    tablePrefix = tablePrefix,
    tableName = "_DED_concepts_2",
    cdm = cdm
  )

  if (verbose == TRUE) {
    message("Progress: limiting to concepts in the db")
  }
  dbConceptsTable <- dbConceptsTable %>%
    dplyr::inner_join(
      cdm[[drugRecordsTable]] %>%
        dplyr::select("drug_concept_id") %>%
        dplyr::distinct(),
      by = c("concept_id" = "drug_concept_id")
    ) %>%
    dplyr::distinct()
  # store result
  dbConceptsTable <- computeDBQuery(
    table = dbConceptsTable,
    tablePrefix = tablePrefix,
    tableName = "_DED_concepts_3",
    cdm = cdm
  )

  if (verbose == TRUE) {
    message("Progress: adding concept names")
  }
  ingredientTable <- dbConceptsTable %>%
    dplyr::select("concept_id", "ingredient_concept_id") %>%
    dplyr::left_join(cdm$concept,
      by = c("ingredient_concept_id" = "concept_id")
    ) %>%
    dplyr::select("concept_id", "concept_name") %>%
    dplyr::rename("ingredient" = "concept_name")

  amountUnitTable <- dbConceptsTable %>%
    dplyr::select("concept_id", "amount_unit_concept_id") %>%
    dplyr::left_join(cdm$concept,
      by = c("amount_unit_concept_id" = "concept_id")
    ) %>%
    dplyr::select("concept_id", "concept_name") %>%
    dplyr::rename("amount_unit" = "concept_name")

  numeratorUnitTable <- dbConceptsTable %>%
    dplyr::select("concept_id", "numerator_unit_concept_id") %>%
    dplyr::left_join(cdm$concept,
      by = c("numerator_unit_concept_id" = "concept_id")
    ) %>%
    dplyr::select("concept_id", "concept_name") %>%
    dplyr::rename("numerator_unit" = "concept_name")

  denominatorUnitTable <- dbConceptsTable %>%
    dplyr::select("concept_id", "denominator_unit_concept_id") %>%
    dplyr::left_join(cdm$concept,
      by = c("denominator_unit_concept_id" = "concept_id")
    ) %>%
    dplyr::select("concept_id", "concept_name") %>%
    dplyr::rename("denominator_unit" = "concept_name")

  dbConceptsTable <- dbConceptsTable %>%
    dplyr::full_join(ingredientTable,
      by = "concept_id"
    ) %>%
    dplyr::full_join(amountUnitTable,
      by = "concept_id"
    ) %>%
    dplyr::full_join(numeratorUnitTable,
      by = "concept_id"
    ) %>%
    dplyr::full_join(denominatorUnitTable,
      by = "concept_id"
    )
  # store result
  dbConceptsTable <- computeDBQuery(
    table = dbConceptsTable,
    tablePrefix = tablePrefix,
    tableName = "_DED_concepts_4",
    cdm = cdm
  )

  # add rxnorm dose form
  drugConceptForm <- dbConceptsTable %>%
    dplyr::select("concept_id") %>%
    dplyr::left_join(
      cdm$concept_relationship %>%
        dplyr::filter(.data$relationship_id ==
          "RxNorm has dose form") %>%
        dplyr::left_join(cdm$concept,
          by = c("concept_id_2" = "concept_id")
        ) %>%
        dplyr::select("concept_id_1", "concept_id_2", "concept_name") %>%
        dplyr::distinct(),
      by = c("concept_id" = "concept_id_1")
    ) %>%
    dplyr::select("concept_id", "concept_name") %>%
    dplyr::rename("dose_form" = "concept_name") %>%
    dplyr::collect()

  # can have multiple forms so pivot (locally)
  drugConceptForm <- drugConceptForm %>%
    dplyr::group_by(.data$concept_id) %>%
    dplyr::mutate(seq = dplyr::row_number()) %>%
    tidyr::pivot_wider(
      names_from = "seq",
      values_from = "dose_form"
    )
  if (nrow(drugConceptForm) > 0) {
    if (ncol(drugConceptForm) > 2) { # multiple forms for at least one concept
      drugConceptForm <- drugConceptForm %>%
        tidyr::unite(
          col = "dose_form", 2:ncol(drugConceptForm), sep = "; ",
          na.rm = TRUE
        )
    } else {
      names(drugConceptForm)[2] <- "dose_form"
    }

    drugConceptFormTblName <- CDMConnector::uniqueTableName()
    cdm <- CDMConnector::insertTable(
      cdm = cdm, name = drugConceptFormTblName, table = drugConceptForm, overwrite = TRUE
    )
    dbConceptsTable <- dbConceptsTable %>%
      dplyr::left_join(cdm[[drugConceptFormTblName]],
        by = "concept_id"
      )
  } else {
    dbConceptsTable <- dbConceptsTable %>%
      dplyr::mutate(dose_form = as.character(NA))
  }
  # store result
  dbConceptsTable <- computeDBQuery(
    table = dbConceptsTable,
    tablePrefix = tablePrefix,
    tableName = "_DED_concepts_5",
    cdm = cdm
  )

  if (verbose == TRUE) {
    duration <- abs(as.numeric(Sys.time() - start_collect, units = "secs"))
    message(glue::glue(
      "Overall time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }
  return(dbConceptsTable)
}
