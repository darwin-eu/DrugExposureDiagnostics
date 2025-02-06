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

#' @title ShinyApp
#'
#' @description
#' R6 ShinyApp class.
#'
#' @noRd
ShinyApp <- R6::R6Class(
  classname = "ShinyApp",
  inherit = ShinyModule,
  public = list(

    #' @description
    #' Initializer method
    #'
    #' @param resultList (`list`) List containing the output of the checks
    #' @param database_id (`character`)
    #'
    #' @return (`invisible(self)`)
    initialize = function(resultList, database_id) {
      super$initialize()
      private$.resultList <- resultList
      private$.database_id <- database_id
      lapply(names(private$.resultList), FUN = function(name) {
        private$.resultList[[name]] <- dplyr::bind_cols(
          database_id = database_id,
          private$.resultList[[name]]
        )
      })
      if (nrow(private$.resultList$conceptSummary) > 0) {
        ingredientConceptsColumnsToHide <- c(
          "concept_code", "valid_start_date", "valid_end_date",
          "invalid_reason", "amount_value", "amount_unit_concept_id", "numerator_value",
          "numerator_unit_concept_id", "numerator_unit", "denominator_value",
          "denominator_unit_concept_id", "denominator_unit", "box_size", "amount_unit"
        )
        ingredientConceptColumnsSelected <- colnames(private$.resultList$conceptSummary)
        ingredientConceptColumnsSelected <- setdiff(ingredientConceptColumnsSelected, ingredientConceptsColumnsToHide)
        private$.ingredientConceptsTab <- dataPlotPanel$new(
          data = private$.resultList$conceptSummary,
          id = "ingredientConcepts",
          title = "Ingredient concepts",
          description = "Ingredient concepts",
          plotPercentage = FALSE,
          byConcept = FALSE,
          downloadFilename = "IngredientConcepts.csv",
          selectedColumns = ingredientConceptColumnsSelected
        )
        private$.ingredientConceptsTab$parentNamespace <- self$namespace
      }
      if (nrow(private$.resultList$drugRoutesOverall) > 0) {
        private$.drugRoutesTab <- dataPlotPanel$new(
          data = private$.resultList$drugRoutesOverall,
          dataByConcept = private$.resultList$drugRoutesByConcept,
          id = "drugRoutes",
          title = "Drug routes",
          description = "Drug routes",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugRoutes.csv"
        )
        private$.drugRoutesTab$parentNamespace <- self$namespace
      }
      if (nrow(private$.resultList$drugTypesOverall) > 0) {
        private$.drugTypesTab <- dataPlotPanel$new(
          data = private$.resultList$drugTypesOverall,
          dataByConcept = private$.resultList$drugTypesByConcept,
          id = "drugTypes",
          title = "Drug types",
          description = "Drug types",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugTypes.csv"
        )
        private$.drugTypesTab$parentNamespace <- self$namespace
      }
      if (nrow(private$.resultList$drugSourceConceptsOverall) > 0) {
        private$.drugSourceConceptsTab <- dataPlotPanel$new(
          data = private$.resultList$drugSourceConceptsOverall,
          id = "drugSourceConcepts",
          title = "Drug source concepts",
          description = "Drug source concepts",
          plotPercentage = FALSE,
          byConcept = FALSE,
          downloadFilename = "DrugSourceConcepts.csv"
        )
        private$.drugSourceConceptsTab$parentNamespace <- self$namespace
      }
      if (nrow(private$.resultList$drugExposureDurationOverall) > 0) {
        private$.drugExposureDurationTab <- dataPlotPanel$new(
          data = private$.resultList$drugExposureDurationOverall,
          dataByConcept = private$.resultList$drugExposureDurationByConcept,
          id = "drugExposureDuration",
          title = "Drug exposure duration",
          description = "Drug exposure duration",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugExposureDuration.csv"
        )
        private$.drugExposureDurationTab$parentNamespace <- self$namespace
      }
      if (nrow(private$.resultList$missingValuesOverall) > 0) {
        private$.drugVariablesMissingTab <- dataPlotPanel$new(
          data = private$.resultList$missingValuesOverall,
          dataByConcept = private$.resultList$missingValuesByConcept,
          id = "drugVariablesMissing",
          title = "Drug variables missing",
          description = "Drug variables missing",
          plotPercentage = TRUE,
          byConcept = TRUE,
          downloadFilename = "DrugVariablesMissing.csv"
        )
        private$.drugVariablesMissingTab$parentNamespace <- self$namespace
      }
      if (nrow(private$.resultList$drugDaysSupply) > 0) {
        private$.drugDaysSupplyTab <- dataPlotPanel$new(
          data = private$.resultList$drugDaysSupply,
          dataByConcept = private$.resultList$drugDaysSupplyByConcept,
          id = "drugDaysSupply",
          title = "Drug days supply",
          description = "Drug days supply",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugDaysSupply.csv"
        )
        private$.drugDaysSupplyTab$parentNamespace <- self$namespace
      }
      if (nrow(private$.resultList$drugQuantity) > 0) {
        private$.drugQuantityTab <- dataPlotPanel$new(
          data = private$.resultList$drugQuantity,
          dataByConcept = private$.resultList$drugQuantityByConcept,
          id = "drugQuantity",
          title = "Drug quantity",
          description = "Drug quantity",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugQuantity.csv"
        )
        private$.drugQuantityTab$parentNamespace <- self$namespace
      }
      if (nrow(private$.resultList$drugSig) > 0) {
        private$.drugSigTab <- dataPlotPanel$new(
          data = private$.resultList$drugSig,
          dataByConcept = private$.resultList$drugSigByConcept,
          id = "drugSig",
          title = "Drug sig",
          description = "Drug sig",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugSig.csv"
        )
        private$.drugSigTab$parentNamespace <- self$namespace
      }
      if (nrow(private$.resultList$drugVerbatimEndDate) > 0) {
        private$.drugVerbatimEndDateTab <- dataPlotPanel$new(
          data = private$.resultList$drugVerbatimEndDate,
          dataByConcept = private$.resultList$drugVerbatimEndDateByConcept,
          id = "drugVerbatimEndDate",
          title = "Drug verbatim end date",
          description = "Drug verbatim end date",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugVerbatimEndDate.csv"
        )
        private$.drugVerbatimEndDateTab$parentNamespace <- self$namespace
      }
      if (nrow(private$.resultList$drugDose) > 0) {
        private$.drugDailyDoseTab <- dataPlotPanel$new(
          data = private$.resultList$drugDose,
          id = "drugDailyDose",
          title = "Drug daily dose",
          description = "Drug daily dose",
          plotPercentage = FALSE,
          byConcept = FALSE,
          downloadFilename = "DrugDailyDose.csv"
        )
        private$.drugDailyDoseTab$parentNamespace <- self$namespace
      }
      # metadata
      if (nrow(private$.resultList$metadata) > 0) {
        private$.metaDataTab <- metaDataPanel$new(
          data = private$.resultList$metadata,
          id = "metaData",
          title = "Metadata",
          description = "Metadata",
          downloadFilename = "metaData.csv"
        )
        private$.metaDataTab$parentNamespace <- self$namespace
      }
      return(invisible(self))
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .resultList = NULL,
    .database_id = NULL,
    .ingredientConceptsTab = NULL,
    .drugRoutesTab = NULL,
    .drugTypesTab = NULL,
    .drugSourceConceptsTab = NULL,
    .drugExposureDurationTab = NULL,
    .drugVariablesMissingTab = NULL,
    .drugDaysSupplyTab = NULL,
    .drugQuantityTab = NULL,
    .drugSigTab = NULL,
    .drugVerbatimEndDateTab = NULL,
    .drugDailyDoseTab = NULL,
    .metaDataTab = NULL,

    ### Methods ----
    .UI = function() {
      allTabsList <- list(widths = c(2, 10))
      if (!is.null(private$.ingredientConceptsTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.ingredientConceptsTab$uiBody()
      }
      if (!is.null(private$.drugRoutesTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.drugRoutesTab$uiBody()
      }
      if (!is.null(private$.drugTypesTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.drugTypesTab$uiBody()
      }
      if (!is.null(private$.drugSourceConceptsTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.drugSourceConceptsTab$uiBody()
      }
      if (!is.null(private$.drugExposureDurationTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.drugExposureDurationTab$uiBody()
      }
      if (!is.null(private$.drugVariablesMissingTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.drugVariablesMissingTab$uiBody()
      }
      if (!is.null(private$.drugDaysSupplyTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.drugDaysSupplyTab$uiBody()
      }
      if (!is.null(private$.drugQuantityTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.drugQuantityTab$uiBody()
      }
      if (!is.null(private$.drugSigTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.drugSigTab$uiBody()
      }
      if (!is.null(private$.drugVerbatimEndDateTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.drugVerbatimEndDateTab$uiBody()
      }
      if (!is.null(private$.drugDailyDoseTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.drugDailyDoseTab$uiBody()
      }
      if (!is.null(private$.metaDataTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.metaDataTab$uiBody()
      }
      shiny::fluidPage(
        theme = bslib::bs_theme(version = "5", bootswatch = "spacelab"),
        shinyjs::useShinyjs(),
        shiny::titlePanel(
          title = shiny::h2("Drug Exposure Diagnostics Dashboard", align = "center"),
          windowTitle = "Drug Exposure Diagnostics Dashboard"
        ),
        do.call(navlistPanel, allTabsList)
      )
    },
    .server = function(input, output, session) {
      private$.ingredientConceptsTab$server(input, output, session)
      private$.drugRoutesTab$server(input, output, session)
      private$.drugTypesTab$server(input, output, session)
      private$.drugSourceConceptsTab$server(input, output, session)
      private$.drugExposureDurationTab$server(input, output, session)
      private$.drugVariablesMissingTab$server(input, output, session)
      private$.drugDaysSupplyTab$server(input, output, session)
      private$.drugQuantityTab$server(input, output, session)
      private$.drugSigTab$server(input, output, session)
      private$.drugVerbatimEndDateTab$server(input, output, session)
      private$.drugDailyDoseTab$server(input, output, session)
      private$.metaDataTab$server(input, output, session)
    }
  )
)
