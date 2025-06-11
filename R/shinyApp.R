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

#' @title DrugExposureDiagnostics ShinyApp
#'
#' @include ShinyModule.R
#'
#' @description
#' DrugExposureDiagnostics shiny app that shows tables and plots
#'
#' @details
#' The module consists of the following:
#' \describe{
#'   \item{"dataPlotPanel"}{Table and a plot (bar or box) for each check.}
#'   \item{"metaDataPanel"}{Table containing the metadata.}
#' }
ShinyApp <- R6::R6Class(
  classname = "ShinyApp",
  inherit = ShinyModule,
  public = list(

    #' @description
    #' Initializer method
    #'
    #' @param resultList (`list`) List containing the output of the checks
    #' @param database_id (`character`) Database identifier (optional)
    #'
    #' @return (`invisible(self)`)
    initialize = function(resultList, database_id = NULL) {
      super$initialize()
      private$.resultList <- resultList
      if (!is.null(database_id)) {
        private$.database_id <- database_id
        lapply(names(private$.resultList), FUN = function(name) {
          private$.resultList[[name]] <- dplyr::bind_cols(
            database_id = database_id,
            private$.resultList[[name]]
          )
        })
      }
      private$.initIngredientConceptsTab()
      private$.initDrugRoutesTab()
      private$.initDrugTypesTab()
      private$.initDrugSourceConceptsTab()
      private$.initDrugExposureDurationTab()
      private$.initMissingValuesTab()
      private$.initDrugDaysSupplyTab
      private$.initDrugQuantityTab()
      private$.initDrugSigTab()
      private$.initDrugVerbatimEndDateTab()
      private$.initDrugDoseTab()
      private$.initDrugTimeBetweenTab()
      private$.initMetaDataTab()
      return(invisible(self))
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .resultList = NULL,
    .database_id = NULL,
    .modules = list(),

    ### Methods ----
    .UI = function() {
      allTabsList <- list(widths = c(2, 10))
      for (module in private$.modules) {
        allTabsList[[length(allTabsList) + 1]] <- module$uiBody()
      }
      shiny::fluidPage(
        theme = bslib::bs_theme(version = "4", bootswatch = "spacelab"),
        shiny::titlePanel(
          title = h2("Drug Exposure Diagnostics Dashboard", align = "center"),
          windowTitle = "Drug Exposure Diagnostics Dashboard"
        ),
        do.call(navlistPanel, allTabsList)
      )
    },
    .server = function(input, output, session) {
      for (module in private$.modules) {
        module$server(input, output, session)
      }
    },
    .initIngredientConceptsTab = function() {
      if (!is.null(private$.resultList$conceptSummary) && nrow(private$.resultList$conceptSummary) > 0) {
        ingredientConceptsColumnsToHide <- c(
          "concept_code", "valid_start_date", "valid_end_date",
          "invalid_reason", "amount_value", "amount_unit_concept_id", "numerator_value",
          "numerator_unit_concept_id", "numerator_unit", "denominator_value",
          "denominator_unit_concept_id", "denominator_unit", "box_size", "amount_unit"
        )
        ingredientConceptColumnsSelected <- colnames(private$.resultList$conceptSummary)
        ingredientConceptColumnsSelected <- setdiff(ingredientConceptColumnsSelected, ingredientConceptsColumnsToHide)
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$conceptSummary,
          id = "ingredientConcepts",
          title = "Ingredient concepts",
          description = "Ingredient concepts",
          plotPercentage = FALSE,
          byConcept = FALSE,
          downloadFilename = "IngredientConcepts.csv",
          selectedColumns = ingredientConceptColumnsSelected
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initDrugRoutesTab = function() {
      if (!is.null(private$.resultList$drugRoutesOverall) && nrow(private$.resultList$drugRoutesOverall) > 0) {
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$drugRoutesOverall,
          dataByConcept = private$.resultList$drugRoutesByConcept,
          id = "drugRoutes",
          title = "Drug routes",
          description = "Drug routes",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugRoutes.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initDrugTypesTab = function() {
      if (!is.null(private$.resultList$drugTypesOverall) && nrow(private$.resultList$drugTypesOverall) > 0) {
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$drugTypesOverall,
          dataByConcept = private$.resultList$drugTypesByConcept,
          id = "drugTypes",
          title = "Drug types",
          description = "Drug types",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugTypes.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initDrugSourceConceptsTab = function() {
      if (!is.null(private$.resultList$drugSourceConceptsOverall) && nrow(private$.resultList$drugSourceConceptsOverall) > 0) {
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$drugSourceConceptsOverall,
          id = "drugSourceConcepts",
          title = "Drug source concepts",
          description = "Drug source concepts",
          plotPercentage = FALSE,
          byConcept = FALSE,
          downloadFilename = "DrugSourceConcepts.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initDrugExposureDurationTab = function() {
      if (!is.null(private$.resultList$drugExposureDurationOverall) && nrow(private$.resultList$drugExposureDurationOverall) > 0) {
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$drugExposureDurationOverall,
          dataByConcept = private$.resultList$drugExposureDurationByConcept,
          id = "drugExposureDuration",
          title = "Drug exposure duration",
          description = "Drug exposure duration",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugExposureDuration.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initMissingValuesTab = function() {
      if (!is.null(private$.resultList$missingValuesOverall) && nrow(private$.resultList$missingValuesOverall) > 0) {
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$missingValuesOverall,
          dataByConcept = private$.resultList$missingValuesByConcept,
          id = "drugVariablesMissing",
          title = "Drug variables missing",
          description = "Drug variables missing",
          plotPercentage = TRUE,
          byConcept = TRUE,
          downloadFilename = "DrugVariablesMissing.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initDrugDaysSupplyTab = function() {
      if (!is.null(private$.resultList$drugDaysSupply) && nrow(private$.resultList$drugDaysSupply) > 0) {
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$drugDaysSupply,
          dataByConcept = private$.resultList$drugDaysSupplyByConcept,
          id = "drugDaysSupply",
          title = "Drug days supply",
          description = "Drug days supply",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugDaysSupply.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initDrugQuantityTab = function() {
      if (!is.null(private$.resultList$drugQuantity) && nrow(private$.resultList$drugQuantity) > 0) {
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$drugQuantity,
          dataByConcept = private$.resultList$drugQuantityByConcept,
          id = "drugQuantity",
          title = "Drug quantity",
          description = "Drug quantity",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugQuantity.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initDrugSigTab = function() {
      if (!is.null(private$.resultList$drugSig) && nrow(private$.resultList$drugSig) > 0) {
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$drugSig,
          dataByConcept = private$.resultList$drugSigByConcept,
          id = "drugSig",
          title = "Drug sig",
          description = "Drug sig",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugSig.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initDrugVerbatimEndDateTab = function() {
      if (!is.null(private$.resultList$drugVerbatimEndDate) && nrow(private$.resultList$drugVerbatimEndDate) > 0) {
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$drugVerbatimEndDate,
          dataByConcept = private$.resultList$drugVerbatimEndDateByConcept,
          id = "drugVerbatimEndDate",
          title = "Drug verbatim end date",
          description = "Drug verbatim end date",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugVerbatimEndDate.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initDrugDoseTab = function() {
      if (!is.null(private$.resultList$drugDose) && nrow(private$.resultList$drugDose) > 0) {
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$drugDose,
          id = "drugDailyDose",
          title = "Drug daily dose",
          description = "Drug daily dose",
          plotPercentage = FALSE,
          byConcept = FALSE,
          downloadFilename = "DrugDailyDose.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initDrugTimeBetweenTab = function() {
      if (!is.null(private$.resultList$drugTimeBetween) && nrow(private$.resultList$drugTimeBetween) > 0) {
        newModule <- DrugExposureDiagnostics::dataPlotPanel$new(
          data = private$.resultList$drugTimeBetween,
          dataByConcept = private$.resultList$drugTimeBetweenByConcept,
          id = "drugTimeBetween",
          title = "Drug time between",
          description = "Drug time between",
          plotPercentage = FALSE,
          byConcept = TRUE,
          downloadFilename = "DrugTimeBetween.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    .initMetaDataTab = function() {
      if (!is.null(private$.resultList$metadata) && nrow(private$.resultList$metadata) > 0) {
        newModule <- DrugExposureDiagnostics::metaDataPanel$new(
          data = private$.resultList$metadata,
          id = "metaData",
          title = "Metadata",
          description = "Metadata",
          downloadFilename = "metaData.csv"
        )
        newModule$parentNamespace <- self$namespace
        private$.modules <- append(private$.modules, newModule)
      }
    },
    assertInstall = function() {
      if (!require("DrugExposureDiagnostics", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
        answer <- readline(prompt = "`DrugExposureDiagnostics` is not installed, would you like to install from CRAN? (y/n)")
        if (substr(tolower(answer), start = 1, stop = 1) == "y") {
          utils::install.packages("DrugExposureDiagnostics")
        } else if (substr(tolower(answer), start = 1, stop = 1) == "n") {
          stop("You can install `DrugExposureDiagnostics` manually by running one of the following:\n  1. `install.packages('DrugExposureDiagnostics')`\n  2. `remotes::install_github('darwin-eu/DrugExposureDiagnostics')`")
        } else {
          stop("Your answer was not `y` or `n`")
        }
      }
    }
  )
)
