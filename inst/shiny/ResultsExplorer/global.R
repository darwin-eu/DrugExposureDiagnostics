#### PACKAGES -----
library(shiny)
library(bslib)
library(here)
library(readr)
library(dplyr)
library(stringr)
library(checkmate)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(glue)
library(ggplot2)
library(plotly)
library(shinyjs)
source("dataPlotPanel.R")
source("metaDataPanel.R")
source("utils.R")
source("plots.R")

# input directory: this directory should have 1 or more zip files
if (exists("shinySettings") && !is.null(shinySettings$dataFolder)) {
  DATA_DIRECTORY <- shinySettings$dataFolder
} else {
  DATA_DIRECTORY <- "data"
}

if (!dir.exists(DATA_DIRECTORY)) {
  stop(glue("Please make sure the '{DATA_DIRECTORY}' directory exists."))
}

# Check for zip files
zipFiles <- list.files(DATA_DIRECTORY, pattern = ".zip", full.names = TRUE)
if (length(zipFiles) == 0) {
  stop(glue("Please make sure the '{DATA_DIRECTORY}' contains 1 or more zip files from drug exposure diagnostics."))
}

# Process zip files
for (i in 1:length(zipFiles)) {
  writeLines(paste("Processing", zipFiles[i]))
  tempFolder <- tempfile()
  dir.create(tempFolder)
  unzip(zipFiles[i], exdir = tempFolder, junkpaths = TRUE)

  csvFiles <- list.files(tempFolder, pattern = ".csv")
  lapply(csvFiles, loadFile, folder = tempFolder, overwrite = (i == 1), i)

  unlink(tempFolder, recursive = TRUE)
}

commonInputsInitValue <- c("INIT")

ingredientConcepts <- formatResult(conceptsummary)

# data overall and by concept
drugRoutes <- drugRoutesByConcept <- data.frame()
if (exists("drugroutesoverall")) {
  drugRoutes <- formatResult(drugroutesoverall)
  drugRoutesByConcept <- formatResult(drugroutesbyconcept)
}

drugVariablesMissing <- drugVariablesMissingByConcept <- data.frame()
if (exists("missingvaluesoverall")) {
  drugVariablesMissing <- formatResult(missingvaluesoverall)
  drugVariablesMissingByConcept <- formatResult(missingvaluesbyconcept)
}

drugTypes <- drugTypesByConcept <- data.frame()
if (exists("drugtypesoverall")) {
  drugTypes <- formatResult(drugtypesoverall)
  drugTypesByConcept <- formatResult(drugtypesbyconcept)
}

drugExposureDuration <- drugExposureDurationByConcept <- data.frame()
if (exists("drugexposuredurationoverall")) {
  drugExposureDuration <- formatResult(drugexposuredurationoverall)
  drugExposureDurationByConcept <- formatResult(drugexposuredurationbyconcept)
}

drugSourceConcepts <- data.frame()
if (exists("drugsourceconceptsoverall")) {
  drugSourceConcepts <- formatResult(drugsourceconceptsoverall)
}

drugDaysSupply <- drugDaysSupplyByConcept <- data.frame()
if (exists("drugdayssupply")) {
  drugDaysSupply <- formatResult(drugdayssupply)
  drugDaysSupplyByConcept <- formatResult(drugdayssupplybyconcept)
}

drugQuantity <- drugQuantityByConcept <- data.frame()
if (exists("drugquantity")) {
  drugQuantity <- formatResult(drugquantity)
  drugQuantityByConcept <- formatResult(drugquantitybyconcept)
}

drugSig <- drugSigByConcept <- data.frame()
if (exists("drugsig")) {
  drugSig <- formatResult(drugsig)
  drugSigByConcept <- formatResult(drugsigbyconcept)
}

drugVerbatimEndDate <- drugVerbatimEndDateByConcept <- data.frame()
if (exists("drugverbatimenddate")) {
  drugVerbatimEndDate <- formatResult(drugverbatimenddate)
  drugVerbatimEndDateByConcept <- formatResult(drugverbatimenddatebyconcept)
}

drugDailyDose <- data.frame()
if (exists("drugdose")) {
  drugDailyDose <- formatResult(drugdose) %>%
    dplyr::filter(!is.na(strata_level)) %>%
    dplyr::mutate(result_id = as.integer(result_id))
}

metaData <- data.frame()
if (exists("metadata")) {
  metaData <- metadata
}

# show/hide columns
ingredientConceptsColumnsToHide <- c(
  "concept_code", "valid_start_date", "valid_end_date",
  "invalid_reason", "amount_value", "amount_unit_concept_id", "numerator_value",
  "numerator_unit_concept_id", "numerator_unit", "denominator_value",
  "denominator_unit_concept_id", "denominator_unit", "box_size", "amount_unit"
)
ingredientConceptColumnsSelected <- colnames(ingredientConcepts)
ingredientConceptColumnsSelected <- setdiff(ingredientConceptColumnsSelected, ingredientConceptsColumnsToHide)

# ggplot modules (others will be plotly)
ggplotModules <- c("drugDailyDose", "drugQuantity", "drugExposureDuration", "drugDaysSupply")
