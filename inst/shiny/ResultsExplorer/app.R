#### PACKAGES -----
library(DrugExposureDiagnostics)
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
source("utils.R")

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

data <- list("conceptSummary" = conceptsummary,
             "metadata" = metadata)

if (exists("drugroutesoverall")) {
  data <- append(data, list("drugRoutesOverall" = drugroutesoverall,
                            "drugRoutesByConcept" = drugroutesbyconcept))
}
if (exists("drugtypesoverall")) {
  data <- append(data, list("drugTypesOverall" = drugtypesoverall,
                            "drugTypesByConcept" = drugtypesbyconcept))
}
if (exists("drugsourceconceptsoverall")) {
  data <- append(data, list("drugSourceConceptsOverall" = drugsourceconceptsoverall))
}
if (exists("drugexposuredurationoverall")) {
  data <- append(data, list("drugExposureDurationOverall" = drugexposuredurationoverall,
                            "drugExposureDurationByConcept" = drugexposuredurationbyconcept))
}
if (exists("missingvaluesoverall")) {
  data <- append(data, list("missingValuesOverall" = missingvaluesoverall,
                            "missingValuesByConcept" = missingvaluesbyconcept))
}
if (exists("drugdayssupply")) {
  data <- append(data, list("drugDaysSupply" = drugdayssupply,
                            "drugDaysSupplyByConcept" = drugdayssupplybyconcept))
}
if (exists("drugquantity")) {
  data <- append(data, list("drugQuantity" = drugquantity,
                            "drugQuantityByConcept" = drugquantitybyconcept))
}
if (exists("drugsig")) {
  data <- append(data, list("drugSig" = drugsig,
                            "drugSigByConcept" = drugsigbyconcept))
}
if (exists("drugverbatimenddate")) {
  data <- append(data, list("drugVerbatimEndDate" = drugverbatimenddate,
                            "drugVerbatimEndDateByConcept" = drugverbatimenddatebyconcept))
}
if (exists("drugdose")) {
  data <- append(data, list("drugDose" = drugdose))
}
if (exists("drugtimebetween")) {
  data <- append(data, list("drugTimeBetween" = drugtimebetween))
  data <- append(data, list("drugTimeBetweenByConcept" = drugtimebetweenbyconcept))
}


ded <- DrugExposureDiagnostics:::ShinyApp$new(resultList = data)

ui <- shiny::fluidPage(
  shiny::tagList(
    ded$UI()
  )
)

server <- function(input, output, session) {
  ded$server(input, output, session)
}

shiny::shinyApp(ui, server)
