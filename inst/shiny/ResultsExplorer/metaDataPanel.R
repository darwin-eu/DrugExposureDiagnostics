# Module to display metadata

# @file metaDataPanel
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


#' The module viewer for rendering the results
#'
#' @param id the unique reference id for the module
#' @param title panel title
#'
#' @return
#' The user interface to the results
#'
#' @export
metaDataViewer <- function(id, title) {
  ns <- shiny::NS(id)
  filterRow <- fluidRow(
    column(width = 3, uiOutput(ns("dbPickerUI")))
  )

  tabPanel(
    title,
    filterRow,
    fluidRow(column(width = 12, uiOutput(ns("tableDescription")))),
    tags$hr(),
    withSpinner(DT::dataTableOutput(ns("mainTable"))),
    tags$hr(),
    uiOutput(ns("downloadButtonUI"))
  )
}

#' The module server for rendering the results
#'
#' @param id the unique reference id for the module
#' @param data the data
#' @param downloadFilename filename of the table data that can be downloaded
#' @param description table description
#' @param commonInputs common inputs
#'
#' @return the results table server
#'
#' @export
metaDataServer <- function(id, data, downloadFilename, description, commonInputs) {
  ns <- shiny::NS(id)

  shiny::moduleServer(
    id,
    function(input, output, session) {
      requiredCols <- c("database_id")
      referenceTabId <- "ingredientConcepts"

      if (nrow(data) > 0) {
        databases <- unique(data$database_id)
        columns <- colnames(data)[!colnames(data) %in% requiredCols]

        getData <- reactive({
          result <- NULL
          databases <- input$dbPicker
          if (!is.null(databases)) {
            result <- data
            result <- result %>%
              filter(database_id %in% databases)
          }
        })

        observe({
          req(commonInputs)
          if (id != referenceTabId) {
            if (!identical(commonInputs$databases, commonInputsInitValue)) {
              databases <- commonInputs$databases
              updatePickerInput(
                session = session,
                inputId = "dbPicker",
                selected = databases
              )
            }
          }
        })

        output$tableDescription <- renderUI({
          HTML(glue::glue("<center><b>{description}</b></center>"))
        })

        output$dbPickerUI <- renderUI({
          if (id != referenceTabId && !identical(commonInputs$databases, commonInputsInitValue)) {
            databases <- commonInputs$databases
          }
          pickerInput <- pickerInput(
            inputId = ns("dbPicker"),
            label = "Databases",
            choices = databases,
            options = list(`actions-box` = TRUE),
            multiple = T,
            selected = databases
          )
          if (id != referenceTabId) {
            pickerInput <- disabled(pickerInput)
          }
          pickerInput
        })

        output$downloadButtonUI <- renderUI({
          data <- getData()
          if (!is.null(data) && nrow(data) > 0) {
            downloadBttn(ns("downloadButton"),
              size = "xs",
              label = "Download"
            )
          }
        })

        output$mainTable <- DT::renderDataTable({
          validate(need(ncol(getData()) > 1, "No input data"))

          DT::datatable(getData(), rownames = FALSE)
        })

        output$downloadButton <- downloadHandler(
          filename = function() {
            downloadFilename
          },
          content = function(file) {
            write.csv(getData(), file, row.names = FALSE)
          }
        )
      }
    }
  )
}
