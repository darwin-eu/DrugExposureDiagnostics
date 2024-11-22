# Module to filter data and display data and a plot

# @file tableFilterPanel
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
#' @param byConcept add byConcept switch
#'
#' @return
#' The user interface to the results
#'
#' @export
dataPlotPanelViewer <- function(id, title, byConcept = TRUE, plotPercentage = FALSE) {
  ns <- shiny::NS(id)
  filterRow <- fluidRow(
    column(width = 3, uiOutput(ns("dbPickerUI"))),
    column(width = 3, uiOutput(ns("ingredientPickerUI"))),
    column(width = 3, uiOutput(ns("columnPickerUI")))
  )

  topNCol <- tagList()
  if (!id %in% ggplotModules) {
    topNCol <- column(width = 3, numericInput(ns("top_n"), label = "Top n:", 20, min = 1, max = 100))
  }

  plotFilterRow <- fluidRow(
    column(width = 3, uiOutput(ns("plotDbPickerUI"))),
    column(width = 3, uiOutput(ns("plotIngredientPickerUI"))),
    topNCol
  )

  if (plotPercentage) {
    plotFilterRow <- fluidRow(
      column(width = 3, uiOutput(ns("plotDbPickerUI"))),
      column(width = 3, uiOutput(ns("plotIngredientPickerUI"))),
      topNCol,
      column(width = 3, tags$br(), tags$br(), checkboxInput(ns("perc"), label = "Percentage", value = TRUE))
    )
  }

  if (byConcept) {
    filterRow <- fluidRow(
      column(width = 3, uiOutput(ns("dbPickerUI"))),
      column(width = 3, uiOutput(ns("ingredientPickerUI"))),
      column(width = 3, uiOutput(ns("columnPickerUI"))),
      column(
        width = 3,
        tags$br(), tags$br(),
        prettyCheckbox(
          inputId = ns("byConcept"),
          label = "By concept",
          value = FALSE
        )
      )
    )
  }
  tabPanel(
    title,
    tabsetPanel(
      tabPanel(
        "Data",
        filterRow,
        fluidRow(column(width = 12, uiOutput(ns("tableDescription")))),
        tags$hr(),
        withSpinner(DT::dataTableOutput(ns("mainTable"))),
        tags$hr(),
        uiOutput(ns("downloadButtonUI"))
      ),
      tabPanel(
        "Plot",
        plotFilterRow,
        fluidRow(column(width = 12, uiOutput(ns("plotDescription")))),
        tags$hr(),
        withSpinner(uiOutput(ns("plotUI"), height = "700px"))
      )
    )
  )
}

#' The module server for rendering the results
#'
#' @param id the unique reference id for the module
#' @param data the data
#' @param dataByConcept the data
#' @param downloadFilename filename of the table data that can be downloaded
#' @param description table description
#' @param commonInputs common inputs
#' @param selectedColumns selected columns, by default NULL which means all columns are shown
#'
#' @return the results table server
#'
#' @export
dataPlotPanelServer <- function(id, data, dataByConcept, downloadFilename, description, commonInputs, selectedColumns = NULL) {
  ns <- shiny::NS(id)

  createBarChart <- function(data, x = "count", y = "variable", fill = "ingredient", xLabel = "count") {
    if (!is.null(data) && nrow(data) > 0) {
      data %>%
        ggplot(aes_string(x = x, y = y, fill = fill)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          plot.title = element_text(hjust = 0.5)
        ) +
        facet_wrap(. ~ database_id) +
        ggtitle(description) +
        labs(x = xLabel)
    }
  }

  addBoxPlotTheme <- function(plot, fontSize = 16) {
    plot +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 0.5, size = fontSize),
        axis.text.y = element_text(size = (fontSize - 4)),
        axis.title = element_text(size = fontSize),
        plot.title = element_text(hjust = 0.5, size = fontSize),
        strip.text = element_text(size = fontSize)
      )
  }
  # Box chart for quantiles data
  createBoxChart <- function(data) {
    if (!is.null(data) && nrow(data) > 0) {
      addBoxPlotTheme(data %>%
        ggplot(aes(x = ingredient, ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100)) +
        geom_boxplot(stat = "identity") +
        facet_wrap(. ~ database_id) +
        ggtitle(description))
    }
  }

  createDoseChart <- function(data, colour = "database_id", x = "group_level", facet = "strata_name") {
    result <- NULL
    if (!is.null(data) && nrow(data) > 0) {
      result <- addBoxPlotTheme(data %>%
        dplyr::filter(estimate_name != "count_missing") %>%
        dplyr::filter(estimate_name != "percentage_missing") %>%
        dplyr::filter(variable_name == "daily_dose") %>%
        dplyr::mutate(estimate_value = as.integer(estimate_value)) %>%
        plotCharacteristics(
          plotStyle = "boxplot",
          colour = colour,
          x = x,
          facet = facet
        ))
    }
    return(result)
  }

  filterTopN <- function(data, topN) {
    result <- data
    if (!is.null(topN)) {
      result <- data %>%
        group_by(database_id, ingredient_id) %>%
        slice_max(order_by = count, n = topN, with_ties = TRUE) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(variable = factor(variable, levels = unique(variable[order(count, decreasing = F)])))
    }
    return(result)
  }

  createChart <- function(data, topN = NULL, asPercentage = FALSE) {
    if (!is.null(data) && nrow(data) > 0) {
      if (id == "ingredientConcepts") {
        data %>%
          dplyr::rename(variable = drug, count = n_records) %>%
          filterTopN(topN) %>%
          createBarChart()
      } else if (id == "drugDaysSupply") {
        data %>%
          dplyr::rename(
            y0 = minimum_drug_exposure_days_supply,
            y25 = q25_drug_exposure_days_supply,
            y50 = median_drug_exposure_days_supply,
            y75 = q75_drug_exposure_days_supply,
            y100 = maximum_drug_exposure_days_supply
          ) %>%
          createBoxChart()
      } else if (id == "drugRoutes") {
        data %>%
          dplyr::rename(variable = route_type, count = n_records) %>%
          createBarChart()
      } else if (id == "drugTypes") {
        data %>%
          dplyr::rename(variable = drug_type, count = n_records) %>%
          createBarChart()
      } else if (id == "drugSourceConcepts") {
        data %>%
          dplyr::rename(variable = drug_source, count = n_records) %>%
          dplyr::group_by(database_id, ingredient_id, ingredient, variable) %>%
          dplyr::summarise(count = sum(count), .groups = "drop") %>%
          filterTopN(topN) %>%
          createBarChart()
      } else if (id == "drugSig") {
        data %>%
          dplyr::rename(variable = sig, count = n_records) %>%
          filterTopN(topN) %>%
          createBarChart()
      } else if (id == "drugVariablesMissing") {
        columnToUse <- ifelse(asPercentage, "perc_records_missing_value", "n_records_missing_value")
        xLabel <- ifelse(asPercentage, "Percentage", "Count")
        data %>%
          dplyr::rename(count = columnToUse) %>%
          filterTopN(topN) %>%
          createBarChart(xLabel = xLabel)
      } else if (id == "drugVerbatimEndDate") {
        cols <- colnames(data)[!colnames(data) %in% c(
          "database_id", "ingredient_concept_id", "ingredient_id", "result_obscured",
          "ingredient", "minimum_verbatim_end_date", "maximum_verbatim_end_date",
          "n_records", "n_sample"
        )]
        data %>%
          tidyr::pivot_longer(all_of(cols), names_to = "variable", values_to = "count") %>%
          filterTopN(topN) %>%
          createBarChart()
      } else if (id == "drugDailyDose") {
        patterns <- data %>% pull("pattern_name")
        data %>%
          dplyr::select(-c("pattern_name")) %>%
          omopgenerics::newSummarisedResult() %>%
          dplyr::mutate(pattern_name = patterns) %>%
          dplyr::left_join(omopgenerics::settings(.), by = "result_id") %>%
          createDoseChart(colour = "database_id", x = "group_level", facet = "pattern_name")
      } else if (id == "drugExposureDuration") {
        data %>%
          dplyr::rename(
            y0 = minimum_drug_exposure_days,
            y25 = q25_drug_exposure_days,
            y50 = median_drug_exposure_days,
            y75 = q75_drug_exposure_days,
            y100 = maximum_drug_exposure_days
          ) %>%
          createBoxChart()
      } else if (id == "drugQuantity") {
        data %>%
          dplyr::rename(
            y0 = minimum_drug_exposure_quantity,
            y25 = q25_drug_exposure_quantity,
            y50 = median_drug_exposure_quantity,
            y75 = q75_drug_exposure_quantity,
            y100 = maximum_drug_exposure_quantity
          ) %>%
          createBoxChart()
      }
    }
  }

  getIngredientId <- function(ingredient) {
    as.numeric(sub(" .*", "", ingredient))
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {
      requiredCols <- c("database_id", "ingredient_id", "ingredient")
      requiredColsByConcept <- c(requiredCols, "drug_concept_id", "drug")
      ingredientCols <- requiredCols[2:3]
      referenceTabId <- "ingredientConcepts"

      if (nrow(data) > 0) {
        databases <- unique(data$database_id)
        ingredients <- data %>%
          dplyr::mutate(ingredient = str_to_title(ingredient)) %>%
          dplyr::select(all_of(ingredientCols)) %>%
          dplyr::distinct()
        ingredients <- do.call(paste, ingredients)
        columns <- colnames(data)[!colnames(data) %in% requiredCols]

        getData <- reactive({
          result <- NULL
          databases <- input$dbPicker
          ingredients <- input$ingredientPicker
          columns <- input$columnPicker
          if (!is.null(databases) && !is.null(ingredients) && !is.null(columns)) {
            result <- data
            byConcept <- input$byConcept
            dataRequiredCols <- requiredCols
            if (!is.null(byConcept) && byConcept) {
              result <- dataByConcept
              dataRequiredCols <- requiredColsByConcept
            }
            result <- result %>%
              select(all_of(c(dataRequiredCols, columns))) %>%
              filter(database_id %in% databases) %>%
              filter(ingredient_id %in% sub(" .*", "", ingredients))
          }
        })

        observeEvent(input[["ingredientPicker"]],
          {
            if (id == referenceTabId) {
              commonInputs$ingredients <- input$ingredientPicker
            }
          },
          ignoreNULL = FALSE,
          ignoreInit = TRUE
        )

        # update ingredients when db changes
        observeEvent(input[["dbPicker"]],
          {
            databases <- input$dbPicker
            if (id == referenceTabId) {
              commonInputs$databases <- databases
              if (!is.null(databases)) {
                ingredientIds <- data %>%
                  filter(database_id %in% databases) %>%
                  pull(ingredient_id)

                ingredients <- data %>%
                  dplyr::filter(ingredient_id %in% .env$ingredientIds) %>%
                  dplyr::mutate(ingredient = str_to_title(.data$ingredient)) %>%
                  dplyr::select(all_of(ingredientCols)) %>%
                  dplyr::distinct()

                ingredients <- do.call(paste, ingredients)
                updatePickerInput(
                  session = session,
                  inputId = "ingredientPicker",
                  choices = ingredients,
                  selected = ingredients
                )
              }
            }
          },
          ignoreNULL = FALSE,
          ignoreInit = TRUE
        )

        observe({
          req(commonInputs)
          if (id != referenceTabId) {
            if (!identical(commonInputs$ingredients, commonInputsInitValue)) {
              ingredients <- commonInputs$ingredients
              updatePickerInput(
                session = session,
                inputId = "ingredientPicker",
                selected = ingredients
              )
            }

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
          byConcept <- ifelse(!is.null(input$byConcept) && input$byConcept, " by concept", "")
          HTML(glue::glue("<center><b>{description}{byConcept}</b></center>"))
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

        output$ingredientPickerUI <- renderUI({
          if (id != referenceTabId && !identical(commonInputs$ingredients, commonInputsInitValue)) {
            ingredients <- commonInputs$ingredients
          }
          pickerInput <- pickerInput(
            inputId = ns("ingredientPicker"),
            label = "ingredients",
            choices = ingredients,
            options = list(`actions-box` = TRUE),
            multiple = T,
            selected = ingredients
          )
          if (id != referenceTabId) {
            pickerInput <- disabled(pickerInput)
          }
          pickerInput
        })

        output$columnPickerUI <- renderUI({
          if (is.null(selectedColumns)) {
            selectedColumns <- columns
          }
          pickerInput(
            inputId = ns("columnPicker"),
            label = "Columns",
            choices = columns,
            options = list(`actions-box` = TRUE),
            multiple = T,
            selected = selectedColumns
          )
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

        # plotting
        output$plotDbPickerUI <- renderUI({
          databases <- commonInputs$databases
          pickerInput <- pickerInput(
            inputId = ns("plotDbPicker"),
            label = "Databases",
            choices = databases,
            options = list(`actions-box` = TRUE),
            multiple = T,
            selected = databases
          )
          pickerInput <- disabled(pickerInput)
          pickerInput
        })

        output$plotIngredientPickerUI <- renderUI({
          pickerInput(
            inputId = ns("plotIngredientPicker"),
            label = "Ingredients",
            choices = ingredients,
            options = list(`actions-box` = TRUE),
            multiple = F,
            selected = ingredients
          )
        })


        getPlotData <- reactive({
          result <- NULL
          databases <- input$plotDbPicker
          ingredient <- input$plotIngredientPicker
          if (!is.null(databases) && !is.null(ingredient)) {
            result <- data
            result <- result %>%
              filter(database_id %in% databases) %>%
              filter(ingredient_id == getIngredientId(.env$ingredient))
          }
          result
        })

        output$plotUI <- renderUI({
          result <- NULL
          # Boxplots and ggplotly do not work together, so we output ggplot for these
          if (id %in% ggplotModules) {
            result <- plotOutput(ns("plot"), height = "700px")
            output$plot <- renderPlot({
              createChart(getPlotData(), input$top_n)
            })
          } else {
            result <- plotlyOutput(ns("plot"), height = "700px")
            output$plot <- renderPlotly({
              createChart(getPlotData(), input$top_n, input$perc)
            })
          }
          result
        })
      }
    }
  )
}
