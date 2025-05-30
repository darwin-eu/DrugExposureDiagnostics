#' @title dataPlotPanel
#'
#' @include shinyModule.R
#'
#' @description
#' Class to view the data and plot view of a DrugExposureDiagnostics check.
#'
#' @export
dataPlotPanel <- R6::R6Class(
  classname = "dataPlotPanel",
  inherit = ShinyModule,

  # Private ----
  private = list(
    ## Fields
    .data = NULL,
    .dataByConcept = NULL,
    .id = NULL,
    .title = NULL,
    .description = NULL,
    .plotPercentage = TRUE,
    .byConcept = TRUE,
    .selectedColumns = NULL,
    .downloadFilename = NULL,
    .ggplotModules = NULL,
    .ingredients = NULL,
    .databases = NULL,
    .requiredCols = NULL,
    .requiredColsByConcept = NULL,
    .ingredientCols = NULL,

    #' @description
    #' Method to handle the back-end.
    #'
    #' @param input (`input`)\cr
    #' Input from the server function.
    #'
    #' @param output (`output`)\cr
    #' Output from the server function.
    #'
    #' @param session (`session`)\cr
    #' Session from the server function.
    #'
    #' @return (`NULL`)
    .server = function(input, output, session) {
      super$.server(input, output, session)

      if (nrow(private$.data) > 0) {
        databases <- unique(private$.data$database_id)
        ingredients <- private$.data %>%
          dplyr::mutate(ingredient = stringr::str_to_title(ingredient)) %>%
          dplyr::select(all_of(private$.ingredientCols)) %>%
          dplyr::distinct()
        ingredients <- do.call(paste, ingredients)
        columns <- colnames(private$.data)[!colnames(private$.data) %in% private$.requiredCols]

        getData <- reactive({
          result <- NULL
          databases <- input$dbPicker
          ingredients <- input$ingredientPicker
          columns <- input$columnPicker
          if (!is.null(databases) && !is.null(ingredients) && !is.null(columns)) {
            result <- private$.data
            byConcept <- input$byConcept
            dataRequiredCols <- private$.requiredCols
            if (!is.null(byConcept) && byConcept) {
              result <- private$.dataByConcept
              dataRequiredCols <- private$.requiredColsByConcept
            }
            result <- result %>%
              dplyr::select(all_of(c(dataRequiredCols, columns))) %>%
              dplyr::filter(database_id %in% databases) %>%
              dplyr::filter(ingredient_id %in% sub(" .*", "", ingredients))
          }
        })

        getTabData <- reactive({
          data <- getData()
          if (!is.null(data) && private$.id == "drugVariablesMissing") {
            variables <- input$variablesPicker
            data <- data %>%
              dplyr::filter(variable %in% variables)
          }
          return(data)
        })

        # update ingredients when db changes
        observeEvent(input[["dbPicker"]],
          {
            databases <- input$dbPicker
            if (!is.null(databases)) {
              ingredients <- private$getIngredients(databases)
              shinyWidgets::updatePickerInput(
                session = session,
                inputId = shiny::NS(private$.namespace, "ingredientPicker"),
                choices = ingredients,
                selected = ingredients
              )
            }
          },
          ignoreNULL = FALSE,
          ignoreInit = TRUE
        )
        observeEvent(input[["plotDbPicker"]],
          {
            databases <- input$plotDbPicker
            if (!is.null(databases)) {
              ingredients <- private$getIngredients(databases)
              shinyWidgets::updatePickerInput(
                session = session,
                inputId = shiny::NS(private$.namespace, "plotIngredientPicker"),
                choices = ingredients,
                selected = ingredients
              )
            }
          },
          ignoreNULL = FALSE,
          ignoreInit = TRUE
        )

        output$tableDescription <- renderUI({
          byConcept <- ifelse(!is.null(input$byConcept) && input$byConcept, " by concept", "")
          HTML(glue::glue("<center><b>{private$.description}{byConcept}</b></center>"))
        })

        output$dbPickerUI <- renderUI({
          shinyWidgets::pickerInput(
            inputId = shiny::NS(private$.namespace, "dbPicker"),
            label = "Databases",
            choices = private$.databases,
            options = list(`actions-box` = TRUE),
            multiple = T,
            selected = private$.databases
          )
        })

        output$ingredientPickerUI <- renderUI({
          shinyWidgets::pickerInput(
            inputId = shiny::NS(private$.namespace, "ingredientPicker"),
            label = "ingredients",
            choices = private$.ingredients,
            options = list(`actions-box` = TRUE),
            multiple = T,
            selected = private$.ingredients
          )
        })

        output$columnPickerUI <- renderUI({
          shinyWidgets::pickerInput(
            inputId = shiny::NS(private$.namespace, "columnPicker"),
            label = "Columns",
            choices = columns,
            options = list(`actions-box` = TRUE),
            multiple = T,
            selected = private$.selectedColumns
          )
        })

        output$variablesPickerUI <- renderUI({
          if (private$.id == "drugVariablesMissing") {
            data <- getData()
            choices <- unique(data$variable)
            pickerInput(
              inputId = shiny::NS(private$.namespace, "variablesPicker"),
              label = "Variables",
              choices = choices,
              options = list(`actions-box` = TRUE),
              multiple = T,
              selected = choices
            )
          }
        })

        output$downloadButtonUI <- renderUI({
          data <- getTabData()
          if (!is.null(data) && nrow(data) > 0) {
            shinyWidgets::downloadBttn(shiny::NS(private$.namespace, "downloadButton"),
              size = "xs",
              label = "Download"
            )
          }
        })

        output$mainTable <- DT::renderDataTable({
          validate(need(ncol(getTabData()) > 1, "No input data"))

          DT::datatable(getTabData(), rownames = FALSE)
        })

        output$downloadButton <- downloadHandler(
          filename = function() {
            private$.downloadFilename
          },
          content = function(file) {
            write.csv(getTabData(), file, row.names = FALSE)
          }
        )

        # plotting
        output$plotDbPickerUI <- renderUI({
          shinyWidgets::pickerInput(
            inputId = shiny::NS(private$.namespace, "plotDbPicker"),
            label = "Databases",
            choices = private$.databases,
            options = list(`actions-box` = TRUE),
            multiple = T,
            selected = private$.databases
          )
        })

        output$plotIngredientPickerUI <- renderUI({
          shinyWidgets::pickerInput(
            inputId = shiny::NS(private$.namespace, "plotIngredientPicker"),
            label = "Ingredients",
            choices = private$.ingredients,
            options = list(`actions-box` = TRUE),
            multiple = F,
            selected = private$.ingredients
          )
        })

        output$plotVariablesPickerUI <- renderUI({
          if (private$.id == "drugVariablesMissing") {
            data <- getPlotData()
            choices <- unique(data$variable)
            pickerInput(
              inputId = shiny::NS(private$.namespace, "plotVariablesPicker"),
              label = "Variables",
              choices = choices,
              options = list(`actions-box` = TRUE),
              multiple = T,
              selected = choices
            )
          }
        })

        getPlotData <- reactive({
          result <- NULL
          databases <- input$plotDbPicker
          ingredient <- input$plotIngredientPicker
          if (!is.null(databases) && !is.null(ingredient)) {
            result <- private$.data %>%
              dplyr::filter(database_id %in% databases) %>%
              dplyr::filter(ingredient_id == !!private$getIngredientId(ingredient))
          }
          result
        })

        getPlotTabData <- reactive({
          data <- getPlotData()
          if (!is.null(data) && private$.id == "drugVariablesMissing") {
            variables <- input$plotVariablesPicker
            data <- data %>%
              dplyr::filter(variable %in% variables)
          }
          return(data)
        })

        output$plotUI <- renderUI({
          result <- NULL
          # Boxplots and ggplotly do not work together, so we output ggplot for these
          if (private$.id %in% private$.ggplotModules) {
            result <- plotOutput(shiny::NS(private$.namespace, "plot"), height = "700px")
            output$plot <- renderPlot({
              private$createChart(getPlotTabData(), input$top_n)
            })
          } else {
            result <- plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = "700px")
            output$plot <- plotly::renderPlotly({
              plotData <- getPlotTabData()
              if (!is.null(plotData) && nrow(plotData) > 0) {
                plot <- private$createChart(plotData, input$top_n, input$perc)
                plotly::ggplotly(plot, tooltip = c("text"))
              }
            })
          }
          result
        })
      }
    },
    # format a DED check output
    formatData = function(data) {
      if (!is.null(data) && nrow(data) > 0) {
        data <- data %>%
          dplyr::rename(any_of(c(ingredient_id = "ingredient_concept_id"))) %>%
          dplyr::mutate_at(dplyr::vars(starts_with("proportion_")), ~ 100 * .) %>%
          dplyr::rename_with(~ gsub("proportion_", "perc_", .x)) %>%
          dplyr::mutate_at(
            dplyr::vars(which(sapply(., is.numeric) & !names(.) %in% c(names(.)[grepl("id$", names(.))], "n"))),
            ~ floor(.) + signif(. %% 1, 4)
          )
      }
      return(data)
    },

    ## Functions
    createBarChart = function(data, asPercentage = FALSE) {
      if (!is.null(data) && nrow(data) > 0) {
        xLabel <- ifelse(asPercentage, "Percentage", "Count")
        data %>%
          ggplot2::ggplot(ggplot2::aes(x = count, y = variable, fill = ingredient, text = paste0(variable, ": ", count))) +
          ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
            plot.title = ggplot2::element_text(hjust = 0.5)
          ) +
          ggplot2::facet_wrap(. ~ database_id) +
          ggplot2::ggtitle(private$.description) +
          ggplot2::labs(x = xLabel)
      }
    },
    addBoxPlotTheme = function(plot, fontSize = 16) {
      plot +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, size = fontSize),
          axis.text.y = ggplot2::element_text(size = (fontSize - 4)),
          axis.title = ggplot2::element_text(size = fontSize),
          plot.title = ggplot2::element_text(hjust = 0.5, size = fontSize),
          strip.text = ggplot2::element_text(size = fontSize)
        )
    },

    # Box chart for quantiles data
    createBoxChart = function(data) {
      if (!is.null(data) && nrow(data) > 0) {
        private$addBoxPlotTheme(data %>%
          ggplot2::ggplot(ggplot2::aes(x = ingredient, ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100)) +
          ggplot2::geom_boxplot(stat = "identity") +
          ggplot2::facet_wrap(. ~ database_id) +
          ggplot2::ggtitle(private$.description))
      }
    },
    createDoseChart = function(data, colour = "database_id", x = "group_level", facet = "strata_name") {
      result <- NULL
      if (!is.null(data) && nrow(data) > 0) {
        result <- private$addBoxPlotTheme(data %>%
          dplyr::filter(estimate_name != "count_missing") %>%
          dplyr::filter(estimate_name != "percentage_missing") %>%
          dplyr::filter(variable_name == "daily_dose") %>%
          dplyr::mutate(estimate_value = as.integer(estimate_value)) %>%
          private$plotCharacteristics(
            facet = facet,
            colour = colour
          ))
      }
      return(result)
    },
    filterTopN = function(data, topN) {
      result <- data
      if (!is.null(topN) && topN > 0) {
        result <- data %>%
          dplyr::group_by(database_id, ingredient_id) %>%
          dplyr::mutate(count = replace(count, is.na(count), 0)) %>%
          dplyr::slice_max(order_by = count, n = topN, with_ties = TRUE) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(variable = factor(variable, levels = unique(variable[order(count, decreasing = F)])))
      }
      return(result)
    },
    createChart = function(data, topN = NULL, asPercentage = FALSE) {
      if (!is.null(data) && nrow(data) > 0) {
        if (private$.id == "ingredientConcepts") {
          data %>%
            dplyr::rename(variable = drug, count = n_records) %>%
            private$filterTopN(topN) %>%
            private$createBarChart()
        } else if (private$.id == "drugDaysSupply") {
          data %>%
            dplyr::rename(
              y0 = q05_drug_exposure_days_supply,
              y25 = q25_drug_exposure_days_supply,
              y50 = median_drug_exposure_days_supply,
              y75 = q75_drug_exposure_days_supply,
              y100 = q95_drug_exposure_days_supply
            ) %>%
            private$createBoxChart()
        } else if (private$.id == "drugRoutes") {
          data %>%
            dplyr::rename(variable = route_type, count = n_records) %>%
            private$filterTopN(topN) %>%
            private$createBarChart()
        } else if (private$.id == "drugTypes") {
          data %>%
            dplyr::rename(variable = drug_type, count = n_records) %>%
            private$filterTopN(topN) %>%
            private$createBarChart()
        } else if (private$.id == "drugSourceConcepts") {
          data %>%
            dplyr::rename(variable = drug_source, count = n_records) %>%
            dplyr::group_by(database_id, ingredient_id, ingredient, variable) %>%
            dplyr::summarise(count = sum(count), .groups = "drop") %>%
            private$filterTopN(topN) %>%
            private$createBarChart()
        } else if (private$.id == "drugSig") {
          data %>%
            dplyr::rename(variable = sig, count = n_records) %>%
            dplyr::mutate(variable = if_else(is.na(variable), "NA", variable)) %>%
            private$filterTopN(topN) %>%
            private$createBarChart()
        } else if (private$.id == "drugVariablesMissing") {
          columnToUse <- ifelse(asPercentage, "perc_records_missing_value", "n_records_missing_value")
          data %>%
            dplyr::rename(count = columnToUse) %>%
            private$filterTopN(topN) %>%
            private$createBarChart(asPercentage = asPercentage)
        } else if (private$.id == "drugVerbatimEndDate") {
          cols <- colnames(data)[!colnames(data) %in% c(
            "database_id", "ingredient_concept_id", "ingredient_id", "result_obscured",
            "ingredient", "minimum_verbatim_end_date", "maximum_verbatim_end_date",
            "n_records", "n_sample"
          )]
          data %>%
            tidyr::pivot_longer(all_of(cols), names_to = "variable", values_to = "count") %>%
            private$filterTopN(topN) %>%
            private$createBarChart()
        } else if (private$.id == "drugDailyDose") {
          patterns <- data %>% dplyr::pull("pattern_name")
          data %>%
            dplyr::select(-c("pattern_name")) %>%
            omopgenerics::newSummarisedResult() %>%
            dplyr::mutate(pattern_name = patterns) %>%
            dplyr::left_join(omopgenerics::settings(.), by = "result_id") %>%
            private$createDoseChart(colour = "database_id", x = "group_level", facet = "pattern_name")
        } else if (private$.id == "drugExposureDuration") {
          data %>%
            dplyr::rename(
              y0 = q05_drug_exposure_days,
              y25 = q25_drug_exposure_days,
              y50 = median_drug_exposure_days,
              y75 = q75_drug_exposure_days,
              y100 = q95_drug_exposure_days
            ) %>%
            private$createBoxChart()
        } else if (private$.id == "drugQuantity") {
          data %>%
            dplyr::rename(
              y0 = q05_drug_exposure_quantity,
              y25 = q25_drug_exposure_quantity,
              y50 = median_drug_exposure_quantity,
              y75 = q75_drug_exposure_quantity,
              y100 = q95_drug_exposure_quantity
            ) %>%
            private$createBoxChart()
        } else if (private$.id == "drugTimeBetween") {
          data %>%
            dplyr::rename(
              y0 = q05_time_between_days,
              y25 = q25_time_between_days,
              y50 = median_time_between_days,
              y75 = q75_time_between_days,
              y100 = q95_time_between_days
            ) %>%
            private$createBoxChart()
        }
      }
    },
    getIngredientId = function(ingredient) {
      as.numeric(sub(" .*", "", ingredient))
    },
    getIngredients = function(databases = NULL) {
      data <- private$.data
      if (!is.null(databases)) {
        ingredientIds <- data %>%
          dplyr::filter(database_id %in% databases) %>%
          dplyr::pull(ingredient_id)
        data <- data %>%
          dplyr::filter(ingredient_id %in% .env$ingredientIds)
      }
      ingredients <- data %>%
        dplyr::mutate(ingredient = stringr::str_to_title(.data$ingredient)) %>%
        dplyr::select(all_of(private$.ingredientCols)) %>%
        dplyr::distinct()
      do.call(paste, ingredients)
    },
    # Copied from CohortCharacteristics. The new version of this function doesn't work with dose data
    plotCharacteristics = function(data, x = "variable_name", facet = NULL,
                                   colour = NULL, colourName = NULL, .options = list()) {
      result <- NULL
      if (nrow(data) > 0) {
        xAxis <- x
        yAxis <- "estimate_value"
        vertical_x <- FALSE
        nVariableNames <- length(dplyr::pull(dplyr::distinct(dplyr::select(
          data,
          "variable_name"
        ))))
        if (nVariableNames != 1) {
          emptyPlot(
            "Only one variable name can be plotted at a time.",
            "Please filter variable_name column in results before passing to plotCharacteristics()"
          )
        }
        data <- dplyr::mutate(data, estimate_type = dplyr::if_else(.data$estimate_type ==
          "integer", "numeric", .data$estimate_type))
        estimateType <- dplyr::pull(dplyr::distinct(dplyr::select(
          data,
          "estimate_type"
        )))
        nEstimateTypes <- length(estimateType)
        if (nEstimateTypes != 1) {
          private$emptyPlot(
            "Only one estimate type can be plotted at a time.",
            "Please filter estimate_type column in results before passing to plotCharacteristics()"
          )
        }
        if (!estimateType %in% c("numeric", "percentage")) {
          private$emptyPlot(paste0(estimateType, " not currently supported by plotCharacteristics()"))
        }
        gg <- private$plotfunction(data, xAxis, yAxis,
          facetVarX = NULL, facetVarY = NULL,
          colorVars = colour, vertical_x, facet = facet, .options = .options
        )
        gg <- gg + ggplot2::theme_bw()
        if (estimateType == "numeric") {
          var <- unique(data$variable_name)
          if (xAxis == "estimate_value") {
            gg <- gg + ggplot2::ylab(var) + ggplot2::xlab("")
          }
          if (yAxis == "estimate_value") {
            gg <- gg + ggplot2::ylab(var) + ggplot2::xlab("")
          }
        }
        if (estimateType == "percentage") {
          if (xAxis == "estimate_value") {
            gg <- gg + ggplot2::xlab("Percentage") + ggplot2::ylab("")
          }
          if (yAxis == "estimate_value") {
            gg <- gg + ggplot2::ylab("Percentage") + ggplot2::xlab("")
          }
        }
        gg <- gg + ggplot2::theme_bw() + ggplot2::theme(legend.position = "top")
        if (!is.null(colourName)) {
          gg <- gg + ggplot2::labs(color = colourName, fill = colourName)
        } else {
          gg <- gg + ggplot2::labs(color = "", fill = "")
        }
        result <- gg
      }
      return(result)
    },
    plotfunction = function(data,
                            xAxis = "variable_name",
                            yAxis = "estimate_value",
                            plotStyle = "boxplot",
                            facetVarX = "variable_name",
                            facetVarY = c("group_level", "strata_level"),
                            colorVars = "variable_level",
                            vertical_x = FALSE,
                            facet = NULL,
                            .options = list()) {
      errorMessage <- checkmate::makeAssertCollection()
      checkmate::assertTRUE(inherits(data, "summarised_result"))
      all_vars <- c(xAxis, yAxis, facetVarX, facetVarY, colorVars)
      checkmate::assertTRUE(all(all_vars[!is.null(all_vars)] %in% colnames(data)))
      checkmate::assertVector(facetVarX, add = errorMessage, null.ok = TRUE)
      checkmate::assertVector(facetVarY, add = errorMessage, null.ok = TRUE)
      if (nrow(data) == 0) {
        return(ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(title = "Empty Data Provided", subtitle = "No data available for plotting."))
      }
      if (!all(c("q25", "median", "q75", "min", "max") %in% data$estimate_name)) {
        return(
          ggplot2::ggplot() +
            ggplot2::theme_void() +
            ggplot2::labs(
              title = "No Data Provided",
              subtitle = "Boxplot needs to have min max q25 q75 in estimate_name"
            )
        )
      }
      data <- data %>%
        dplyr::mutate(color_combined = private$construct_color_variable(data, colorVars))
      if (is.null(facetVarX)) {
        data$overall <- "overall"
        facetVarX <- "overall"
      }
      if (is.null(facetVarY)) {
        data$overall <- "overall"
        facetVarY <- "overall"
      }
      data <- data %>%
        dplyr::mutate(
          facet_combined_x = private$construct_variable(data, facetVarX),
          facet_combined_y = private$construct_variable(data, facetVarY)
        )
      if (!is.null(facet)) {
        data <- data %>%
          tidyr::unite("facet_var",
            c(dplyr::all_of(.env$facet)),
            remove = FALSE, sep = "; "
          )
      }
      checkmate::assertTRUE(any(xAxis == "estimate_value", yAxis == "estimate_value"), add = errorMessage)
      checkmate::reportAssertions(collection = errorMessage)

      df_dates <- data %>% dplyr::filter(.data$estimate_type == "date")
      df_non_dates <- data %>% dplyr::filter(!(.data$estimate_type %in% c("date", "logical")))

      # Start constructing the plot
      if (nrow(df_non_dates) > 0) {
        df_non_dates <- df_non_dates %>%
          dplyr::filter(.data$estimate_name %in% c("q25", "median", "q75", "min", "max")) %>%
          dplyr::mutate(
            estimate_value = as.numeric(.data$estimate_value),
            estimate_type = "numeric"
          )
        non_numeric_cols <- df_non_dates %>%
          dplyr::select(-c(
            "estimate_value", "estimate_name",
            if ("facet_combined_x" %in% names(df_non_dates)) "facet_combined_x" else NULL,
            if ("facet_combined_y" %in% names(df_non_dates)) "facet_combined_y" else NULL,
            if ("color_combined" %in% names(df_non_dates)) "color_combined" else NULL
          )) %>%
          dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.) > 1)) %>%
          dplyr::select(dplyr::where(~.)) %>%
          names()

        df_non_dates_wide <- df_non_dates %>%
          tidyr::pivot_wider(
            id_cols = dplyr::all_of(colnames(
              df_non_dates %>%
                dplyr::select(-c("estimate_name", "estimate_value"))
            )),
            names_from = "estimate_name",
            values_from = "estimate_value"
          )


        if (length(non_numeric_cols) > 0) {
          df_non_dates_wide$group_identifier <- interaction(df_non_dates_wide %>%
            dplyr::select(dplyr::all_of(non_numeric_cols)))
        } else {
          df_non_dates_wide$group_identifier <- "overall"
        }
      }

      if (nrow(df_dates) > 0) {
        df_dates <- df_dates %>%
          dplyr::filter(.data$estimate_name %in% c("q25", "median", "q75", "min", "max")) %>%
          dplyr::mutate(estimate_value = as.Date(.data$estimate_value))

        df_dates_wide <- df_dates %>%
          tidyr::pivot_wider(
            id_cols = dplyr::all_of(colnames(df_dates %>%
              dplyr::select(-c(
                "estimate_name",
                "estimate_value"
              )))),
            names_from = "estimate_name", values_from = "estimate_value"
          )
        if (length(non_numeric_cols) > 0) {
          df_dates_wide$group_identifier <- interaction(df_dates_wide %>%
            dplyr::select(
              dplyr::all_of(non_numeric_cols)
            ))
        } else {
          df_dates_wide$group_identifier <- "overall"
        }
      }

      # Check if the dataframe has rows to plot
      if (nrow(df_non_dates) > 0) {
        xcol <- ifelse(xAxis == "estimate_value", yAxis, xAxis)
        p_non_dates <- df_non_dates_wide %>% ggplot2::ggplot(
          ggplot2::aes(x = .data[[xcol]])
        )

        if ("color_combined" %in% names(df_non_dates_wide)) {
          if (!all(is.na(df_non_dates_wide$color_combined))) {
            p_non_dates <- p_non_dates + ggplot2::aes(color = .data$color_combined) +
              ggplot2::labs(color = "Color")
          }
        }

        p_non_dates <- p_non_dates + ggplot2::geom_boxplot(
          ggplot2::aes(
            group = .data$group_identifier,
            lower = .data$q25,
            upper = .data$q75,
            middle = .data$median,
            ymin = .data$min,
            ymax = .data$max
          ),
          stat = "identity"
        )

        # Determine if the plot should be horizontal or vertical based on 'estimate_value'
        if (xAxis == "estimate_value") {
          # Horizontal plot
          p_non_dates <- p_non_dates +
            ggplot2::coord_flip()
        }
      } else {
        # Setup for empty data
        p_non_dates <- NULL
      }

      if (nrow(df_dates) > 0) {
        xcol <- ifelse(xAxis == "estimate_value", yAxis, xAxis)

        p_dates <- df_dates_wide %>% ggplot2::ggplot(
          ggplot2::aes(x = .data[[xcol]])
        ) +
          ggplot2::labs(
            title = "Date Data",
            x = "Variable and Group Level",
            y = "Quantile Values"
          )

        if ("color_combined" %in% names(df_dates_wide)) {
          if (!all(is.na(df_dates_wide$color_combined))) {
            p_dates <- p_dates + ggplot2::aes(color = .data$color_combined) +
              ggplot2::labs(color = "Color")
          }
        }

        p_dates <- p_dates + ggplot2::geom_boxplot(
          ggplot2::aes(
            group = .data$group_identifier,
            lower = .data$q25,
            upper = .data$q75,
            middle = .data$median,
            ymin = .data$min,
            ymax = .data$max
          ),
          stat = "identity"
        ) +
          ggplot2::labs(
            title = "Date Data", x = "Variable and Group Level",
            y = "Quantile Values"
          )
        # Determine if the plot should be horizontal or vertical based on 'estimate_value'
        if (xAxis == "estimate_value") {
          # Horizontal plot
          p_dates <- p_dates +
            ggplot2::coord_flip()
        }
      } else {
        p_dates <- NULL
      }

      if (suppressWarnings(!is.null(data$facet_combined_x) || !is.null(data$facet_combined_y))) {
        if (!is.null(p_dates)) {
          facet_x_exists <- "facet_combined_x" %in% names(df_dates)
          facet_y_exists <- "facet_combined_y" %in% names(df_dates)

          # Construct the faceting formula based on the existence of the variables
          facet_formula <- paste0(
            ifelse(facet_y_exists, "facet_combined_y", "."),
            " ~ ",
            ifelse(facet_x_exists, "facet_combined_x", ".")
          )

          p_dates <- p_dates +
            ggplot2::facet_grid(rows = facet_formula, scales = "free")
          if (vertical_x) {
            p_dates <- p_dates + ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            ))
          }
        }
        if (!is.null(p_non_dates)) {
          facet_x_exists <- "facet_combined_x" %in% names(df_non_dates)
          facet_y_exists <- "facet_combined_y" %in% names(df_non_dates)

          # Construct the faceting formula based on the existence of the variables
          facet_formula <- paste0(
            ifelse(facet_y_exists, "facet_combined_y", "."),
            " ~ ",
            ifelse(facet_x_exists, "facet_combined_x", ".")
          )

          p_non_dates <- p_non_dates +
            ggplot2::facet_grid(rows = facet_formula, scales = "free")
          if (vertical_x) {
            p_non_dates <- p_non_dates + ggplot2::theme(
              axis.text.x =
                ggplot2::element_text(
                  angle = 90,
                  hjust = 1,
                  vjust = 0.5
                )
            )
          }
        }

        p <- if (!is.null(p_dates) && !is.null(p_non_dates)) {
          ggpubr::ggarrange(p_dates, p_non_dates, nrow = 2)
        } else if (!is.null(p_dates)) {
          p_dates
        } else if (!is.null(p_non_dates)) {
          p_non_dates
        } else {
          ggplot2::ggplot() +
            ggplot2::theme_void() +
            ggplot2::labs(
              title = "No Data Provided",
              subtitle = "Boxplot needs to have min max q25 q75 in estimate_name"
            )
        }
      } else {
        if (!is.null(p_dates) || !is.null(p_non_dates)) {
          if (!is.null(p_dates) && is.null(p_non_dates)) {
            if (vertical_x) {
              p_dates <- p_dates +
                ggplot2::theme(axis.text.x = ggplot2::element_text(
                  angle = 90,
                  hjust = 1,
                  vjust = 0.5
                ))
            }
            p <- p_dates
          } else if (is.null(p_dates) && !is.null(p_non_dates)) {
            if (vertical_x) {
              p_non_dates <- p_non_dates +
                ggplot2::theme(axis.text.x = ggplot2::element_text(
                  angle = 90,
                  hjust = 1,
                  vjust = 0.5
                ))
            }
            p <- p_non_dates
          } else {
            if (vertical_x) {
              p_dates <- p_dates +
                ggplot2::theme(axis.text.x = ggplot2::element_text(
                  angle = 90,
                  hjust = 1,
                  vjust = 0.5
                ))
              p_non_dates <- p_non_dates +
                ggplot2::theme(axis.text.x = ggplot2::element_text(
                  angle = 90,
                  hjust = 1,
                  vjust = 0.5
                ))
            }
            p <- ggpubr::ggarrange(p_dates, p_non_dates, nrow = 2)
          }
        }
      }

      if (!is.null(facet)) {
        facetNcols <- NULL
        if ("facetNcols" %in% names(.options)) {
          facetNcols <- .options[["facetNcols"]]
        }
        facetScales <- "fixed"
        if ("facetScales" %in% names(.options)) {
          facetScales <- .options[["facetScales"]]
        }
        p <- p +
          ggplot2::facet_wrap(ggplot2::vars(.data$facet_var),
            ncol = facetNcols,
            scales = facetScales
          )
      }
      return(p)
    },
    construct_variable = function(data, facet_vars) {
      if (!is.null(facet_vars) && length(facet_vars) > 1) {
        unique_val_vars <- sapply(facet_vars, function(var) {
          dplyr::n_distinct(data[[var]], na.rm = TRUE) > 1
        })

        valid_vars <- facet_vars[unique_val_vars]

        if (length(valid_vars) > 1) {
          return(as.factor(interaction(data %>% dplyr::select(dplyr::all_of(valid_vars)), sep = ".")))
        } else if (length(valid_vars) == 1) {
          return(as.factor(data[[valid_vars]]))
        }
      } else if (!is.null(facet_vars) && length(facet_vars) == 1) {
        if (dplyr::n_distinct(data[[facet_vars]], na.rm = TRUE) > 1) {
          return(as.factor(data[[facet_vars]]))
        }
      }
      return(NULL)
    },
    construct_color_variable = function(data, color_vars) {
      if (!is.null(color_vars) && length(color_vars) >= 1) {
        combined_factor <- interaction(dplyr::select(
          data,
          dplyr::all_of(color_vars)
        ), sep = ".")
        return(as.factor(combined_factor))
      }
      return(NULL)
    },
    emptyPlot = function(title = "No result to plot", subtitle = "") {
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::labs(
          title = title,
          subtitle = subtitle
        )
    }
  ),

  # Public ----
  public = list(

    #' @description
    #' Initializer method
    #'
    #' @param data data from the `DrugExposureDiagnostics` package.
    #' @param dataByConcept data by drug concept
    #' @param id the unique reference id for the module
    #' @param title panel title
    #' @param description description of data table
    #' @param plotPercentage if plot by percentage should be enabled
    #' @param byConcept add byConcept switch
    #' @param downloadFilename filename of the downloaded file
    #' @param selectedColumns default selected columns
    #'
    #' @returns `self`
    initialize = function(data, dataByConcept = NULL, id, title, description, plotPercentage, byConcept, downloadFilename, selectedColumns = colnames(data)) {
      rlang::check_installed("shiny (>= 1.6.0)")
      super$initialize()
      private$.data <- private$formatData(data)
      private$.dataByConcept <- private$formatData(dataByConcept)
      private$.id <- id
      private$.title <- title
      private$.description <- description
      private$.plotPercentage <- plotPercentage
      private$.byConcept <- byConcept
      private$.downloadFilename <- downloadFilename
      private$.selectedColumns <- selectedColumns
      private$.databases <- unique(private$.data$database_id)
      private$.requiredCols <- c("database_id", "ingredient_id", "ingredient")
      private$.requiredColsByConcept <- c(private$.requiredCols, "drug_concept_id", "drug")
      private$.ingredientCols <- private$.requiredCols[2:3]
      private$.ingredients <- private$getIngredients()
      private$.ggplotModules <- c("drugDailyDose", "drugQuantity", "drugExposureDuration", "drugDaysSupply", "drugTimeBetween")
      return(invisible(self))
    },

    #' @description
    #' Method to include a \link[shiny]{tabPanel} to include the body.
    #'
    #' @return (`tabItem`)
    uiBody = function() {
      filterRow <- shiny::fluidRow(
        shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "dbPickerUI"))),
        shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "ingredientPickerUI"))),
        shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "columnPickerUI")))
      )

      topNCol <- shiny::tagList()
      if (!private$.id %in% private$.ggplotModules) {
        topNCol <- shiny::column(width = 2, shiny::numericInput(shiny::NS(private$.namespace, "top_n"), label = "Top n:", 20, min = 1, max = 100))
      }

      plotFilterRow <- shiny::fluidRow(
        shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "plotDbPickerUI"))),
        shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "plotIngredientPickerUI"))),
        topNCol
      )

      if (private$.plotPercentage) {
        if (private$.id == "drugVariablesMissing") {
          plotFilterRow <- shiny::fluidRow(
            shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "plotDbPickerUI"))),
            shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "plotIngredientPickerUI"))),
            shiny::column(width = 2, shiny::uiOutput(shiny::NS(private$.namespace, "plotVariablesPickerUI"))),
            topNCol,
            shiny::column(width = 2, shiny::tags$br(), shiny::tags$br(), shiny::checkboxInput(shiny::NS(private$.namespace, "perc"), label = "Percentage", value = TRUE))
          )
        } else {
          plotFilterRow <- shiny::fluidRow(
            shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "plotDbPickerUI"))),
            shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "plotIngredientPickerUI"))),
            topNCol,
            shiny::column(width = 2, shiny::tags$br(), shiny::tags$br(), shiny::checkboxInput(shiny::NS(private$.namespace, "perc"), label = "Percentage", value = TRUE))
          )
        }
      }

      if (private$.byConcept) {
        if (private$.id == "drugVariablesMissing") {
          filterRow <- shiny::fluidRow(
            shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "dbPickerUI"))),
            shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "ingredientPickerUI"))),
            shiny::column(width = 2, shiny::uiOutput(shiny::NS(private$.namespace, "columnPickerUI"))),
            shiny::column(width = 2, shiny::uiOutput(shiny::NS(private$.namespace, "variablesPickerUI"))),
            shiny::column(
              width = 2,
              shiny::tags$br(), shiny::tags$br(),
              shinyWidgets::prettyCheckbox(
                inputId = shiny::NS(private$.namespace, "byConcept"),
                label = "By concept",
                value = FALSE
              )
            )
          )
        } else {
          filterRow <- shiny::fluidRow(
            shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "dbPickerUI"))),
            shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "ingredientPickerUI"))),
            shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "columnPickerUI"))),
            shiny::column(
              width = 3,
              shiny::tags$br(), shiny::tags$br(),
              shinyWidgets::prettyCheckbox(
                inputId = shiny::NS(private$.namespace, "byConcept"),
                label = "By concept",
                value = FALSE
              )
            )
          )
        }
      }
      shiny::tabPanel(
        private$.title,
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Data",
            filterRow,
            shiny::fluidRow(shiny::column(width = 12, shiny::uiOutput(shiny::NS(private$.namespace, "tableDescription")))),
            shiny::tags$hr(),
            shinycssloaders::withSpinner(DT::dataTableOutput(shiny::NS(private$.namespace, "mainTable"))),
            shiny::tags$hr(),
            shiny::uiOutput(shiny::NS(private$.namespace, "downloadButtonUI"))
          ),
          shiny::tabPanel(
            "Plot",
            plotFilterRow,
            shiny::fluidRow(shiny::column(width = 12, shiny::uiOutput(shiny::NS(private$.namespace, "plotDescription")))),
            shiny::tags$hr(),
            shinycssloaders::withSpinner(shiny::uiOutput(shiny::NS(private$.namespace, "plotUI"), height = "700px"))
          )
        )
      )
    }
  )
)
