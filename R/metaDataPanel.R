#' @title metaDataPanel
#'
#' @include shinyModule.R
#'
#' @description
#' Class to view the metadata of a DrugExposureDiagnostics execution.
#'
#' @export
metaDataPanel <- R6::R6Class(
  classname = "metaDataPanel",
  inherit = ShinyModule,

  # Private ----
  private = list(
    ## Fields
    .data = NULL,
    .id = NULL,
    .title = NULL,
    .description = NULL,
    .downloadFilename = NULL,
    .requiredCols = NULL,
    .databases = NULL,

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
        columns <- colnames(private$.data)[!colnames(private$.data) %in% private$.requiredCols]

        getData <- reactive({
          result <- NULL
          databases <- input$dbPicker
          if (!is.null(databases)) {
            result <- private$.data %>%
              dplyr::filter(database_id %in% databases)
          }
        })

        output$tableDescription <- renderUI({
          HTML(glue::glue("<center><b>{private$.description}</b></center>"))
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

        output$downloadButtonUI <- renderUI({
          data <- getData()
          if (!is.null(data) && nrow(data) > 0) {
            shinyWidgets::downloadBttn(shiny::NS(private$.namespace, "downloadButton"),
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
            private$.downloadFilename
          },
          content = function(file) {
            write.csv(getData(), file, row.names = FALSE)
          }
        )
      }
    }
  ),
  # Public ----
  public = list(

    #' @description
    #' Initializer method
    #'
    #' @param data data from the `DrugExposureDiagnostics` package.
    #' @param id the unique reference id for the module
    #' @param title panel title
    #' @param description description of data table
    #' @param downloadFilename filename of the downloaded file
    #'
    #' @returns `self`
    initialize = function(data, id, title, description, downloadFilename) {
      rlang::check_installed("shiny (>= 1.6.0)")
      super$initialize()
      private$.data <- data
      private$.id <- id
      private$.title <- title
      private$.description <- description
      private$.downloadFilename <- downloadFilename
      private$.requiredCols <- c("database_id")
      private$.databases <- unique(private$.data$database_id)
      return(invisible(self))
    },

    #' @description
    #' Method to include a \link[shiny]{tabPanel} to include the body.
    #'
    #' @return (`tabItem`)
    uiBody = function() {
      filterRow <- shiny::fluidRow(
        shiny::column(width = 3, shiny::uiOutput(shiny::NS(private$.namespace, "dbPickerUI")))
      )
      shiny::tabPanel(
        private$.title,
        filterRow,
        shiny::fluidRow(shiny::column(width = 12, shiny::uiOutput(shiny::NS(private$.namespace, "tableDescription")))),
        shiny::tags$hr(),
        shinycssloaders::withSpinner(DT::dataTableOutput(shiny::NS(private$.namespace, "mainTable"))),
        shiny::tags$hr(),
        shiny::uiOutput(shiny::NS(private$.namespace, "downloadButtonUI"))
      )
    }
  )
)
