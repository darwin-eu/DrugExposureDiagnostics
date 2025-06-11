#' @title Module Decorator Class
ShinyModule <- R6::R6Class(
  classname = "ShinyModule",

  # Active ----
  active = list(
    #' @field instanceId (`character(1)`) Random ID
    instanceId = function(instanceId) {
      if (missing(instanceId)) {
        return(private$.instanceId)
      } else {
        checkmate::assertCharacter(x = instanceId, len = 1)
        private$.instanceId <- instanceId
        private$.moduleId <- paste(c(private$.moduleName, private$.instanceId), collapse = "-")
        private$.namespace <- paste(c(private$.parentNamespace, private$.moduleId), collapse = "-")
        return(invisible(self))
      }
    },

    #' @field parentNamespace (`character(1)`) Namespace parent module
    parentNamespace = function(parentNamespace) {
      if (missing(parentNamespace)) {
        return(private$.parentNamespace)
      } else {
        checkmate::assertCharacter(x = parentNamespace, len = 1, null.ok = TRUE)
        private$.parentNamespace <- parentNamespace
        private$.namespace <- paste(c(private$.parentNamespace, private$.moduleId), collapse = "-")
        return(invisible(self))
      }
    },

    #' @field moduleName (`character(1)`) Module name
    moduleName = function() {
      return(private$.moduleName)
    },

    #' @field moduleId (`character(1)`) Module id
    #' `moduleName-instanceId`
    moduleId = function() {
      return(private$.moduleId)
    },

    #' @field namespace (`character(1)`) Namespace, composed like:
    #' `[parentNamespace-]moduleName-instanceId` where `parentNamespace` is
    #' optional
    namespace = function() {
      return(private$.namespace)
    },

    #' @field reactiveValues (`reactivevalues`) Reactive values. use
    #' `shiny::isolate()` to get a non-reactive item from the reactive
    #' environment.
    reactiveValues = function() {
      return(private$.reactiveValues)
    }
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @return
    #' (`self`)
    initialize = function() {
      private$checkMethodOverrides()
      private$.moduleName <- class(self)[1]
      private$.instanceId <- private$makeInstanceId()
      private$.moduleId <- sprintf("%s-%s", private$.moduleName, private$.instanceId)
      private$.namespace <- c(private$.parentNamespace, private$.moduleId)
      return(invisible(self))
    },

    #' @description
    #' Validator method
    #'
    #' @return
    #' (`self`)
    validate = function() {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(
        .var.name = "instanceId",
        x = private$.instanceId,
        len = 1,
        add = assertions
      )

      checkmate::assertCharacter(
        .var.name = "parentNamespace",
        x = private$.parentNamespace,
        len = 1,
        null.ok = TRUE,
        add = assertions
      )

      checkmate::assertCharacter(
        .var.name = "moduleName",
        x = private$.moduleName,
        len = 1,
        add = assertions
      )

      checkmate::assertCharacter(
        .var.name = "moduleId",
        x = private$.moduleId,
        len = 1,
        add = assertions
      )

      checkmate::reportAssertions(assertions)
      return(invisible(self))
    },

    #' @description
    #' Method to include a \link[shiny]{tagList} to include the body.
    #'
    #' @return
    #' (`tagList`)
    UI = function() {
      private$.UI()
    },

    #' @description
    #' Method to handle the back-end.
    #'
    #' @param input (`input`) Input from the server function.
    #' @param output (`output`) Output from the server function.
    #' @param session (`session`) Session from the server function.
    #'
    #' @return
    #' (`NULL`)
    server = function(input, output, session) {
      shiny::moduleServer(id = self$moduleId, module = function(input, output, session) {
        private$.init()
        private$.server(input, output, session)
      })
      return(NULL)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .moduleName = "",
    .instanceId = "",
    .moduleId = "",
    .parentNamespace = NULL,
    .namespace = "",
    .reactiveValues = NULL,

    ## Methods ----
    .init = function() {
      private$.reactiveValues <- shiny::reactiveValues()
      return(invisible(self))
    },

    .server = function(input, output, session) {},

    .UI = function(input, output, session) {},

    finalize = function() {
      return(NULL)
    },

    makeInstanceId = function(n = 20) {
      items <- c(letters, LETTERS, c(1:9), c("_"))
      paste0(sample(x = items, size = n), collapse = "")
    },

    checkMethodOverrides = function() {
      if (!is.null(self$.__enclos_env__$super)) {
        serverErr <- if (!identical(self$.__enclos_env__$super$server, self$server)) {
          "`self$server()` was overridden in `public = list(...)` override `private$.server()` instead in `private = list(.server = function(input, output, session) {})`"
        }

        uiErr <- if (!identical(self$.__enclos_env__$super$UI, self$UI)) {
          "`self$UI()` was overridden in `public = list(...)` override `private$.UI()` instead in `private = list(.UI = function() {})`"
        }

        if (any(!is.null(c(serverErr, uiErr)))) {
          stop(c(serverErr, "\n  ", uiErr))
        }
      }
    }
  )
)
