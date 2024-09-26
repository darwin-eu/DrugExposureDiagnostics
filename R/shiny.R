#' View the results in the Shiny app
#'
#' @param dataFolder A folder where the exported zip files with the results are stored.
#'                   Zip files containing results from multiple databases can be placed in the same
#'                   folder.
#' @param makePublishable     (Optional) copy data files to make app publishable to posit connect/shinyapp.io
#' @param publishDir          If make publishable is true - the directory that the shiny app is copied to
#' @param overwritePublishDir (Optional) If make publishable is true - overwrite the directory for publishing

#' @param launch.browser Should the app be launched in your default browser, or in a Shiny window.
#'                       Note: copying to clipboard will not work in a Shiny window.
#'
#' @details
#' Launches a Shiny app that allows the user to explore the diagnostics
#'
#' @export
viewResults <- function(dataFolder,
                        makePublishable = FALSE,
                        publishDir = file.path(getwd(), "ResultsExplorer"),
                        overwritePublishDir = FALSE,
                        launch.browser = FALSE) {

  appDir <- system.file("shiny", "ResultsExplorer", package = "DrugExposureDiagnostics")
  shinySettings <- list(dataFolder = dataFolder)
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm(shinySettings, envir = .GlobalEnv))

  if (makePublishable) {
    if (dir.exists(publishDir) && !overwritePublishDir) {
      warning("Directory for publishing exists, use overwritePublishDir to overwrite")
    } else {
      if (getwd() == publishDir) {
        stop("Publishable dir should not be current working directory")
      }

      # create publish and data dir
      dataPath <- "data"
      dir.create(file.path(publishDir, dataPath), showWarnings = FALSE, recursive = TRUE)

      # copy app & data
      appFiles <- file.path(appDir, list.files(appDir))
      appFiles <- appFiles[!grepl("tests$|data$", appFiles)]
      file.copy(appFiles, publishDir, recursive = TRUE, overwrite = TRUE)
      dataFiles <- file.path(dataFolder, list.files(dataFolder))
      file.copy(dataFiles, file.path(publishDir, dataPath), recursive = TRUE, overwrite = TRUE)
    }
    appDir <- publishDir
  }
  if (launch.browser) {
    options(shiny.launch.browser = TRUE)
  }

  shiny::runApp(appDir = appDir)
}
