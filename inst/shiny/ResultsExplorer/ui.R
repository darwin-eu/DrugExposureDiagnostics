# User interface definition

allTabsList <- list(
  dataPlotPanelViewer("ingredientConcepts", "Ingredient concepts", byConcept = FALSE),
  widths = c(2, 10)
)

# add tabs depending on if the data is available
if (nrow(drugRoutes) > 0) {
  allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugRoutes", "Drug routes")
}
if (nrow(drugTypes) > 0) {
  allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugTypes", "Drug types")
}
if (nrow(drugSourceConcepts) > 0) {
  allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugSourceConcepts", "Drug source concepts", byConcept = FALSE)
}
if (nrow(drugExposureDuration) > 0) {
  allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugExposureDuration", "Drug exposure duration")
}
if (nrow(drugVariablesMissing) > 0) {
  allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugVariablesMissing", "Drug variables missing", plotPercentage = TRUE)
}
if (nrow(drugDaysSupply) > 0) {
  allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugDaysSupply", "Drug days supply")
}
if (nrow(drugQuantity) > 0) {
  allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugQuantity", "Drug quantity")
}
if (nrow(drugSig) > 0) {
  allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugSig", "Drug sig")
}
if (nrow(drugVerbatimEndDate) > 0) {
  allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugVerbatimEndDate", "Drug verbatim end date")
}
if (nrow(drugDailyDose) > 0) {
  allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugDailyDose", "Drug daily dose")
}
if (nrow(metaData) > 0) {
  allTabsList[[length(allTabsList) + 1]] <- metaDataViewer("metaData", "Metadata")
}

fluidPage(
  theme = bs_theme(version = "5", bootswatch = "spacelab"),
  useShinyjs(),
  titlePanel(
    title = h2("Drug Exposure Diagnostics Dashboard", align = "center"),
    windowTitle = "Drug Exposure Diagnostics Dashboard"
  ),
  do.call(navlistPanel, allTabsList)
)
