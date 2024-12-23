# Drug Exposure Diagnostics App

A shiny application to view the results of the [DrugExposureDiagnostics](https://github.com/darwin-eu/DrugExposureDiagnostics)

### Requirements

* a directory containing the diagnostics result from the DrugExposureDiagnostics package
* make sure the DATA_DIRECTORY variable in global.R points to your data directory.
The results from each database should be in a zip file in this directory.
Th mock data is from diagnostics about "acetaminophen".
* an environment with R, Rstudio and packages installed.

### Run

The app can be run by opening either server.R, ui.R or global.R and clicking on "Run App" in the top right corner.
