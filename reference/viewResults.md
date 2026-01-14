# View the results in the Shiny app

View the results in the Shiny app

## Usage

``` r
viewResults(
  dataFolder,
  makePublishable = FALSE,
  publishDir = file.path(getwd(), "ResultsExplorer"),
  overwritePublishDir = FALSE,
  launch.browser = FALSE
)
```

## Arguments

- dataFolder:

  A folder where the exported zip files with the results are stored. Zip
  files containing results from multiple databases can be placed in the
  same folder.

- makePublishable:

  (Optional) copy data files to make app publishable to posit
  connect/shinyapp.io

- publishDir:

  If make publishable is true - the directory that the shiny app is
  copied to

- overwritePublishDir:

  (Optional) If make publishable is true - overwrite the directory for
  publishing

- launch.browser:

  Should the app be launched in your default browser, or in a Shiny
  window. Note: copying to clipboard will not work in a Shiny window.

## Details

Launches a Shiny app that allows the user to explore the diagnostics
