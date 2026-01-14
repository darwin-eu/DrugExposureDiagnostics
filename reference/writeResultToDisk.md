# Write diagnostics results to a zip file on disk in given output folder.

Write diagnostics results to a zip file on disk in given output folder.

## Usage

``` r
writeResultToDisk(resultList, databaseId, outputFolder, filename = NULL)
```

## Arguments

- resultList:

  named list with results

- databaseId:

  database identifier

- outputFolder:

  folder to write to

- filename:

  output filename, if NULL it will be equal to databaseId

## Value

No return value, called for side effects

## Examples

``` r
if (FALSE) { # \dontrun{
resultList <- list("mtcars" = mtcars)
result <- writeResultToDisk(
  resultList = resultList,
  databaseId = "mtcars",
  outputFolder = here::here()
)
} # }
```
