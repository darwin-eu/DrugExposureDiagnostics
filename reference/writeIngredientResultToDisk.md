# Write (ingredient) diagnostics results on disk in given output folder.

Write (ingredient) diagnostics results on disk in given output folder.

## Usage

``` r
writeIngredientResultToDisk(
  resultList,
  databaseId,
  outputFolder,
  clearDBDir = FALSE
)
```

## Arguments

- resultList:

  named list with results

- databaseId:

  database identifier

- outputFolder:

  folder to write to

- clearDBDir:

  if database directory should be cleared

## Value

No return value, called for side effects

## Examples

``` r
if (FALSE) { # \dontrun{
resultList <- list("mtcars" = mtcars)
result <- writeIngredientResultToDisk(
  resultList = resultList,
  databaseId = "mtcars",
  outputFolder = here::here()
)
} # }
```
