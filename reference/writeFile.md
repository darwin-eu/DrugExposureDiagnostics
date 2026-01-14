# Write a result to a file on disk.

Write a result to a file on disk.

## Usage

``` r
writeFile(result, resultName, databaseId, dbDir)
```

## Arguments

- result:

  check result

- resultName:

  name of the result

- databaseId:

  database identifier

- dbDir:

  output directory for current db

## Value

No return value, called for side effects

## Examples

``` r
if (FALSE) { # \dontrun{
resultList <- list("mtcars" = mtcars)
result <- writeZipToDisk(
  metadata = metadata,
  databaseId = "mtcars",
  outputFolder = here::here()
)
} # }
```
