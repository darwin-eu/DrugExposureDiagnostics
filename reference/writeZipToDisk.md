# Write (ingredient) diagnostics results on disk in given output folder.

Write (ingredient) diagnostics results on disk in given output folder.

## Usage

``` r
writeZipToDisk(metadata, databaseId, outputFolder, filename = NULL)
```

## Arguments

- metadata:

  metadata results

- databaseId:

  database identifier

- outputFolder:

  folder to write to

- filename:

  output filename for the zip file

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
