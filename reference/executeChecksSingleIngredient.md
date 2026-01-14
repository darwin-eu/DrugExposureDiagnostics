# Execute given checks on Drug Exposure for a single ingredient.

Execute given checks on Drug Exposure for a single ingredient.

## Usage

``` r
executeChecksSingleIngredient(
  cdm,
  ingredient = 1125315,
  subsetToConceptId = NULL,
  checks = c("missing", "exposureDuration", "quantity"),
  minCellCount = 5,
  sampleSize = 10000,
  tablePrefix = NULL,
  earliestStartDate = "2010-01-01",
  verbose = FALSE,
  byConcept = FALSE,
  exposureTypeId = NULL
)
```

## Arguments

- cdm:

  CDMConnector reference object

- ingredient:

  ingredient, by default: acetaminophen

- subsetToConceptId:

  vector of concept IDs of the ingredients to filter. If a concept ID is
  positive it will be included, a negative one will be excluded. If
  NULL, all concept IDs for an ingredient will be considered.

- checks:

  the checks to be executed, by default the missing values, the exposure
  duration and the quantity.

- minCellCount:

  minimum number of events to report- results lower than this will be
  obscured. If 0 all results will be reported.

- sampleSize:

  the number of samples, default 10.000

- tablePrefix:

  The stem for the permanent tables that will be created when running
  the diagnostics. Permanent tables will be created using this prefix,
  and any existing tables that start with this will be at risk of being
  dropped or overwritten. If NULL, temporary tables will be used
  throughout.

- earliestStartDate:

  the earliest date from which a record can be included

- verbose:

  verbose, default FALSE

- byConcept:

  boolean argument whether to return results by Concept or overall only

- exposureTypeId:

  id of the drug exposure type to be filtered on (e.g. only prescribed).
  By default all record types will be taken into account.

## Value

named list with results
