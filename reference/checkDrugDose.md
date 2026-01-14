# Get a summary of the daily drug dose

Get a summary of the daily drug dose

## Usage

``` r
checkDrugDose(cdm, ingredientConceptId, sampleSize = NULL, minCellCount = 5)
```

## Arguments

- cdm:

  CDMConnector reference object

- ingredientConceptId:

  ingredient

- sampleSize:

  Maximum number of records of an ingredient to estimate dose coverage.
  If an ingredient has more, a random sample equal to `sampleSize` will
  be considered. If NULL, all records will be used.

- minCellCount:

  minimum number of events to report- results lower than this will be
  obscured. If NULL all results will be reported.

## Value

a table with the stats about the daily dose
