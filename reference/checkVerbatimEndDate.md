# Check the verbatim_end_date field

Check the verbatim_end_date field

## Usage

``` r
checkVerbatimEndDate(
  cdm,
  drugRecordsTable = "ingredient_drug_records",
  byConcept = TRUE,
  sampleSize = 10000
)
```

## Arguments

- cdm:

  CDMConnector reference object

- drugRecordsTable:

  a modified version of the drug exposure table, default
  "ingredient_drug_records"

- byConcept:

  whether to get result by drug concept

- sampleSize:

  the sample size given in execute checks

## Value

a table with the stats about the verbatim_end_date
