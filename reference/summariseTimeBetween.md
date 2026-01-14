# Check time in between drug records per person and report the summary

Check time in between drug records per person and report the summary

## Usage

``` r
summariseTimeBetween(
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

a table with the stats about the time between
