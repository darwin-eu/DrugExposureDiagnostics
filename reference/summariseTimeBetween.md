# Check time in between drug records per person and report the summary

This check requires complete exposure sequences, so
[`executeChecks()`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/executeChecks.md)
runs it on all eligible records rather than on the record-level sample
used by other checks.

## Usage

``` r
summariseTimeBetween(
  cdm,
  drugRecordsTable = "ingredient_drug_records",
  byConcept = TRUE
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

## Value

a table with the stats about the time between
