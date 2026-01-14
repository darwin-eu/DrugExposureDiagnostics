# Get drug exposure route types

Get drug exposure route types

## Usage

``` r
getDrugRoutes(
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

  by individual drug Concept

- sampleSize:

  the sample size given in execute checks

## Value

a table with the drug exposure route types
