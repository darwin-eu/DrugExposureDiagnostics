# Summarise the quantity column of the drug_exposure table

Summarise the quantity column of the drug_exposure table

## Usage

``` r
summariseQuantity(
  cdm,
  drugRecordsTable = "ingredient_drug_records",
  byConcept = TRUE,
  sampleSize = sampleSize
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

a table with the summarized quantity result
