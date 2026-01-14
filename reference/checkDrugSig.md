# Check the drug sig field; this is the verbatim instruction for the drug as written by the provider.

Check the drug sig field; this is the verbatim instruction for the drug
as written by the provider.

## Usage

``` r
checkDrugSig(
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

a table with a summary of the sig values
