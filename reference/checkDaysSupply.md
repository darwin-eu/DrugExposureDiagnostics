# Check if Days_supply is the same as datediff(drug_exp_start_date,drug_exp_end_date)

Check if Days_supply is the same as
datediff(drug_exp_start_date,drug_exp_end_date)

## Usage

``` r
checkDaysSupply(
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

a table with the stats of days supply compared to start and end date
