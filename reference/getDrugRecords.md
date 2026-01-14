# Drug exposure records for ingredients of interest

Drug exposure records for ingredients of interest

## Usage

``` r
getDrugRecords(
  cdm,
  ingredient,
  includedConceptsTable,
  drugRecordsTable = "drug_exposure",
  exposureTypeId = NULL,
  tablePrefix = NULL,
  verbose = FALSE
)
```

## Arguments

- cdm:

  CDMConnector reference object

- ingredient:

  Concept ID for ingredient of interest

- includedConceptsTable:

  includedConceptsTable

- drugRecordsTable:

  drugRecordsTable, default "drug_exposure"

- exposureTypeId:

  id of the drug exposure type to be filtered on (e.g. only prescribed).
  By default all record types will be taken into account.

- tablePrefix:

  The stem for the permanent tables that will be created when running
  the diagnostics. Permanent tables will be created using this prefix,
  and any existing tables that start with this will be at risk of being
  dropped or overwritten. If NULL, temporary tables will be used
  throughout.

- verbose:

  verbose

## Value

a table containing drug exposure records
