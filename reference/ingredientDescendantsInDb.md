# Get the descendants for the given ingredients

Get the descendants for the given ingredients

## Usage

``` r
ingredientDescendantsInDb(
  cdm,
  ingredient,
  drugRecordsTable = "drug_exposure",
  tablePrefix = NULL,
  verbose = FALSE
)
```

## Arguments

- cdm:

  CDMConnector reference object

- ingredient:

  ingredient concept id for ingredient of interest

- drugRecordsTable:

  table name of the drug exposure records, default "drug_exposure"

- tablePrefix:

  The stem for the permanent tables that will be created when running
  the diagnostics. Permanent tables will be created using this prefix,
  and any existing tables that start with this will be at risk of being
  dropped or overwritten. If NULL, temporary tables will be used
  throughout.

- verbose:

  if verbose set to TRUE, the function will output extra messages

## Value

temp table with concepts used
