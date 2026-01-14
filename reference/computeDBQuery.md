# Store the given input in a remote database table. It will be stored either in a permanent table or a temporary table depending on tablePrefix.

Store the given input in a remote database table. It will be stored
either in a permanent table or a temporary table depending on
tablePrefix.

## Usage

``` r
computeDBQuery(table, tablePrefix, tableName, cdm, overwrite = TRUE)
```

## Arguments

- table:

  the input table

- tablePrefix:

  The stem for the permanent tables that will be created when running
  the diagnostics. Permanent tables will be created using this prefix,
  and any existing tables that start with this will be at risk of being
  dropped or overwritten. If NULL, temporary tables will be used
  throughout.

- tableName:

  the input table

- cdm:

  cdm reference object

- overwrite:

  if the table should be overwritten (default TRUE).

## Value

reference to the table
