# Compute the difference in days between 2 variables in a database table.

Compute the difference in days between 2 variables in a database table.

## Usage

``` r
getDuration(
  cdm,
  tableName = "drug_exposure",
  startDateCol = "drug_exposure_start_date",
  endDateCol = "drug_exposure_end_date",
  colName = "duration"
)
```

## Arguments

- cdm:

  CDMConnector reference object

- tableName:

  the table name

- startDateCol:

  the start date column name

- endDateCol:

  the end date column name

- colName:

  the result column name

## Value

the table with as new column the duration
