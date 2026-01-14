# Obscure the small number of counts

Obscure the small number of counts

## Usage

``` r
obscureCounts(table, tableName, minCellCount = 5, substitute = NA)
```

## Arguments

- table:

  the table as a tibble

- tableName:

  the table name

- minCellCount:

  the minimum number of counts that will be displayed. If 0 all results
  will be reported.

- substitute:

  the substitute value if values will be obscured

## Value

the input table with results obscured if minCellCount applies
