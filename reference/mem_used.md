# How much memory is currently used by R?

R breaks down memory usage into Vcells (memory used by vectors) and
Ncells (memory used by everything else). However, neither this
distinction nor the "gc trigger" and "max used" columns are typically
important. What we're usually most interested in is the the first
column: the total memory used. This function wraps around
[`gc()`](https://rdrr.io/r/base/gc.html) to return the total amount of
memory (in megabytes) currently used by R.

## Usage

``` r
mem_used()
```

## Value

Megabytes of ram used by R objects.
