# Drug Time Between

## Run the drug time-between check

``` r

library(DrugExposureDiagnostics)
cdm <- mockDrugExposure()
result <- executeChecks(
  cdm = cdm,
  checks = "daysBetween",
  sample = 10
)
```

## Drug time between overall

This check calculates the number of days between the start dates of
consecutive drug exposure records for each person, summarised at the
ingredient level. Only people with at least two records in the relevant
group contribute an interval.

`sample` is **not used** for this check. Sampling individual records
would remove records from a person’s exposure sequence and create
artificial gaps. Therefore, even when a sample size is supplied to
[`executeChecks()`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/executeChecks.md),
the time-between check uses all eligible records after any
`earliestStartDate`, concept, and exposure-type restrictions have been
applied. The other requested checks continue to use the record-level
sample.

``` r

knitr::kable(result$drugTimeBetween)
```

| ingredient_concept_id | ingredient | n_records | n_person | minimum_time_between_days | q05_time_between_days | q10_time_between_days | q25_time_between_days | median_time_between_days | q75_time_between_days | q90_time_between_days | q95_time_between_days | maximum_time_between_days | result_obscured |
|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| 1125315 | acetaminophen | 19 | 12 | 62 | 98.9 | 113.4 | 506 | 1034 | 1497 | 2420 | 2535.9 | 3219 | FALSE |

| Column | Description |
|:---|:---|
| ingredient_concept_id | Concept ID of the ingredient. |
| ingredient | Name of the drug ingredient. |
| n_records | Number of consecutive-record intervals included in the summary. This is not the number of original drug exposure records. |
| n_person | Number of people contributing at least one consecutive-record interval. |
| minimum_time_between_days | Minimum number of days between consecutive drug exposure start dates. |
| q05_time_between_days | 5th percentile of the time between consecutive records. |
| q10_time_between_days | 10th percentile of the time between consecutive records. |
| q25_time_between_days | 25th percentile of the time between consecutive records. |
| median_time_between_days | Median number of days between consecutive records. |
| q75_time_between_days | 75th percentile of the time between consecutive records. |
| q90_time_between_days | 90th percentile of the time between consecutive records. |
| q95_time_between_days | 95th percentile of the time between consecutive records. |
| maximum_time_between_days | Maximum number of days between consecutive drug exposure start dates. |
| result_obscured | `TRUE` if counts have been suppressed because they are below the minimum cell count; otherwise `FALSE`. |

There is no `n_sample` column because this check does not use a
record-level sample.

## Drug time between by drug concept

When `byConcept = TRUE` (the default), `drugTimeBetweenByConcept`
provides the same summary separately for each drug concept. Consecutive
records are determined within person and drug concept.

| Column          | Description               |
|:----------------|:--------------------------|
| drug_concept_id | ID of the drug concept.   |
| drug            | Name of the drug concept. |

``` r

knitr::kable(result$drugTimeBetweenByConcept)
```
