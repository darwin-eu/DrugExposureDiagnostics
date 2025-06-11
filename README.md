
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrugExposureDiagnostics <img src='man/figures/DrugExposureDiagnostics.png' align="right" width="139"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/DrugExposureDiagnostics)](https://CRAN.R-project.org/package=DrugExposureDiagnostics)
[![codecov.io](https://codecov.io/github/darwin-eu/DrugExposureDiagnostics/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu/DrugExposureDiagnostics?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/DrugExposureDiagnostics/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/DrugExposureDiagnostics/actions)
[![Lifecycle:stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The goal of DrugExposureDiagnostics is to summarise ingredient specific
drug exposure data in the OMOP CDM.

## Installation

You can install the DrugExposureDiagnostics from CRAN like this:

``` r
install.packages("DrugExposureDiagnostics")
```

or install the development version:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu/DrugExposureDiagnostics")
```

## Citation

``` r
citation("DrugExposureDiagnostics")
#> Warning in citation("DrugExposureDiagnostics"): could not determine year for
#> 'DrugExposureDiagnostics' from package DESCRIPTION file
#> To cite package 'DrugExposureDiagnostics' in publications use:
#> 
#>   Inberg G, Burn E, Burkard T (????). _DrugExposureDiagnostics:
#>   Diagnostics for OMOP Common Data Model Drug Records_. R package
#>   version 1.1.0, https://github.com/darwin-eu/DrugExposureDiagnostics,
#>   <https://darwin-eu.github.io/DrugExposureDiagnostics/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {DrugExposureDiagnostics: Diagnostics for OMOP Common Data Model Drug Records},
#>     author = {Ger Inberg and Edward Burn and Theresa Burkard},
#>     note = {R package version 1.1.0, https://github.com/darwin-eu/DrugExposureDiagnostics},
#>     url = {https://darwin-eu.github.io/DrugExposureDiagnostics/},
#>   }
```

## Example use

``` r
library(DrugExposureDiagnostics)
library(CDMConnector)
library(dplyr)
```

``` r
cdm <- mockDrugExposure()
#> Note: method with signature 'DBIConnection#Id' chosen for function 'dbExistsTable',
#>  target signature 'duckdb_connection#Id'.
#>  "duckdb_connection#ANY" would also be valid
```

Let´s look at the ingredient acetaminophen
(<https://athena.ohdsi.org/search-terms/terms/1125315>).

We can run all the checks available in ´DrugExposureDiagnostics´ using
the ´executeChecks´ function.

``` r
all_checks <- executeChecks(
  cdm = cdm,
  ingredients = 1125315,
  checks = c(
    "missing", "exposureDuration", "type", "route", "sourceConcept", "daysSupply",
    "verbatimEndDate", "dose", "sig", "quantity", "diagnosticsSummary"
  )
)
#> population after earliestStartDate smaller than sample, sampling ignored
#> ℹ The following estimates will be computed:
#> • daily_dose: count_missing, percentage_missing, mean, sd, q05, q25, median,
#>   q75, q95, min, max
#> ! Table is collected to memory as not all requested estimates are supported on
#>   the database side
#> → Start summary of data, at 2024-12-19 18:52:57.147139
#> 
#> Registered S3 method overwritten by 'visOmopResults':
#>   method                 from        
#>   tidy.summarised_result omopgenerics
#> 
#> ✔ Summary finished, at 2024-12-19 18:52:57.302844
#> `sample_size` casted to character.
#> `sample_size` eliminated from settings as all elements are NA.
```

The output is a list which contains the following set of tibbles:

``` r
names(all_checks)
#>  [1] "conceptSummary"                "missingValuesOverall"         
#>  [3] "missingValuesByConcept"        "drugExposureDurationOverall"  
#>  [5] "drugExposureDurationByConcept" "drugTypesOverall"             
#>  [7] "drugTypesByConcept"            "drugRoutesOverall"            
#>  [9] "drugRoutesByConcept"           "drugSourceConceptsOverall"    
#> [11] "drugDaysSupply"                "drugDaysSupplyByConcept"      
#> [13] "drugVerbatimEndDate"           "drugVerbatimEndDateByConcept" 
#> [15] "drugDose"                      "drugSig"                      
#> [17] "drugSigByConcept"              "drugQuantity"                 
#> [19] "drugQuantityByConcept"         "diagnosticsSummary"           
#> [21] "metadata"
```

The first item contains information on the concept ids that are used in
the database for a given ingredient.

``` r
glimpse(all_checks$conceptSummary)
#> Rows: 6
#> Columns: 26
#> Rowwise: 
#> $ drug_concept_id             <int> 1127078, 40162522, 40229134, 40231925, 191…
#> $ drug                        <chr> "acetaminophen 750 MG / Hydrocodone Bitart…
#> $ ingredient_concept_id       <int> 1125315, 1125315, 1125315, 1125315, 112531…
#> $ ingredient                  <chr> "acetaminophen", "acetaminophen", "acetami…
#> $ n_records                   <int> 19, 18, 12, 10, 14, 13
#> $ n_patients                  <int> 13, 15, 11, 9, 13, 11
#> $ domain_id                   <chr> "Drug", "Drug", "Drug", "Drug", "Drug", "D…
#> $ vocabulary_id               <chr> "RxNorm", "RxNorm", "RxNorm", "RxNorm", "R…
#> $ concept_class_id            <chr> "Clinical Drug", "Clinical Drug", "Clinica…
#> $ standard_concept            <chr> "S", "S", "S", "S", "S", "S"
#> $ concept_code                <chr> "833036", "313782", "1043400", "857005", "…
#> $ valid_start_date            <date> 1970-01-01, 1970-01-01, 1970-01-01, 1970-0…
#> $ valid_end_date              <date> 2099-12-31, 2099-12-31, 2099-12-31, 2099-1…
#> $ invalid_reason              <chr> NA, NA, NA, NA, NA, NA
#> $ amount_value                <dbl> 200, NA, NA, 300, 200, 200
#> $ amount_unit_concept_id      <int> 9655, NA, NA, 9655, 9655, 9655
#> $ numerator_value             <dbl> NA, 1, 1, NA, NA, NA
#> $ numerator_unit_concept_id   <int> NA, 8576, 8576, NA, NA, NA
#> $ numerator_unit              <chr> NA, NA, NA, NA, NA, NA
#> $ denominator_value           <dbl> NA, 10, 10, NA, NA, NA
#> $ denominator_unit_concept_id <int> NA, 8576, 8576, NA, NA, NA
#> $ denominator_unit            <chr> NA, NA, NA, NA, NA, NA
#> $ box_size                    <dbl> 0, 0, 0, 0, 0, 0
#> $ amount_unit                 <chr> NA, NA, NA, NA, NA, NA
#> $ dose_form                   <chr> "Oral Tablet", "Oral Tablet", "Oral Tablet…
#> $ result_obscured             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
all_checks$conceptSummary %>%
  select("drug_concept_id", "drug")
#> # A tibble: 6 × 2
#> # Rowwise: 
#>   drug_concept_id drug                                          
#>             <int> <chr>                                         
#> 1         1127078 acetaminophen 750 MG / Hydrocodone Bitartrate 
#> 2        40162522 acetaminophen 325 MG Oral Tablet              
#> 3        40229134 acetaminophen 21.7 MG/ML / Dextromethorphan   
#> 4        40231925 acetaminophen 325 MG / Hydrocodone Bitartrate 
#> 5        19133768 acetaminophen 160 MG Oral Tablet              
#> 6         1127433 acetaminophen 325 MG / Oxycodone Hydrochloride
```

Other tibbles then contain information from the various checks
performed.

For example, we can see a summary of missingness for the
ingredient-related records in the drug exposure table, both overall and
by concept.

``` r
all_checks$missingValuesOverall
#> # A tibble: 15 × 10
#> # Rowwise:  ingredient_concept_id, ingredient
#>    ingredient_concept_id ingredient    variable      n_records n_sample n_person
#>                    <int> <chr>         <chr>             <int>    <dbl>    <dbl>
#>  1               1125315 acetaminophen n_missing_dr…        44    10000       25
#>  2               1125315 acetaminophen n_missing_pe…        44    10000       25
#>  3               1125315 acetaminophen n_missing_dr…        44    10000       25
#>  4               1125315 acetaminophen n_missing_dr…        44    10000       25
#>  5               1125315 acetaminophen n_missing_dr…        44    10000       25
#>  6               1125315 acetaminophen n_missing_ve…        44    10000       25
#>  7               1125315 acetaminophen n_missing_dr…        44    10000       25
#>  8               1125315 acetaminophen n_missing_qu…        44    10000       25
#>  9               1125315 acetaminophen n_missing_da…        44    10000       25
#> 10               1125315 acetaminophen n_missing_sig        44    10000       25
#> 11               1125315 acetaminophen n_missing_ro…        44    10000       25
#> 12               1125315 acetaminophen n_missing_dr…        44    10000       25
#> 13               1125315 acetaminophen n_missing_dr…        44    10000       25
#> 14               1125315 acetaminophen n_missing_ro…        44    10000       25
#> 15               1125315 acetaminophen n_missing_do…        44    10000       25
#> # ℹ 4 more variables: n_records_not_missing_value <dbl>,
#> #   n_records_missing_value <dbl>, proportion_records_missing_value <dbl>,
#> #   result_obscured <lgl>
all_checks$missingValuesByConcept
#> # A tibble: 90 × 12
#> # Rowwise:  drug_concept_id, drug, ingredient_concept_id, ingredient
#>    drug_concept_id drug      ingredient_concept_id ingredient variable n_records
#>              <int> <chr>                     <int> <chr>      <chr>        <int>
#>  1        40229134 acetamin…               1125315 acetamino… n_missi…         6
#>  2        40229134 acetamin…               1125315 acetamino… n_missi…         6
#>  3        40229134 acetamin…               1125315 acetamino… n_missi…         6
#>  4        40229134 acetamin…               1125315 acetamino… n_missi…         6
#>  5        40229134 acetamin…               1125315 acetamino… n_missi…         6
#>  6        40229134 acetamin…               1125315 acetamino… n_missi…         6
#>  7        40229134 acetamin…               1125315 acetamino… n_missi…         6
#>  8        40229134 acetamin…               1125315 acetamino… n_missi…         6
#>  9        40229134 acetamin…               1125315 acetamino… n_missi…         6
#> 10        40229134 acetamin…               1125315 acetamino… n_missi…         6
#> # ℹ 80 more rows
#> # ℹ 6 more variables: n_sample <dbl>, n_person <dbl>,
#> #   n_records_not_missing_value <dbl>, n_records_missing_value <dbl>,
#> #   proportion_records_missing_value <dbl>, result_obscured <lgl>
```

Or we can also see a summary of drug exposure duration
(drug_exposure_end_date - drug_exposure_end_date + 1), again overall or
by concept.

``` r
all_checks$drugExposureDurationOverall
#> # A tibble: 1 × 18
#> # Rowwise:  ingredient_concept_id
#>   ingredient_concept_id ingredient    n_records n_sample n_person
#>                   <int> <chr>             <int>    <dbl>    <int>
#> 1               1125315 acetaminophen        44    10000       25
#> # ℹ 13 more variables: n_non_negative_days <int>, n_negative_days <int>,
#> #   proportion_negative_days <dbl>, minimum_drug_exposure_days <dbl>,
#> #   q05_drug_exposure_days <dbl>, q10_drug_exposure_days <dbl>,
#> #   q25_drug_exposure_days <dbl>, median_drug_exposure_days <dbl>,
#> #   q75_drug_exposure_days <dbl>, q90_drug_exposure_days <dbl>,
#> #   q95_drug_exposure_days <dbl>, maximum_drug_exposure_days <dbl>,
#> #   result_obscured <lgl>
all_checks$drugExposureDurationByConcept
#> # A tibble: 6 × 20
#> # Rowwise:  drug_concept_id, drug, ingredient_concept_id
#>   drug_concept_id drug       ingredient_concept_id ingredient n_records n_sample
#>             <int> <chr>                      <int> <chr>          <int>    <dbl>
#> 1         1127078 acetamino…               1125315 acetamino…         8    10000
#> 2         1127433 acetamino…               1125315 acetamino…         8    10000
#> 3        19133768 acetamino…               1125315 acetamino…         8    10000
#> 4        40162522 acetamino…               1125315 acetamino…        12    10000
#> 5        40229134 acetamino…               1125315 acetamino…         6    10000
#> 6        40231925 acetamino…               1125315 acetamino…        NA       NA
#> # ℹ 14 more variables: n_person <int>, n_non_negative_days <int>,
#> #   n_negative_days <int>, proportion_negative_days <dbl>,
#> #   minimum_drug_exposure_days <dbl>, q05_drug_exposure_days <dbl>,
#> #   q10_drug_exposure_days <dbl>, q25_drug_exposure_days <dbl>,
#> #   median_drug_exposure_days <dbl>, q75_drug_exposure_days <dbl>,
#> #   q90_drug_exposure_days <dbl>, q95_drug_exposure_days <dbl>,
#> #   maximum_drug_exposure_days <dbl>, result_obscured <lgl>
```

For further information on the checks performed please see the package
vignettes.

After running the checks we can write the CSVs to disk using the
`writeResultToDisk` function.

``` r
writeResultToDisk(all_checks,
  databaseId = "Synthea",
  outputFolder = tempdir()
)
```
