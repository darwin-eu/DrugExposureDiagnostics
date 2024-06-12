
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrugExposureDiagnostics <img src='man/figures/DrugExposureDiagnostics.png' align="right" width="139"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/DrugExposureDiagnostics)](https://CRAN.R-project.org/package=DrugExposureDiagnostics)
[![codecov.io](https://codecov.io/github/darwin-eu/DrugExposureDiagnostics/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu/DrugExposureDiagnostics?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/DrugExposureDiagnostics/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/DrugExposureDiagnostics/actions)
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
#>   version 1.0.5, https://github.com/darwin-eu/DrugExposureDiagnostics,
#>   <https://darwin-eu.github.io/DrugExposureDiagnostics/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {DrugExposureDiagnostics: Diagnostics for OMOP Common Data Model Drug Records},
#>     author = {Ger Inberg and Edward Burn and Theresa Burkard},
#>     note = {R package version 1.0.5, https://github.com/darwin-eu/DrugExposureDiagnostics},
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
```

Let´s look at the ingredient acetaminophen
(<https://athena.ohdsi.org/search-terms/terms/1125315>).

We can run all the checks available in ´DrugExposureDiagnostics´ using
the ´executeChecks´ function.

``` r
all_checks <- executeChecks(cdm = cdm, 
                            ingredients = 1125315, 
                            checks = c("missing", "exposureDuration", "type", "route", "sourceConcept", "daysSupply", 
                                       "verbatimEndDate", "dose", "sig", "quantity", "diagnosticsSummary"))
#> population after earliestStartDate smaller than sample, sampling ignored
#> ℹ The following estimates will be computed:
#> • daily_dose: count_missing, percentage_missing, mean, sd, min, q05, q25,
#>   median, q75, q95, max
#> ! Table is collected to memory as not all requested estimates are supported on
#>   the database side
#> → Start summary of data, at 2024-06-12 14:48:26.801546
#> 
#> ✔ Summary finished, at 2024-06-12 14:48:26.97046
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
```

The first item contains information on the concept ids that are used in
the database for a given ingredient.

``` r
glimpse(all_checks$conceptSummary)
#> Rows: 6
#> Columns: 25
#> Rowwise: 
#> $ drug_concept_id             <dbl> 40231925, 19133768, 1127433, 1127078, 4022…
#> $ drug                        <chr> "acetaminophen 325 MG / Hydrocodone Bitart…
#> $ ingredient_concept_id       <dbl> 1125315, 1125315, 1125315, 1125315, 112531…
#> $ ingredient                  <chr> "acetaminophen", "acetaminophen", "acetami…
#> $ n_records                   <int> 10, 14, 13, 19, 12, 18
#> $ n_patients                  <int> 9, 13, 11, 13, 11, 15
#> $ domain_id                   <chr> "Drug", "Drug", "Drug", "Drug", "Drug", "D…
#> $ vocabulary_id               <chr> "RxNorm", "RxNorm", "RxNorm", "RxNorm", "R…
#> $ concept_class_id            <chr> "Clinical Drug", "Clinical Drug", "Clinica…
#> $ standard_concept            <chr> "S", "S", "S", "S", "S", "S"
#> $ concept_code                <chr> "857005", "282464", "1049221", "833036", "…
#> $ valid_start_date            <date> 1970-01-01, 1970-01-01, 1970-01-01, 1970-0…
#> $ valid_end_date              <date> 2099-12-31, 2099-12-31, 2099-12-31, 2099-1…
#> $ invalid_reason              <lgl> NA, NA, NA, NA, NA, NA
#> $ amount_value                <dbl> NA, NA, 200, NA, 100, 200
#> $ amount_unit_concept_id      <dbl> NA, NA, 9655, NA, 9655, 9655
#> $ numerator_value             <dbl> 3, 3, NA, 3, NA, NA
#> $ numerator_unit_concept_id   <dbl> 8576, 8576, NA, 8576, NA, NA
#> $ numerator_unit              <chr> NA, NA, NA, NA, NA, NA
#> $ denominator_value           <dbl> 10, 10, NA, 10, NA, NA
#> $ denominator_unit_concept_id <dbl> 8587, 8587, NA, 8587, NA, NA
#> $ denominator_unit            <chr> NA, NA, NA, NA, NA, NA
#> $ amount_unit                 <chr> NA, NA, NA, NA, NA, NA
#> $ dose_form                   <chr> "Oral Tablet", "Oral Tablet", "Oral Tablet…
#> $ result_obscured             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
all_checks$conceptSummary %>% 
  select("drug_concept_id", "drug")
#> # A tibble: 6 × 2
#> # Rowwise: 
#>   drug_concept_id drug                                          
#>             <dbl> <chr>                                         
#> 1        40231925 acetaminophen 325 MG / Hydrocodone Bitartrate 
#> 2        19133768 acetaminophen 160 MG Oral Tablet              
#> 3         1127433 acetaminophen 325 MG / Oxycodone Hydrochloride
#> 4         1127078 acetaminophen 750 MG / Hydrocodone Bitartrate 
#> 5        40229134 acetaminophen 21.7 MG/ML / Dextromethorphan   
#> 6        40162522 acetaminophen 325 MG Oral Tablet
```

Other tibbles then contain information from the various checks
performed.

For example, we can see a summary of missingness for the
ingredient-related records in the drug exposure table, both overall and
by concept.

``` r
all_checks$missingValuesOverall
#> # A tibble: 15 × 9
#> # Rowwise:  ingredient_concept_id, ingredient
#>    ingredient_concept_id ingredient    variable               n_records n_sample
#>                    <dbl> <chr>         <chr>                      <int>    <dbl>
#>  1               1125315 acetaminophen n_missing_drug_exposu…        44    10000
#>  2               1125315 acetaminophen n_missing_person_id           44    10000
#>  3               1125315 acetaminophen n_missing_drug_concep…        44    10000
#>  4               1125315 acetaminophen n_missing_drug_exposu…        44    10000
#>  5               1125315 acetaminophen n_missing_drug_exposu…        44    10000
#>  6               1125315 acetaminophen n_missing_verbatim_en…        44    10000
#>  7               1125315 acetaminophen n_missing_drug_type_c…        44    10000
#>  8               1125315 acetaminophen n_missing_quantity            44    10000
#>  9               1125315 acetaminophen n_missing_days_supply         44    10000
#> 10               1125315 acetaminophen n_missing_sig                 44    10000
#> 11               1125315 acetaminophen n_missing_route_conce…        44    10000
#> 12               1125315 acetaminophen n_missing_drug_source…        44    10000
#> 13               1125315 acetaminophen n_missing_drug_source…        44    10000
#> 14               1125315 acetaminophen n_missing_route_sourc…        44    10000
#> 15               1125315 acetaminophen n_missing_dose_unit_s…        44    10000
#> # ℹ 4 more variables: n_records_not_missing_value <dbl>,
#> #   n_records_missing_value <dbl>, proportion_records_missing_value <dbl>,
#> #   result_obscured <lgl>
all_checks$missingValuesByConcept
#> # A tibble: 90 × 11
#> # Rowwise:  drug_concept_id, drug, ingredient_concept_id, ingredient
#>    drug_concept_id drug      ingredient_concept_id ingredient variable n_records
#>              <dbl> <chr>                     <dbl> <chr>      <chr>        <int>
#>  1        40162522 acetamin…               1125315 acetamino… n_missi…        12
#>  2        40162522 acetamin…               1125315 acetamino… n_missi…        12
#>  3        40162522 acetamin…               1125315 acetamino… n_missi…        12
#>  4        40162522 acetamin…               1125315 acetamino… n_missi…        12
#>  5        40162522 acetamin…               1125315 acetamino… n_missi…        12
#>  6        40162522 acetamin…               1125315 acetamino… n_missi…        12
#>  7        40162522 acetamin…               1125315 acetamino… n_missi…        12
#>  8        40162522 acetamin…               1125315 acetamino… n_missi…        12
#>  9        40162522 acetamin…               1125315 acetamino… n_missi…        12
#> 10        40162522 acetamin…               1125315 acetamino… n_missi…        12
#> # ℹ 80 more rows
#> # ℹ 5 more variables: n_sample <dbl>, n_records_not_missing_value <dbl>,
#> #   n_records_missing_value <dbl>, proportion_records_missing_value <dbl>,
#> #   result_obscured <lgl>
```

Or we can also see a summary of drug exposure duration
(drug_exposure_end_date - drug_exposure_end_date + 1), again overall or
by concept.

``` r
all_checks$drugExposureDurationOverall
#> # A tibble: 1 × 18
#> # Rowwise:  ingredient_concept_id
#>   ingredient_concept_id ingredient    n_records n_sample n_person
#>                   <dbl> <chr>             <int>    <dbl>    <int>
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
#>             <dbl> <chr>                      <dbl> <chr>          <int>    <dbl>
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
                  outputFolder =tempdir())
```
