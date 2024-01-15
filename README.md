
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrugExposureDiagnostics <img src='man/figures/DrugExposureDiagnostics.png' align="right" height="139"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/DrugExposureDiagnostics)](https://CRAN.R-project.org/package=DrugExposureDiagnostics)
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

## Setup

In order to use the example data from the eunomia dataset, you need to
set an environment variable. You can do it like this:

- `{r} usethis::edit_r_environ()`
- EUNOMIA_DATA_FOLDER = “/mypath/eunomiaData”
- save the file
- restart RStudio

## Example use

``` r
library(DrugExposureDiagnostics)
library(CDMConnector)
library(dplyr)
```

``` r
cdm <- getEunomiaCdm()
```

Let´s look at the ingredient acetaminophen
(<https://athena.ohdsi.org/search-terms/terms/1125315>).

We can run all the checks available in ´DrugExposureDiagnostics´ using
the ´executeChecks´ function.

``` r
all_checks <- executeChecks(cdm = cdm, 
                            ingredients = 1125315, 
                            checks = c("missing", "exposureDuration", "type", "route", "sourceConcept", "daysSupply", "verbatimEndDate", 
                                       "dose", "sig", "quantity", "ingredientOverview", "ingredientPresence", "histogram", "diagnosticsSummary"))
#> Warning: `getIngredientOverview()` was deprecated in DrugExposureDiagnostics 0.4.7.
#> ℹ The deprecated feature was likely used in the DrugExposureDiagnostics
#>   package.
#>   Please report the issue to the authors.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> population after earliestStartDate smaller than sample
#> Warning: `getIngredientPresence()` was deprecated in DrugExposureDiagnostics 0.4.7.
#> ℹ The deprecated feature was likely used in the DrugExposureDiagnostics
#>   package.
#>   Please report the issue to the authors.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Joining with `by = join_by(ingredient_concept_id)`
#> Joining with `by = join_by(ingredient_concept_id)`
```

The output is a list which contains the following set of tibbles:

``` r
names(all_checks)
#>  [1] "conceptSummary"              "missingValuesOverall"       
#>  [3] "drugExposureDurationOverall" "drugTypesOverall"           
#>  [5] "drugRoutesOverall"           "drugSourceConceptsOverall"  
#>  [7] "drugDaysSupply"              "drugVerbatimEndDate"        
#>  [9] "drugDose"                    "drugSig"                    
#> [11] "drugQuantity"                "drugIngredientOverview"     
#> [13] "drugIngredientPresence"      "drugDaysSupplyHistogram"    
#> [15] "drugQuantityHistogram"       "drugDurationHistogram"      
#> [17] "diagnosticsSummary"
```

The first item contains information on the concept ids that are used in
the database for a given ingredient.

``` r
glimpse(all_checks$conceptSummary)
#> Rows: 2
#> Columns: 26
#> Rowwise: 
#> $ drug_concept_id             <int> 40162522, 1127078
#> $ drug                        <chr> "Acetaminophen 325 MG / Hydrocodone Bitart…
#> $ ingredient_concept_id       <dbl> 1125315, 1125315
#> $ ingredient                  <chr> "Acetaminophen", "Acetaminophen"
#> $ n_records                   <int> 312, 2158
#> $ n_patients                  <int> 312, 1428
#> $ domain_id                   <chr> "Drug", "Drug"
#> $ vocabulary_id               <chr> "RxNorm", "RxNorm"
#> $ concept_class_id            <chr> "Clinical Drug", "Clinical Drug"
#> $ standard_concept            <chr> "S", "S"
#> $ concept_code                <chr> "857005", "282464"
#> $ valid_start_date            <date> 2009-08-02, 1970-01-01
#> $ valid_end_date              <date> 2099-12-31, 2099-12-31
#> $ invalid_reason              <chr> NA, NA
#> $ amount_value                <dbl> 1, 1
#> $ amount_unit_concept_id      <dbl> 1, 1
#> $ numerator_value             <dbl> 1, 1
#> $ numerator_unit_concept_id   <dbl> 1, 1
#> $ numerator_unit              <chr> NA, NA
#> $ denominator_value           <dbl> 1, 1
#> $ denominator_unit_concept_id <dbl> 1, 1
#> $ denominator_unit            <chr> NA, NA
#> $ box_size                    <int> NA, NA
#> $ amount_unit                 <chr> NA, NA
#> $ dose_form                   <chr> NA, NA
#> $ result_obscured             <lgl> FALSE, FALSE
all_checks$conceptSummary %>% 
  select("drug_concept_id", "drug")
#> # A tibble: 2 × 2
#> # Rowwise: 
#>   drug_concept_id drug                                                          
#>             <int> <chr>                                                         
#> 1        40162522 Acetaminophen 325 MG / Hydrocodone Bitartrate 7.5 MG Oral Tab…
#> 2         1127078 Acetaminophen 160 MG Oral Tablet
```

Other tibbles then contain information from the various checks
performed.

For example, we can see a summary of missingness for the
ingredient-related records in the drug exposure table, both overall and
by concept.

``` r
all_checks$missingValuesOverall
#> # A tibble: 18 × 9
#> # Rowwise:  ingredient_concept_id, ingredient
#>    ingredient_concept_id ingredient    variable               n_records n_sample
#>                    <dbl> <chr>         <chr>                      <int>    <dbl>
#>  1               1125315 Acetaminophen n_missing_drug_exposu…        59    10000
#>  2               1125315 Acetaminophen n_missing_drug_exposu…        59    10000
#>  3               1125315 Acetaminophen n_missing_verbatim_en…        59    10000
#>  4               1125315 Acetaminophen n_missing_drug_type_c…        59    10000
#>  5               1125315 Acetaminophen n_missing_stop_reason         59    10000
#>  6               1125315 Acetaminophen n_missing_refills             59    10000
#>  7               1125315 Acetaminophen n_missing_quantity            59    10000
#>  8               1125315 Acetaminophen n_missing_days_supply         59    10000
#>  9               1125315 Acetaminophen n_missing_sig                 59    10000
#> 10               1125315 Acetaminophen n_missing_route_conce…        59    10000
#> 11               1125315 Acetaminophen n_missing_lot_number          59    10000
#> 12               1125315 Acetaminophen n_missing_provider_id         59    10000
#> 13               1125315 Acetaminophen n_missing_visit_occur…        59    10000
#> 14               1125315 Acetaminophen n_missing_visit_detai…        59    10000
#> 15               1125315 Acetaminophen n_missing_drug_source…        59    10000
#> 16               1125315 Acetaminophen n_missing_drug_source…        59    10000
#> 17               1125315 Acetaminophen n_missing_route_sourc…        59    10000
#> 18               1125315 Acetaminophen n_missing_dose_unit_s…        59    10000
#> # ℹ 4 more variables: n_records_not_missing_value <dbl>,
#> #   n_records_missing_value <dbl>, proportion_records_missing_value <dbl>,
#> #   result_obscured <lgl>
all_checks$missingValuesByConcept
#> NULL
```

Or we can also see a summary of drug exposure duration
(drug_exposure_end_date - drug_exposure_end_date + 1), again overall or
by concept.

``` r
all_checks$drugExposureDurationOverall
#> # A tibble: 1 × 17
#> # Rowwise:  ingredient_concept_id
#>   ingredient_concept_id ingredient    n_records n_sample n_non_negative_days
#>                   <dbl> <chr>             <int>    <dbl>               <int>
#> 1               1125315 Acetaminophen        59    10000                  59
#> # ℹ 12 more variables: n_negative_days <int>, proportion_negative_days <dbl>,
#> #   minimum_drug_exposure_days <dbl>, q05_drug_exposure_days <dbl>,
#> #   q10_drug_exposure_days <dbl>, q25_drug_exposure_days <dbl>,
#> #   median_drug_exposure_days <dbl>, q75_drug_exposure_days <dbl>,
#> #   q90_drug_exposure_days <dbl>, q95_drug_exposure_days <dbl>,
#> #   maximum_drug_exposure_days <dbl>, result_obscured <lgl>
all_checks$drugExposureDurationByConcept
#> NULL
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
