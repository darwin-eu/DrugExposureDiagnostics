
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrugExposureDiagnostics

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
all_checks <- executeChecks(cdm, 1125315, minCellCount = NULL)
#> population after earliestStartDate smaller than sample, ignoring date for sampling
#> Joining with `by = join_by(ingredient_concept_id)`
#> Joining with `by = join_by(ingredient_concept_id)`
```

The output is a list which contains the following set of tibbles:

``` r
names(all_checks)
#>  [1] "conceptSummary"                "missingValuesOverall"         
#>  [3] "missingValuesByConcept"        "drugExposureDurationOverall"  
#>  [5] "drugExposureDurationByConcept" "drugTypesOverall"             
#>  [7] "drugTypesByConcept"            "drugRoutesOverall"            
#>  [9] "drugRoutesByConcept"           "drugSourceConceptsOverall"    
#> [11] "drugSourceConceptsByConcept"   "drugDaysSupply"               
#> [13] "drugDaysSupplyByConcept"       "drugVerbatimEndDate"          
#> [15] "drugVerbatimEndDateByConcept"  "drugDose"                     
#> [17] "drugDoseByConcept"             "drugSig"                      
#> [19] "drugSigByConcept"              "drugQuantity"                 
#> [21] "drugQuantityByConcept"         "drugIngredientOverview"       
#> [23] "drugIngredientPresence"        "diagnosticsSummary"
```

The first item contains information on the concept ids that are used in
the database for a given ingredient.

``` r
glimpse(all_checks$conceptSummary)
#> Rows: 2
#> Columns: 24
#> $ drug_concept_id             <dbl> 40162522, 1127078
#> $ drug                        <chr> "Acetaminophen 325 MG / Hydrocodone Bitart…
#> $ ingredient_concept_id       <dbl> 1125315, 1125315
#> $ ingredient                  <chr> "Acetaminophen", "Acetaminophen"
#> $ n_records                   <dbl> 312, 2158
#> $ domain_id                   <chr> "Drug", "Drug"
#> $ vocabulary_id               <chr> "RxNorm", "RxNorm"
#> $ concept_class_id            <chr> "Clinical Drug", "Clinical Drug"
#> $ standard_concept            <chr> "S", "S"
#> $ concept_code                <chr> "857005", "282464"
#> $ valid_start_date            <date> 2009-08-02, 1970-01-01
#> $ valid_end_date              <date> 2099-12-31, 2099-12-31
#> $ invalid_reason              <lgl> NA, NA
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
all_checks$conceptSummary %>% 
  select("drug_concept_id", "drug")
#> # A tibble: 2 × 2
#>   drug_concept_id drug                                                          
#>             <dbl> <chr>                                                         
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
#> # A tibble: 18 × 7
#> # Groups:   ingredient_concept_id, ingredient [1]
#>    ingredient_concept_id ingredient    variable n_records n_records_not_missin…¹
#>                    <dbl> <chr>         <chr>        <dbl>                  <dbl>
#>  1               1125315 Acetaminophen n_missi…      2470                   2470
#>  2               1125315 Acetaminophen n_missi…      2470                   2470
#>  3               1125315 Acetaminophen n_missi…      2470                   2158
#>  4               1125315 Acetaminophen n_missi…      2470                   2470
#>  5               1125315 Acetaminophen n_missi…      2470                      0
#>  6               1125315 Acetaminophen n_missi…      2470                   2470
#>  7               1125315 Acetaminophen n_missi…      2470                   2470
#>  8               1125315 Acetaminophen n_missi…      2470                   2470
#>  9               1125315 Acetaminophen n_missi…      2470                      0
#> 10               1125315 Acetaminophen n_missi…      2470                   2470
#> 11               1125315 Acetaminophen n_missi…      2470                   2470
#> 12               1125315 Acetaminophen n_missi…      2470                   2470
#> 13               1125315 Acetaminophen n_missi…      2470                   2470
#> 14               1125315 Acetaminophen n_missi…      2470                   2470
#> 15               1125315 Acetaminophen n_missi…      2470                   2470
#> 16               1125315 Acetaminophen n_missi…      2470                   2470
#> 17               1125315 Acetaminophen n_missi…      2470                      0
#> 18               1125315 Acetaminophen n_missi…      2470                      0
#> # ℹ abbreviated name: ¹​n_records_not_missing_value
#> # ℹ 2 more variables: n_records_missing_value <dbl>,
#> #   proportion_records_missing_value <dbl>
all_checks$missingValuesByConcept
#> # A tibble: 36 × 9
#> # Groups:   drug_concept_id, drug, ingredient_concept_id, ingredient [2]
#>    drug_concept_id drug      ingredient_concept_id ingredient variable n_records
#>              <dbl> <chr>                     <dbl> <chr>      <chr>        <dbl>
#>  1         1127078 Acetamin…               1125315 Acetamino… n_missi…      2158
#>  2         1127078 Acetamin…               1125315 Acetamino… n_missi…      2158
#>  3         1127078 Acetamin…               1125315 Acetamino… n_missi…      2158
#>  4         1127078 Acetamin…               1125315 Acetamino… n_missi…      2158
#>  5         1127078 Acetamin…               1125315 Acetamino… n_missi…      2158
#>  6         1127078 Acetamin…               1125315 Acetamino… n_missi…      2158
#>  7         1127078 Acetamin…               1125315 Acetamino… n_missi…      2158
#>  8         1127078 Acetamin…               1125315 Acetamino… n_missi…      2158
#>  9         1127078 Acetamin…               1125315 Acetamino… n_missi…      2158
#> 10         1127078 Acetamin…               1125315 Acetamino… n_missi…      2158
#> # ℹ 26 more rows
#> # ℹ 3 more variables: n_records_not_missing_value <dbl>,
#> #   n_records_missing_value <dbl>, proportion_records_missing_value <dbl>
```

Or we can also see a summary of drug exposure duration
(drug_exposure_end_date - drug_exposure_end_date + 1), again overall or
by concept.

``` r
all_checks$drugExposureDurationOverall
#> # A tibble: 1 × 15
#> # Groups:   ingredient_concept_id [1]
#>   ingredient_concept_id ingredient n_records n_non_negative_days n_negative_days
#>                   <dbl> <chr>          <int>               <int>           <int>
#> 1               1125315 Acetamino…      2470                2470               0
#> # ℹ 10 more variables: proportion_negative_days <dbl>,
#> #   minimum_drug_exposure_days <dbl>, q05_drug_exposure_days <dbl>,
#> #   q10_drug_exposure_days <dbl>, q25_drug_exposure_days <dbl>,
#> #   median_drug_exposure_days <dbl>, q75_drug_exposure_days <dbl>,
#> #   q90_drug_exposure_days <dbl>, q95_drug_exposure_days <dbl>,
#> #   maximum_drug_exposure_days <dbl>
all_checks$drugExposureDurationByConcept
#> # A tibble: 2 × 17
#> # Groups:   drug_concept_id, drug, ingredient_concept_id [2]
#>   drug_concept_id drug                ingredient_concept_id ingredient n_records
#>             <dbl> <chr>                               <dbl> <chr>          <int>
#> 1         1127078 Acetaminophen 160 …               1125315 Acetamino…      2158
#> 2        40162522 Acetaminophen 325 …               1125315 Acetamino…       312
#> # ℹ 12 more variables: n_non_negative_days <int>, n_negative_days <int>,
#> #   proportion_negative_days <dbl>, minimum_drug_exposure_days <dbl>,
#> #   q05_drug_exposure_days <dbl>, q10_drug_exposure_days <dbl>,
#> #   q25_drug_exposure_days <dbl>, median_drug_exposure_days <dbl>,
#> #   q75_drug_exposure_days <dbl>, q90_drug_exposure_days <dbl>,
#> #   q95_drug_exposure_days <dbl>, maximum_drug_exposure_days <dbl>
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
