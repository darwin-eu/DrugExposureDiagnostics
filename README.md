
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrugExposureDiagnostics

<!-- badges: start -->
<!-- badges: end -->

The goal of DrugExposureDiagnostics is to summarise ingredient specific
drug exposure data in the OMOP CDM.

## Installation

You can install the development version of DrugExposureDiagnostics like
this:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu-dev/DrugExposureDiagnostics")
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
#> [23] "drugIngredientPresence"        "drugDaysSupplyHistogram"      
#> [25] "drugQuantityHistogram"         "drugDurationHistogram"        
#> [27] "diagnostics_summary"
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
#>    ingredient_concept_id ingredient    variable  n_rec…¹ n_rec…² n_rec…³ propo…⁴
#>                    <dbl> <chr>         <chr>       <dbl>   <dbl>   <dbl>   <dbl>
#>  1               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#>  2               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#>  3               1125315 Acetaminophen n_missin…    2470    2158     312   0.126
#>  4               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#>  5               1125315 Acetaminophen n_missin…    2470       0    2470   1    
#>  6               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#>  7               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#>  8               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#>  9               1125315 Acetaminophen n_missin…    2470       0    2470   1    
#> 10               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#> 11               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#> 12               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#> 13               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#> 14               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#> 15               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#> 16               1125315 Acetaminophen n_missin…    2470    2470       0   0    
#> 17               1125315 Acetaminophen n_missin…    2470       0    2470   1    
#> 18               1125315 Acetaminophen n_missin…    2470       0    2470   1    
#> # … with abbreviated variable names ¹​n_records, ²​n_records_not_missing_value,
#> #   ³​n_records_missing_value, ⁴​proportion_records_missing_value
all_checks$missingValuesByConcept
#> # A tibble: 36 × 9
#> # Groups:   drug_concept_id, drug, ingredient_concept_id, ingredient [2]
#>    drug_concept_id drug  ingre…¹ ingre…² varia…³ n_rec…⁴ n_rec…⁵ n_rec…⁶ propo…⁷
#>              <dbl> <chr>   <dbl> <chr>   <chr>     <dbl>   <dbl>   <dbl>   <dbl>
#>  1         1127078 Acet… 1125315 Acetam… n_miss…    2158    2158       0       0
#>  2         1127078 Acet… 1125315 Acetam… n_miss…    2158    2158       0       0
#>  3         1127078 Acet… 1125315 Acetam… n_miss…    2158    2158       0       0
#>  4         1127078 Acet… 1125315 Acetam… n_miss…    2158    2158       0       0
#>  5         1127078 Acet… 1125315 Acetam… n_miss…    2158       0    2158       1
#>  6         1127078 Acet… 1125315 Acetam… n_miss…    2158    2158       0       0
#>  7         1127078 Acet… 1125315 Acetam… n_miss…    2158    2158       0       0
#>  8         1127078 Acet… 1125315 Acetam… n_miss…    2158    2158       0       0
#>  9         1127078 Acet… 1125315 Acetam… n_miss…    2158       0    2158       1
#> 10         1127078 Acet… 1125315 Acetam… n_miss…    2158    2158       0       0
#> # … with 26 more rows, and abbreviated variable names ¹​ingredient_concept_id,
#> #   ²​ingredient, ³​variable, ⁴​n_records, ⁵​n_records_not_missing_value,
#> #   ⁶​n_records_missing_value, ⁷​proportion_records_missing_value
```

Or we can also see a summary of drug exposure duration
(drug_exposure_end_date - drug_exposure_end_date + 1), again overall or
by concept.

``` r
all_checks$drugExposureDurationOverall
#> # A tibble: 1 × 15
#> # Groups:   ingredient_concept_id [1]
#>   ingredient_c…¹ ingre…² n_rec…³ n_non…⁴ n_neg…⁵ propo…⁶ minim…⁷ q05_d…⁸ q10_d…⁹
#>            <dbl> <chr>     <int>   <int>   <int>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1        1125315 Acetam…    2470    2470       0       0       1       1       1
#> # … with 6 more variables: q25_drug_exposure_days <dbl>,
#> #   median_drug_exposure_days <dbl>, q75_drug_exposure_days <dbl>,
#> #   q90_drug_exposure_days <dbl>, q95_drug_exposure_days <dbl>,
#> #   maximum_drug_exposure_days <dbl>, and abbreviated variable names
#> #   ¹​ingredient_concept_id, ²​ingredient, ³​n_records, ⁴​n_non_negative_days,
#> #   ⁵​n_negative_days, ⁶​proportion_negative_days, ⁷​minimum_drug_exposure_days,
#> #   ⁸​q05_drug_exposure_days, ⁹​q10_drug_exposure_days
all_checks$drugExposureDurationByConcept
#> # A tibble: 2 × 17
#> # Groups:   drug_concept_id, drug, ingredient_concept_id [2]
#>   drug_c…¹ drug  ingre…² ingre…³ n_rec…⁴ n_non…⁵ n_neg…⁶ propo…⁷ minim…⁸ q05_d…⁹
#>      <dbl> <chr>   <dbl> <chr>     <int>   <int>   <int>   <dbl>   <dbl>   <dbl>
#> 1  1127078 Acet… 1125315 Acetam…    2158    2158       0       0      14      15
#> 2 40162522 Acet… 1125315 Acetam…     312     312       0       0       1       1
#> # … with 7 more variables: q10_drug_exposure_days <dbl>,
#> #   q25_drug_exposure_days <dbl>, median_drug_exposure_days <dbl>,
#> #   q75_drug_exposure_days <dbl>, q90_drug_exposure_days <dbl>,
#> #   q95_drug_exposure_days <dbl>, maximum_drug_exposure_days <dbl>, and
#> #   abbreviated variable names ¹​drug_concept_id, ²​ingredient_concept_id,
#> #   ³​ingredient, ⁴​n_records, ⁵​n_non_negative_days, ⁶​n_negative_days,
#> #   ⁷​proportion_negative_days, ⁸​minimum_drug_exposure_days, …
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
