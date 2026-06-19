# DrugExposureDiagnostics ![](reference/figures/DrugExposureDiagnostics.png)

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
#> To cite package 'DrugExposureDiagnostics' in publications use:
#> 
#>   Inberg G, Burn E, Burkard T (2026). _DrugExposureDiagnostics:
#>   Diagnostics for OMOP Common Data Model Drug Records_. R package
#>   version 1.1.7, commit a2252b98a38603ab7c8342d3a91bd25a13ecf65b,
#>   <https://github.com/darwin-eu/DrugExposureDiagnostics>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {DrugExposureDiagnostics: Diagnostics for OMOP Common Data Model Drug Records},
#>     author = {Ger Inberg and Edward Burn and Theresa Burkard},
#>     year = {2026},
#>     note = {R package version 1.1.7, commit a2252b98a38603ab7c8342d3a91bd25a13ecf65b},
#>     url = {https://github.com/darwin-eu/DrugExposureDiagnostics},
#>   }
```

## Example use

``` r

library(DrugExposureDiagnostics)
library(CDMConnector)
library(dplyr)
```

First, connect to the database. The package is using the CDMConnector
object. You can create this object by passing a DBI connection and
schema names. When creating a DBIConnection using dbConnect, please
don’t forget to specify the bigint parameter (see below). If this is not
set, you could get a merge error when running DrugExposureDiagnostics.
More examples of how to connect your database using CDMConnector can be
found here:
<https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html>  
Here we use the internal mock database.

``` r

# conn <- DBI::dbConnect(
#  RPostgres::Postgres(),
#  dbname = dbname,
#  port = port,
#  host = host,
#  user = user,
#  password = password,
#  bigint = c("numeric")
# )
# cdm <- CDMConnector::cdmFromCon(
#   con = conn,
#   cdmSchema = "cdm schema name"
# )
cdm <- mockDrugExposure()
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
#> ℹ The following estimates will be calculated:
#> • daily_dose: count_missing, percentage_missing, mean, sd, q05, q25, median,
#>   q75, q95, min, max
#> ! Table is collected to memory as not all requested estimates are supported on
#>   the database side
#> → Start summary of data, at 2026-06-18 09:51:45.790348
#> 
#> ✔ Summary finished, at 2026-06-18 09:51:45.995946
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
#> $ drug_concept_id             <int> 40162522, 1127078, 40229134, 1127433, 4023…
#> $ drug                        <chr> "acetaminophen 325 MG Oral Tablet", "aceta…
#> $ ingredient_concept_id       <int> 1125315, 1125315, 1125315, 1125315, 112531…
#> $ ingredient                  <chr> "acetaminophen", "acetaminophen", "acetami…
#> $ n_records                   <int> 18, 19, 12, 13, 10, 14
#> $ n_patients                  <int> 15, 13, 11, 11, 9, 13
#> $ domain_id                   <chr> "Drug", "Drug", "Drug", "Drug", "Drug", "D…
#> $ vocabulary_id               <chr> "RxNorm", "RxNorm", "RxNorm", "RxNorm", "R…
#> $ concept_class_id            <chr> "Clinical Drug", "Clinical Drug", "Clinica…
#> $ standard_concept            <chr> "S", "S", "S", "S", "S", "S"
#> $ concept_code                <chr> "313782", "833036", "1043400", "1049221", …
#> $ valid_start_date            <date> 1970-01-01, 1970-01-01, 1970-01-01, 1970-0…
#> $ valid_end_date              <date> 2099-12-31, 2099-12-31, 2099-12-31, 2099-1…
#> $ invalid_reason              <chr> NA, NA, NA, NA, NA, NA
#> $ amount_value                <dbl> 300, NA, 300, 100, 200, 100
#> $ amount_unit_concept_id      <int> 8576, NA, 8576, 8576, 8576, 8576
#> $ numerator_value             <dbl> NA, 3, NA, NA, NA, NA
#> $ numerator_unit_concept_id   <int> NA, 8576, NA, NA, NA, NA
#> $ numerator_unit              <chr> NA, NA, NA, NA, NA, NA
#> $ denominator_value           <dbl> NA, 1, NA, NA, NA, NA
#> $ denominator_unit_concept_id <int> NA, 8576, NA, NA, NA, NA
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
#> 1        40162522 acetaminophen 325 MG Oral Tablet              
#> 2         1127078 acetaminophen 750 MG / Hydrocodone Bitartrate 
#> 3        40229134 acetaminophen 21.7 MG/ML / Dextromethorphan   
#> 4         1127433 acetaminophen 325 MG / Oxycodone Hydrochloride
#> 5        40231925 acetaminophen 325 MG / Hydrocodone Bitartrate 
#> 6        19133768 acetaminophen 160 MG Oral Tablet
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
#>  1        19133768 acetamin…               1125315 acetamino… n_missi…         8
#>  2        19133768 acetamin…               1125315 acetamino… n_missi…         8
#>  3        19133768 acetamin…               1125315 acetamino… n_missi…         8
#>  4        19133768 acetamin…               1125315 acetamino… n_missi…         8
#>  5        19133768 acetamin…               1125315 acetamino… n_missi…         8
#>  6        19133768 acetamin…               1125315 acetamino… n_missi…         8
#>  7        19133768 acetamin…               1125315 acetamino… n_missi…         8
#>  8        19133768 acetamin…               1125315 acetamino… n_missi…         8
#>  9        19133768 acetamin…               1125315 acetamino… n_missi…         8
#> 10        19133768 acetamin…               1125315 acetamino… n_missi…         8
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
