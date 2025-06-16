# DrugExposureDiagnostics (development version)

# DrugExposureDiagnostics 1.1.4
* Fix error in executeChecks (#312)
* Fix running daysBetween check on certain db (#313)
* Fix loading results in shiny app (#316)

# DrugExposureDiagnostics 1.1.3
* Add time in between check
* Integration with Darwin shiny modules

# DrugExposureDiagnostics 1.1.2
* Add option to run for an exposure type
* Fix rounding issue drug_type_concept_id
* Add error if wrong check is passed
* Fix earliestStartDate NULL behaviour
* Move shiny to suggests

# DrugExposureDiagnostics 1.1.1
* Update shiny app: fix NA in boxplots and add dropdown drugs-missing
* CDMConnector v2 changes
* Add shiny modules

# DrugExposureDiagnostics 1.1.0
* Add params to executeChecks so it will write results to disk, no need to separately call writeResultToDisk

# DrugExposureDiagnostics 1.0.10
* Fix sample is null bug
* Fix too many rows in diagnostics summary
* Add metadata section to shiny app

# DrugExposureDiagnostics 1.0.9
* Integrate shiny app in package
* Fix bug in obscuring results
* Use sample size in dose check

# DrugExposureDiagnostics 1.0.8
* Skip unavailable concepts (#253 and #254)

# DrugExposureDiagnostics 1.0.7
* Updates related to newest release of DrugUtilisation

# DrugExposureDiagnostics 1.0.6
* Fixed bug in generating diagnosticsSummary

# DrugExposureDiagnostics 1.0.5
* Use dailyDoseCoverage function from package DrugUtilisation
* Remove Eunomia dependency
* Remove histogram output from checks

# DrugExposureDiagnostics 1.0.4
* Add option to exclude drug concept ids
* Performance improvement
* Update license

# DrugExposureDiagnostics 1.0.3
* Compatibility with latest CDMConnector
* Fix sample bug
* Remove deprecated functions
* Add links in DESCRIPTION

# DrugExposureDiagnostics 1.0.2
* Add hex sticker
* Add argument byConcept to executeChecks

# DrugExposureDiagnostics 1.0.1
* Fix unit test

# DrugExposureDiagnostics 1.0.0
* Obscure patient counts in concept/diagnostics summary
* Update documentation regarding default checks
* Major version update: no major interface changes expected

# DrugExposureDiagnostics 0.4.7
* Set the default checks to: "missing", "exposureDuration" and "quantity"
* Reduce sample size to 10.000
* Make compatible with duckdb v0.9

# DrugExposureDiagnostics 0.4.6
* Fixed bug in obscureCounts
* Add PaRe report vignette

# DrugExposureDiagnostics 0.4.5
* Fixed bug in obscureCounts

# DrugExposureDiagnostics 0.4.4
* Fixed bug when running subset of checks
* Fixed bug when applying minCellCount
* Fixed bug that caused invalid counts

# DrugExposureDiagnostics 0.4.3
* Update CDMConnector to 1.0.0
* Add subsetToConceptId to executeChecks function

# DrugExposureDiagnostics 0.4.2
* Update diagnostics summary for the onboarding

# DrugExposureDiagnostics 0.4.1
* Update CDMConnector to 0.5.0 

# DrugExposureDiagnostics 0.4.0
* Initial CRAN release March 8, 2023
* Added a `NEWS.md` file to track changes to the package.
