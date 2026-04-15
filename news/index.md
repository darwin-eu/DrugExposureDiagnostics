# Changelog

## DrugExposureDiagnostics (development version)

## DrugExposureDiagnostics 1.1.7

- Suppress results diagnostics summary
  ([\#332](https://github.com/darwin-eu/DrugExposureDiagnostics/issues/332))
- Update ingredient concept plot section
  ([\#338](https://github.com/darwin-eu/DrugExposureDiagnostics/issues/338))

## DrugExposureDiagnostics 1.1.6

CRAN release: 2026-02-16

- Make compatible with newest dplyr
- Remove pryr dependency

## DrugExposureDiagnostics 1.1.5

CRAN release: 2025-09-16

- Make compatible with newest CDMConnector
  ([\#327](https://github.com/darwin-eu/DrugExposureDiagnostics/issues/327))
- Remove additional performance columns when merging results in shiny
  app
  ([\#325](https://github.com/darwin-eu/DrugExposureDiagnostics/issues/325))

## DrugExposureDiagnostics 1.1.4

CRAN release: 2025-06-16

- Fix error in executeChecks
  ([\#312](https://github.com/darwin-eu/DrugExposureDiagnostics/issues/312))
- Fix running daysBetween check on certain db
  ([\#313](https://github.com/darwin-eu/DrugExposureDiagnostics/issues/313))
- Fix loading results in shiny app
  ([\#316](https://github.com/darwin-eu/DrugExposureDiagnostics/issues/316))

## DrugExposureDiagnostics 1.1.3

CRAN release: 2025-05-28

- Add time in between check
- Integration with Darwin shiny modules

## DrugExposureDiagnostics 1.1.2

CRAN release: 2025-03-13

- Add option to run for an exposure type
- Fix rounding issue drug_type_concept_id
- Add error if wrong check is passed
- Fix earliestStartDate NULL behaviour
- Move shiny to suggests

## DrugExposureDiagnostics 1.1.1

CRAN release: 2025-02-06

- Update shiny app: fix NA in boxplots and add dropdown drugs-missing
- CDMConnector v2 changes
- Add shiny modules

## DrugExposureDiagnostics 1.1.0

CRAN release: 2024-12-22

- Add params to executeChecks so it will write results to disk, no need
  to separately call writeResultToDisk

## DrugExposureDiagnostics 1.0.10

CRAN release: 2024-11-22

- Fix sample is null bug
- Fix too many rows in diagnostics summary
- Add metadata section to shiny app

## DrugExposureDiagnostics 1.0.9

CRAN release: 2024-09-25

- Integrate shiny app in package
- Fix bug in obscuring results
- Use sample size in dose check

## DrugExposureDiagnostics 1.0.8

CRAN release: 2024-09-10

- Skip unavailable concepts
  ([\#253](https://github.com/darwin-eu/DrugExposureDiagnostics/issues/253)
  and
  [\#254](https://github.com/darwin-eu/DrugExposureDiagnostics/issues/254))

## DrugExposureDiagnostics 1.0.7

CRAN release: 2024-07-30

- Updates related to newest release of DrugUtilisation

## DrugExposureDiagnostics 1.0.6

CRAN release: 2024-07-02

- Fixed bug in generating diagnosticsSummary

## DrugExposureDiagnostics 1.0.5

CRAN release: 2024-06-12

- Use dailyDoseCoverage function from package DrugUtilisation
- Remove Eunomia dependency
- Remove histogram output from checks

## DrugExposureDiagnostics 1.0.4

CRAN release: 2024-03-18

- Add option to exclude drug concept ids
- Performance improvement
- Update license

## DrugExposureDiagnostics 1.0.3

CRAN release: 2024-01-31

- Compatibility with latest CDMConnector
- Fix sample bug
- Remove deprecated functions
- Add links in DESCRIPTION

## DrugExposureDiagnostics 1.0.2

CRAN release: 2024-01-14

- Add hex sticker
- Add argument byConcept to executeChecks

## DrugExposureDiagnostics 1.0.1

CRAN release: 2023-11-06

- Fix unit test

## DrugExposureDiagnostics 1.0.0

CRAN release: 2023-10-25

- Obscure patient counts in concept/diagnostics summary
- Update documentation regarding default checks
- Major version update: no major interface changes expected

## DrugExposureDiagnostics 0.4.7

CRAN release: 2023-10-16

- Set the default checks to: “missing”, “exposureDuration” and
  “quantity”
- Reduce sample size to 10.000
- Make compatible with duckdb v0.9

## DrugExposureDiagnostics 0.4.6

CRAN release: 2023-08-16

- Fixed bug in obscureCounts
- Add PaRe report vignette

## DrugExposureDiagnostics 0.4.5

- Fixed bug in obscureCounts

## DrugExposureDiagnostics 0.4.4

CRAN release: 2023-07-16

- Fixed bug when running subset of checks
- Fixed bug when applying minCellCount
- Fixed bug that caused invalid counts

## DrugExposureDiagnostics 0.4.3

CRAN release: 2023-06-13

- Update CDMConnector to 1.0.0
- Add subsetToConceptId to executeChecks function

## DrugExposureDiagnostics 0.4.2

- Update diagnostics summary for the onboarding

## DrugExposureDiagnostics 0.4.1

CRAN release: 2023-03-13

- Update CDMConnector to 0.5.0

## DrugExposureDiagnostics 0.4.0

CRAN release: 2023-03-08

- Initial CRAN release March 8, 2023
- Added a `NEWS.md` file to track changes to the package.
