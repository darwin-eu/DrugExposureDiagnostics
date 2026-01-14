# Copyright 2024 DARWIN EU®
#
# This file is part of DrugExposureDiagnostics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Execute given checks on Drug Exposure.
#'
#' @param cdm CDMConnector reference object
#' @param ingredients vector of ingredients, by default: acetaminophen
#' @param subsetToConceptId vector of concept IDs of the ingredients
#'  to filter. If a concept ID is positive it will be included, a negative one will be excluded.
#'  If NULL, all concept IDs for an ingredient will be considered.
#' @param checks the checks to be executed, by default the missing values, the
#' exposure duration and the quantity. Possible options are "missing",
#' "exposureDuration", "type", "route", "sourceConcept", "daysSupply",
#' "verbatimEndDate", "dose", "sig", "quantity", "daysBetween" and "diagnosticsSummary"
#' @param minCellCount minimum number of events to report- results
#' lower than this will be obscured. If 0 all results will be reported.
#' @param sample the number of samples, default 10.000
#' @param tablePrefix The stem for the permanent tables that will
#' be created when running the diagnostics. Permanent tables will be created using
#' this prefix, and any existing tables that start with this will be at risk of
#' being dropped or overwritten. If NULL, temporary tables will be
#' used throughout.
#' @param earliestStartDate the earliest date from which a record can be included
#' @param verbose verbose, default FALSE
#' @param byConcept boolean argument whether to return results by Concept or overall only
#' @param exposureTypeId id of the drug exposure type to be filtered on (e.g. only prescribed).
#' By default all record types will be taken into account.
#' @param outputFolder folder to write to. If NULL, results will not be written to file
#' @param databaseId database identifier
#' @param filename output file name, if NULL it will be equal to databaseId
#'
#' @return named list with results
#' @export
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here ")
#' cdm <- CDMConnector::cdmFromCon(
#'   con = db,
#'   cdmSchema = "cdm schema name"
#' )
#' result <- executeChecks(
#'   cdm = cdm,
#'   ingredients = c(1125315)
#' )
#' }
executeChecks <- function(cdm,
                          ingredients = c(1125315),
                          subsetToConceptId = NULL,
                          checks = c("missing", "exposureDuration", "quantity"),
                          minCellCount = 5,
                          sample = 10000,
                          tablePrefix = NULL,
                          earliestStartDate = "2010-01-01",
                          verbose = FALSE,
                          byConcept = TRUE,
                          exposureTypeId = NULL,
                          outputFolder = NULL,
                          databaseId = CDMConnector::cdmName(cdm),
                          filename = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  checkmate::assertNumeric(ingredients, min.len = 1, add = errorMessage)
  checkmate::assertNumeric(subsetToConceptId, add = errorMessage, null.ok = TRUE)
  validateChecks(checks, messageStore = errorMessage)
  checkmate::assertTRUE(is.numeric(minCellCount), add = errorMessage)
  checkmate::assertNumeric(sample, len = 1, add = errorMessage, null.ok = TRUE)
  checkSampleMinCellCount(sample, minCellCount, messageStore = errorMessage)
  checkmate::assertCharacter(tablePrefix, len = 1, add = errorMessage, null.ok = TRUE)
  checkmate::assertDate(as.Date(earliestStartDate), add = errorMessage, null.ok = FALSE)
  checkLogical(verbose, messageStore = errorMessage)
  checkLogical(byConcept, messageStore = errorMessage)
  if (!is.null(outputFolder)) {
    checkmate::assertPathForOutput(outputFolder, overwrite = TRUE, add = errorMessage)
  }
  checkmate::assertCharacter(databaseId, len = 1, add = errorMessage)
  checkmate::assertCharacter(filename, len = 1, null.ok = TRUE, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  resultList <- vector(mode = "list", length = length(ingredients))
  for (i in seq_along(ingredients)) {
    ingredient <- ingredients[i]
    ingredientResult <- NULL
    tryCatch(
      {
        ingredientResult <- executeChecksSingleIngredient(
          cdm = cdm,
          ingredient = ingredient,
          subsetToConceptId = subsetToConceptId,
          checks = checks,
          minCellCount = minCellCount,
          sampleSize = sample,
          tablePrefix = tablePrefix,
          earliestStartDate = earliestStartDate,
          verbose = verbose,
          byConcept = byConcept,
          exposureTypeId = exposureTypeId
        )

        # write result
        if (!is.null(outputFolder)) {
          writeIngredientResultToDisk(resultList = ingredientResult,
                                      databaseId = databaseId,
                                      outputFolder = outputFolder,
                                      clearDBDir = (i == 1))
        }
      },
      error = function(e) {
        warning(e)
      }
    )
    resultList[[i]] <- ingredientResult
  }
  resultList <- do.call(Map, c(f = rbind, Filter(Negate(is.null), resultList)))

  # add metadata
  metaData <- CDMConnector::snapshot(cdm) %>%
    dplyr::select(-dplyr::one_of("cdm_data_hash")) %>%
    dplyr::mutate(package_version = as.character(utils::packageVersion("DrugExposureDiagnostics")))
  resultList <- append(resultList, list("metadata" = metaData))

  if (!is.null(outputFolder)) {
    writeZipToDisk(metaData, databaseId, outputFolder, filename)
  }
  return(resultList)
}

#' Execute given checks on Drug Exposure for a single ingredient.
#'
#' @param cdm CDMConnector reference object
#' @param ingredient ingredient, by default: acetaminophen
#' @param subsetToConceptId vector of concept IDs of the ingredients
#'  to filter. If a concept ID is positive it will be included, a negative one will be excluded.
#'  If NULL, all concept IDs for an ingredient will be considered.
#' @param checks the checks to be executed, by default the missing values, the
#' exposure duration and the quantity.
#' @param minCellCount minimum number of events to report- results
#' lower than this will be obscured. If 0 all results will be reported.
#' @param sampleSize the number of samples, default 10.000
#' @param tablePrefix The stem for the permanent tables that will
#' be created when running the diagnostics. Permanent tables will be created using
#' this prefix, and any existing tables that start with this will be at risk of
#' being dropped or overwritten. If NULL, temporary tables will be
#' used throughout.
#' @param earliestStartDate the earliest date from which a record can be included
#' @param verbose verbose, default FALSE
#' @param byConcept boolean argument whether to return results by Concept or overall only
#' @param exposureTypeId id of the drug exposure type to be filtered on (e.g. only prescribed).
#' By default all record types will be taken into account.
#'
#' @return named list with results
executeChecksSingleIngredient <- function(cdm,
                                          ingredient = 1125315,
                                          subsetToConceptId = NULL,
                                          checks = c("missing", "exposureDuration", "quantity"),
                                          minCellCount = 5,
                                          sampleSize = 10000,
                                          tablePrefix = NULL,
                                          earliestStartDate = "2010-01-01",
                                          verbose = FALSE,
                                          byConcept = FALSE,
                                          exposureTypeId = NULL) {
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  checkIsIngredient(cdm = cdm, conceptId = ingredient, messageStore = errorMessage)
  checkmate::assertNumeric(subsetToConceptId, add = errorMessage, null.ok = TRUE)
  checkmate::assertTRUE(is.numeric(minCellCount), add = errorMessage)
  checkmate::assertNumeric(sampleSize, len = 1, add = errorMessage, null.ok = TRUE)
  checkSampleMinCellCount(sampleSize, minCellCount, messageStore = errorMessage)
  checkmate::assertCharacter(tablePrefix, len = 1, add = errorMessage, null.ok = TRUE)
  checkmate::assertDate(as.Date(earliestStartDate), add = errorMessage, null.ok = FALSE)
  checkLogical(verbose, messageStore = errorMessage)
  checkLogical(byConcept, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (verbose == TRUE) {
    start <- Sys.time()
    message(glue::glue("Progress: getting descendant concepts of ingredient ({ingredient}) used in database"))
  }

  cdm[["ingredient_concepts"]] <- ingredientDescendantsInDb(
    cdm = cdm,
    ingredient = ingredient,
    verbose = verbose,
    tablePrefix = tablePrefix
  ) %>%
    dplyr::compute(name = "ingredient_concepts")

  if (!is.null(subsetToConceptId)) {
    includedConceptIds <- as.numeric(subsetToConceptId[subsetToConceptId > 0])
    excludedConceptIds <- as.numeric(abs(subsetToConceptId[subsetToConceptId < 0]))
    if (!identical(intersect(includedConceptIds, excludedConceptIds), numeric(0))) {
      stop("Same concept id's can't be included and excluded in subsetToConceptId")
    }
    if (!identical(includedConceptIds, numeric(0))) {
      cdm[["ingredient_concepts"]] <- cdm[["ingredient_concepts"]] %>%
        dplyr::filter(.data$concept_id %in% .env$includedConceptIds)
    }
    if (!identical(excludedConceptIds, numeric(0))) {
      cdm[["ingredient_concepts"]] <- cdm[["ingredient_concepts"]] %>%
        dplyr::filter(!(.data$concept_id %in% .env$excludedConceptIds))
    }
    cdm[["ingredient_concepts"]] <- cdm[["ingredient_concepts"]] %>%
      dplyr::compute()
  }

  if (verbose == TRUE) {
    start <- printDurationAndMessage("Progress: getting drug records for ingredient", start)
  }
  cdm[["ingredient_drug_records"]] <- getDrugRecords(
    cdm = cdm,
    ingredient = ingredient,
    includedConceptsTable = "ingredient_concepts",
    exposureTypeId = exposureTypeId,
    tablePrefix = tablePrefix
  ) %>%
    dplyr::compute(name = "ingredient_drug_records")

  if (verbose == TRUE) {
    start <- printDurationAndMessage("Progress: get concepts used", start)
  }

  conceptsUsed <- cdm[["ingredient_drug_records"]] %>%
    dplyr::group_by(.data$drug_concept_id) %>%
    dplyr::summarise(
      n_records = as.integer(dplyr::n()),
      n_patients = as.integer(dplyr::n_distinct(.data$person_id))
    ) %>%
    dplyr::inner_join(cdm[["ingredient_concepts"]],
      by = c("drug_concept_id" = "concept_id")
    ) %>%
    dplyr::rename("drug" = "concept_name") %>%
    dplyr::relocate("drug", .after = "drug_concept_id") %>%
    dplyr::relocate("ingredient_concept_id", .after = "drug") %>%
    dplyr::relocate("ingredient", .after = "ingredient_concept_id") %>%
    dplyr::relocate("numerator_unit", .after = "numerator_unit_concept_id") %>%
    dplyr::relocate("denominator_unit", .after = "denominator_unit_concept_id") %>%
    dplyr::collect()

  # sample
  # the ingredient overview is for all records
  # all other checks for sampled records
  if (!is.null(sampleSize)) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: sampling drug records", start)
    }
    if (!is.null(earliestStartDate)) {
      if (dplyr::pull(dplyr::tally(dplyr::filter(cdm[["ingredient_drug_records"]], .data$drug_exposure_start_date > .env$earliestStartDate)), .data$n) < sampleSize) {
        message("population after earliestStartDate smaller than sample, sampling ignored")
        cdm[["ingredient_drug_records"]] <- cdm[["ingredient_drug_records"]] %>%
          dplyr::filter(.data$drug_exposure_start_date > .env$earliestStartDate) %>%
          dplyr::compute()
      } else {
        cdm[["ingredient_drug_records"]] <- cdm[["ingredient_drug_records"]] %>%
          dplyr::filter(.data$drug_exposure_start_date > .env$earliestStartDate) %>%
          dplyr::slice_sample(n = sampleSize) %>%
          dplyr::compute()
      }
    } else {
      cdm[["ingredient_drug_records"]] <- cdm[["ingredient_drug_records"]] %>%
        dplyr::slice_sample(n = sampleSize) %>%
        dplyr::compute()
    }
  } else {
    if (!is.null(earliestStartDate)) {
      cdm[["ingredient_drug_records"]] <- cdm[["ingredient_drug_records"]] %>%
        dplyr::filter(.data$drug_exposure_start_date > .env$earliestStartDate) %>%
        dplyr::compute()
    }

    sampleSize <- cdm$ingredient_drug_records %>%
      dplyr::tally() %>%
      dplyr::pull(.data$n)
  }

  missingValuesOverall <- missingValuesByConcept <- NULL
  if ("missing" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check drugsMissing", start)
    }

    missingValuesOverall <- getDrugMissings(cdm, "ingredient_drug_records",
      byConcept = FALSE, sampleSize = sampleSize
    ) %>% dplyr::collect()
    if (isTRUE(byConcept)) {
      missingValuesByConcept <- getDrugMissings(cdm, "ingredient_drug_records",
        byConcept = byConcept, sampleSize = sampleSize
      ) %>% dplyr::collect()
    }
  }

  drugExposureDurationOverall <- drugExposureDurationByConcept <- NULL
  if ("exposureDuration" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check ExposureDuration", start)
    }

    drugExposureDurationOverall <- summariseDrugExposureDuration(cdm,
      "ingredient_drug_records",
      byConcept = FALSE, sampleSize = sampleSize
    ) %>% dplyr::collect()
    if (isTRUE(byConcept)) {
      drugExposureDurationByConcept <- summariseDrugExposureDuration(cdm,
        "ingredient_drug_records",
        byConcept = byConcept, sampleSize = sampleSize
      ) %>% dplyr::collect()
    }
  }

  drugTypesOverall <- drugTypesByConcept <- NULL
  if ("type" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check drugTypes", start)
    }

    drugTypesOverall <- getDrugTypes(cdm, "ingredient_drug_records",
      byConcept = FALSE, sampleSize = sampleSize
    ) %>% dplyr::collect()
    if (isTRUE(byConcept)) {
      drugTypesByConcept <- getDrugTypes(cdm, "ingredient_drug_records",
        byConcept = byConcept, sampleSize = sampleSize
      ) %>% dplyr::collect()
    }
  }

  drugRoutesOverall <- drugRoutesByConcept <- NULL
  if ("route" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check drugRoutes", start)
    }

    drugRoutesOverall <- getDrugRoutes(cdm, "ingredient_drug_records",
      byConcept = FALSE, sampleSize = sampleSize
    ) %>% dplyr::collect()
    if (isTRUE(byConcept)) {
      drugRoutesByConcept <- getDrugRoutes(cdm, "ingredient_drug_records",
        byConcept = byConcept, sampleSize = sampleSize
      ) %>% dplyr::collect()
    }
  }

  drugSourceConcepts <- NULL
  if ("sourceConcept" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check drugSourceConcepts", start)
    }

    drugSourceConcepts <- getDrugSourceConcepts(cdm, "ingredient_drug_records", sampleSize = sampleSize) %>% dplyr::collect()
  }

  drugDaysSupply <- drugDaysSupplyByConcept <- NULL
  if ("daysSupply" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check drugDaysSupply", start)
    }

    drugDaysSupply <- checkDaysSupply(cdm, "ingredient_drug_records", byConcept = FALSE, sampleSize = sampleSize) %>% dplyr::collect()
    if (isTRUE(byConcept)) {
      drugDaysSupplyByConcept <- checkDaysSupply(cdm, "ingredient_drug_records", byConcept = byConcept, sampleSize = sampleSize) %>% dplyr::collect()
    }
  }

  drugVerbatimEndDate <- drugVerbatimEndDateByConcept <- NULL
  if ("verbatimEndDate" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check drugVerbatimEndDate", start)
    }

    drugVerbatimEndDate <- checkVerbatimEndDate(cdm, "ingredient_drug_records", byConcept = FALSE, sampleSize = sampleSize) %>% dplyr::collect()
    if (isTRUE(byConcept)) {
      drugVerbatimEndDateByConcept <- checkVerbatimEndDate(cdm, "ingredient_drug_records", byConcept = byConcept, sampleSize = sampleSize) %>% dplyr::collect()
    }
  }

  drugDose <- NULL
  if ("dose" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check drugDose", start)
    }
    drugDose <- checkDrugDose(cdm, ingredientConceptId = ingredient, sampleSize = sampleSize, minCellCount = minCellCount) %>% dplyr::collect()
  }

  drugSig <- drugSigByConcept <- NULL
  if ("sig" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check drugSig", start)
    }

    drugSig <- checkDrugSig(cdm, "ingredient_drug_records", byConcept = FALSE, sampleSize = sampleSize) %>% dplyr::collect()
    if (isTRUE(byConcept)) {
      drugSigByConcept <- checkDrugSig(cdm, "ingredient_drug_records", byConcept = byConcept, sampleSize = sampleSize) %>% dplyr::collect()
    }
  }

  drugQuantity <- drugQuantityByConcept <- NULL
  if ("quantity" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check drugQuantity", start)
    }

    drugQuantity <- summariseQuantity(cdm, "ingredient_drug_records", byConcept = FALSE, sampleSize = sampleSize) %>% dplyr::collect()
    if (isTRUE(byConcept)) {
      drugQuantityByConcept <- summariseQuantity(cdm, "ingredient_drug_records", byConcept = byConcept, sampleSize = sampleSize) %>% dplyr::collect()
    }
  }

  drugTimeBetween <- drugTimeBetweenByConcept <- NULL
  if ("daysBetween" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check time between drug records", start)
    }

    drugTimeBetween <- summariseTimeBetween(cdm, "ingredient_drug_records", byConcept = FALSE, sampleSize = sampleSize) %>% dplyr::collect()
    if (isTRUE(byConcept)) {
      drugTimeBetweenByConcept <- summariseTimeBetween(cdm, "ingredient_drug_records", byConcept = byConcept, sampleSize = sampleSize) %>% dplyr::collect()
    }
  }

  if (!is.null(tablePrefix)) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Cleaning up tables", start)
    }
    tables <- CDMConnector::listTables(attr(cdm, "dbcon"),
      schema = attr(cdm, "write_schema")
    )
    tables <- tables[grepl(tablePrefix, tables)]
    CDMConnector::dropTable(cdm, tables)
  }

  if (verbose == TRUE) {
    start <- printDurationAndMessage("Finished", start)
  }

  result <- list(
    "conceptSummary" = conceptsUsed,
    "missingValuesOverall" = missingValuesOverall,
    "missingValuesByConcept" = missingValuesByConcept,
    "drugExposureDurationOverall" = drugExposureDurationOverall,
    "drugExposureDurationByConcept" = drugExposureDurationByConcept,
    "drugTypesOverall" = drugTypesOverall,
    "drugTypesByConcept" = drugTypesByConcept,
    "drugRoutesOverall" = drugRoutesOverall,
    "drugRoutesByConcept" = drugRoutesByConcept,
    "drugSourceConceptsOverall" = drugSourceConcepts,
    "drugDaysSupply" = drugDaysSupply,
    "drugDaysSupplyByConcept" = drugDaysSupplyByConcept,
    "drugVerbatimEndDate" = drugVerbatimEndDate,
    "drugVerbatimEndDateByConcept" = drugVerbatimEndDateByConcept,
    "drugDose" = drugDose,
    "drugSig" = drugSig,
    "drugSigByConcept" = drugSigByConcept,
    "drugQuantity" = drugQuantity,
    "drugQuantityByConcept" = drugQuantityByConcept,
    "drugTimeBetween" = drugTimeBetween,
    "drugTimeBetweenByConcept" = drugTimeBetweenByConcept
  )

  # add summary table
  if ("diagnosticsSummary" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: create diagnosticsSummary", start)
    }
    result[["diagnosticsSummary"]] <- summariseChecks(resultList = result)
  }

  result <- Filter(Negate(is.null), sapply(names(result),
    FUN = function(tableName) {
      table <- result[[tableName]]
      if (!is.null(table) && !grepl("Dose", tableName)) {
        table <- obscureCounts(
          table = table,
          tableName = tableName,
          minCellCount = minCellCount,
          substitute = NA
        )
      }
      return(table)
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  ))

  return(result)
}
