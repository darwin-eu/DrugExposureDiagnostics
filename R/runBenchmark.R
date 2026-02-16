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

#' Run benchmark for ExecuteSingleIngredient
#'
#' @param cdm CDMConnector reference object
#' @param ingredients vector of ingredients, by default: acetaminophen
#' @param subsetToConceptId vector of concept IDs of the ingredients
#' to filter. If a concept ID is positive it will be included, a negative one will be excluded.
#' If NULL (default), all concept IDs for an ingredient will be considered.
#' @param checks the checks to be executed, by default the missing values, the
#' exposure duration and the quantity. Possible options are "missing",
#' "exposureDuration", "type", "route", "sourceConcept", "daysSupply",
#' "verbatimEndDate", "dose", "sig", "quantity" and "diagnosticsSummary"
#' @param minCellCount minimum number of events to report- results
#' lower than this will be obscured. If 0 all results will be reported.
#' @param sampleSize the number of samples, default 10.000
#' @param tablePrefix The stem for the permanent tables that will
#' be created when running the diagnostics. Permanent tables will be created using
#' this prefix, and any existing tables that start with this will be at risk of
#' being dropped or overwritten. If NULL, temporary tables will be
#' @param earliestStartDate the earliest date from which a record can be included
#' @param verbose verbose, default FALSE
#' @param byConcept boolean argument whether to return results by Concept or overall only
#'
#' @return a tibble with the time taken and memory usage for different analysis per ingredient
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockDrugExposure()
#'
#' benchmarkResults <- runBenchmarkExecuteSingleIngredient(cdm)
#' }
runBenchmarkExecuteSingleIngredient <- function(cdm,
                                                ingredients = c(1125315),
                                                subsetToConceptId = NULL,
                                                checks = c("missing", "exposureDuration", "quantity"),
                                                minCellCount = 5,
                                                sampleSize = 10000,
                                                tablePrefix = NULL,
                                                earliestStartDate = "2010-01-01",
                                                verbose = FALSE,
                                                byConcept = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(ingredients, min.len = 1, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  result <- tibble::tibble(
    ingredient = numeric(),
    estimate_name = character(),
    estimate_value = numeric(),
    result_id = numeric()
  )

  currId <- 1

  for (ingredient in ingredients) {
    tictoc::tic()

    memUsed <- measureMemoryUsage(
      executeChecksSingleIngredient, cdm, ingredient, subsetToConceptId, checks, minCellCount,
      sampleSize, tablePrefix, earliestStartDate, verbose, byConcept
    )

    t <- tictoc::toc(quiet = TRUE)

    timeDiff <- as.numeric(t$toc - t$tic)

    result <- tibble::add_row(result, ingredient = ingredient, estimate_name = "Time taken (seconds)", estimate_value = timeDiff, result_id = currId)
    result <- tibble::add_row(result, ingredient = ingredient, estimate_name = "Memory used (MB)", estimate_value = memUsed, result_id = currId)

    currId <- currId + 1
  }

  result <- result %>%
    dplyr::mutate(
      result_id = as.integer(.data$result_id),
      cdm_name = omopgenerics::cdmName(cdm),
      result_type = "ExecuteSingleIngredient benchmark",
      package_name = "DrugExposureDiagnostics",
      package_version =
        as.character(utils::packageVersion("DrugExposureDiagnostics")),
      group_name = "overall",
      group_level = "overall",
      strata_name = "overall",
      strata_level = "overall",
      variable_name = "ingredient",
      variable_level = as.character(.data$ingredient),
      estimate_name = as.character(.data$estimate_name),
      estimate_type = "numeric",
      estimate_value = as.character(.data$estimate_value),
      additional_name = "overall",
      additional_level = "overall"
    ) %>%
    dplyr::select(-ingredient) %>%
    omopgenerics::newSummarisedResult()

  return(result)
}

measureMemoryUsage <- function(func, ...) {
  memUsed <- as.numeric(mem_change(func(...)))
  return(memUsed)
}

#' How much memory is currently used by R?
#'
#' R breaks down memory usage into Vcells (memory used by vectors) and
#' Ncells (memory used by everything else). However, neither this distinction
#' nor the "gc trigger" and "max used" columns are typically important. What
#' we're usually most interested in is the the first column: the total memory
#' used. This function wraps around \code{gc()} to return the total amount of
#' memory (in megabytes) currently used by R.
#'
#' @return Megabytes of ram used by R objects.
mem_used <- function() {
  show_bytes(sum(gc()[, 1] * c(node_size(), 8)))
}

node_size <- function() {
  bit <- 8L * .Machine$sizeof.pointer
  if (!(bit == 32L || bit == 64L)) {
    stop("Unknown architecture", call. = FALSE)
  }

  if (bit == 32L) 28L else 56L
}

#' Determine change in memory from running code
#'
#' @param code Code to evaluate.
#' @return Change in memory (in megabytes) before and after running code.
mem_change <- function(code) {
  start <- mem_used()

  expr <- substitute(code)
  eval(expr, parent.frame())
  rm(code, expr)

  show_bytes(mem_used() - start)
}

show_bytes <- function(x) {
  structure(x, class = "pryr_bytes")
}
