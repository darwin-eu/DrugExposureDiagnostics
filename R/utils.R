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

#' Check the database type.
#'
#' @param cdm CDMConnector reference object
#' @param type type of the database, default cdm_reference
#' @param messageStore checkmate collection
#'
checkDbType <- function(cdm, type = "cdm_reference", messageStore) {
  dbInheritsCheck <- inherits(cdm, type)
  checkmate::assertTRUE(dbInheritsCheck,
    add = messageStore
  )
  if (!isTRUE(dbInheritsCheck)) {
    messageStore$push(glue::glue("- cdm must be a CDMConnector {type} object"))
  }
}

#' Check that the sample is bigger than the mincellcount
#'
#' @param sampleSize sample size for sampling
#' @param minCellCount minimum cell count below which to obsure results
#' @param messageStore checkmate collection
#'
checkSampleMinCellCount <- function(sampleSize, minCellCount, messageStore) {
  if (!is.null(sampleSize)) {
    sampleBiggerMinCellCount <- sampleSize > minCellCount
    checkmate::assertTRUE(sampleBiggerMinCellCount, add = messageStore)
    if (!isTRUE(sampleBiggerMinCellCount)) {
      messageStore$push("Sample size needs to be bigger than minimum cell count")
    }
  }
}

#' Check if given object is a boolean.
#'
#' @param input the input
#' @param messageStore checkmate collection
#' @param null.ok if value null is allowed
#'
checkLogical <- function(input, messageStore, null.ok = TRUE) {
  checkmate::assert_logical(input,
    add = messageStore,
    null.ok = null.ok
  )
}

#' Check if given table exists in cdm.
#'
#' @param cdm CDMConnector reference object
#' @param tableName checkmate collection
#' @param messageStore the message store
#'
checkTableExists <- function(cdm, tableName, messageStore) {
  table_exists <- inherits(cdm[[tableName]], "tbl_dbi")
  checkmate::assertTRUE(table_exists, add = messageStore)
  if (!isTRUE(table_exists)) {
    messageStore$push(glue::glue("- {tableName} is not found"))
  }
}

#' Check is an ingredient
#'
#' @param cdm CDMConnector reference object
#' @param conceptId ingredient concept id to check
#' @param messageStore checkmate collection
#'
checkIsIngredient <- function(cdm, conceptId, messageStore) {
  ingredientConcepts <- cdm$concept %>%
    dplyr::filter(.data$concept_id == .env$conceptId) %>%
    dplyr::select("concept_class_id") %>%
    dplyr::collect()

  ingredientCheckResult <- TRUE
  ingredientCheckMessage <- NULL
  if (nrow(ingredientConcepts) > 0) {
    ingredientCheckResult <- all(ingredientConcepts %>%
      dplyr::pull() == "Ingredient")
    if (!isTRUE(ingredientCheckResult)) {
      ingredientCheckMessage <- glue::glue("- ingredient concept ({conceptId}) does not have concept_class_id of Ingredient")
    }
  } else {
    ingredientCheckResult <- FALSE
    ingredientCheckMessage <- glue::glue("- ingredient concept ({conceptId}) could not be found in concept table")
  }
  checkmate::assertTRUE(ingredientCheckResult,
    add = messageStore
  )
  if (!isTRUE(ingredientCheckResult)) {
    messageStore$push(ingredientCheckMessage)
  }
}

#' Check ingredient is present in given table
#'
#' @param cdm CDMConnector reference object
#' @param conceptId ingredient concept id to check
#' @param tableName name of the table to check
#' @param messageStore checkmate collection
#'
checkIngredientInTable <- function(cdm, conceptId, tableName, messageStore) {
  ingredientTable <- cdm[[tableName]] %>%
    dplyr::filter(.data$ingredient_concept_id == .env$conceptId)
  ingredientTableCount <- ingredientTable %>%
    dplyr::tally() %>%
    dplyr::pull("n")
  if (ingredientTableCount == 0) {
    message <- glue::glue("- ingredient concept ({conceptId}) could not be found in {tableName} table")
    messageStore$push(message)
  }
}

#' Compute the difference in days between 2 variables in a database table.
#'
#' @param cdm CDMConnector reference object
#' @param tableName the table name
#' @param startDateCol the start date column name
#' @param endDateCol the end date column name
#' @param colName the result column name
#'
#' @return the table with as new column the duration
getDuration <- function(cdm,
                        tableName = "drug_exposure",
                        startDateCol = "drug_exposure_start_date",
                        endDateCol = "drug_exposure_end_date",
                        colName = "duration") {
  cdm[[tableName]] %>%
    dplyr::mutate(
      !!colName := !!CDMConnector::datediff(
        start = startDateCol,
        end = endDateCol,
        interval = "day"
      ) + 1
    )
}

#' Print duration from start to now and print it as well as new status message
#'
#' @param message the message
#' @param start the start time
#'
#' @return the current time
printDurationAndMessage <- function(message, start) {
  currentTime <- Sys.time()
  duration <- abs(as.numeric(currentTime - start, units = "secs"))
  message(glue::glue("Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"))
  message(message)
  return(currentTime)
}

#' Store the given input in a remote database table. It will be stored either in a
#' permanent table or a temporary table depending on tablePrefix.
#'
#' @param table the input table
#' @param tablePrefix The stem for the permanent tables that will
#' be created when running the diagnostics. Permanent tables will be created using
#' this prefix, and any existing tables that start with this will be at risk of
#' being dropped or overwritten. If NULL, temporary tables will be
#' used throughout.
#' @param tableName the input table
#' @param cdm cdm reference object
#' @param overwrite if the table should be overwritten (default TRUE).
#'
#' @return reference to the table
computeDBQuery <- function(table, tablePrefix, tableName, cdm, overwrite = TRUE) {
  if (is.null(tablePrefix)) {
    table <- table %>%
      dplyr::compute()
  } else {
    table <- table %>%
      dplyr::compute(
        name = paste0(tablePrefix, tableName),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE
      )
  }
  return(table)
}

#' Write a result to a file on disk.
#'
#' @param result check result
#' @param resultName name of the result
#' @param databaseId database identifier
#' @param dbDir output directory for current db
#'
#' @return No return value, called for side effects
#'
#' @examples
#' \dontrun{
#' resultList <- list("mtcars" = mtcars)
#' result <- writeZipToDisk(
#'   metadata = metadata,
#'   databaseId = "mtcars",
#'   outputFolder = here::here()
#' )
#' }
writeFile <- function(result, resultName, databaseId, dbDir) {
  result <- dplyr::bind_cols(
    database_id = databaseId,
    result
  )

  fileName <- file.path(
    dbDir,
    paste0(resultName, ".csv")
  )
  # if file exist, append new result
  if (file.exists(fileName)) {
    oldResult <- tibble::as_tibble(utils::read.csv(fileName))
    if (nrow(oldResult) > 0) {
      result <- rbind(oldResult,
                      result)
    }
  }
  utils::write.csv(result,
                   file = fileName,
                   row.names = FALSE
  )
}

#' Write (ingredient) diagnostics results on disk in given output folder.
#'
#' @param resultList named list with results
#' @param databaseId database identifier
#' @param outputFolder folder to write to
#' @param clearDBDir if database directory should be cleared
#'
#' @return No return value, called for side effects
#'
#' @examples
#' \dontrun{
#' resultList <- list("mtcars" = mtcars)
#' result <- writeIngredientResultToDisk(
#'   resultList = resultList,
#'   databaseId = "mtcars",
#'   outputFolder = here::here()
#' )
#' }
writeIngredientResultToDisk <- function(resultList, databaseId, outputFolder, clearDBDir = FALSE) {
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }
  dbDir <- file.path(outputFolder, databaseId)
  if (!dir.exists(dbDir)) {
    dir.create(dbDir)
  } else {
    if (clearDBDir) {
      unlink(paste0(dbDir, "/*"))
    }
  }

  # write results to disk
  lapply(names(resultList), FUN = function(checkResultName) {
    checkResult <- resultList[[checkResultName]]
    writeFile(checkResult, checkResultName, databaseId, dbDir)
  })
}

#' Write (ingredient) diagnostics results on disk in given output folder.
#'
#' @param metadata metadata results
#' @param databaseId database identifier
#' @param outputFolder folder to write to
#' @param filename output filename for the zip file
#'
#' @return No return value, called for side effects
#'
#' @examples
#' \dontrun{
#' resultList <- list("mtcars" = mtcars)
#' result <- writeZipToDisk(
#'   metadata = metadata,
#'   databaseId = "mtcars",
#'   outputFolder = here::here()
#' )
#' }
writeZipToDisk <- function(metadata, databaseId, outputFolder, filename = NULL) {
  dbDir <- file.path(outputFolder, databaseId)
  writeFile(metadata, "metadata", databaseId, dbDir)

  filename <- ifelse(is.null(filename), databaseId, filename)
  utils::zip(
    zipfile = file.path(outputFolder, paste0(filename, ".zip")),
    files = list.files(dbDir, full.names = TRUE),
    extras = "-j"
  )
  unlink(dbDir, recursive = TRUE)
}

#' Write diagnostics results to a zip file on disk in given output folder.
#'
#' @param resultList named list with results
#' @param databaseId database identifier
#' @param outputFolder folder to write to
#' @param filename output filename, if NULL it will be equal to databaseId
#'
#' @export
#'
#' @return No return value, called for side effects
#'
#' @examples
#' \dontrun{
#' resultList <- list("mtcars" = mtcars)
#' result <- writeResultToDisk(
#'   resultList = resultList,
#'   databaseId = "mtcars",
#'   outputFolder = here::here()
#' )
#' }
writeResultToDisk <- function(resultList, databaseId, outputFolder, filename = NULL) {
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }
  tempDir <- databaseId
  tempDirCreated <- FALSE
  if (!dir.exists(tempDir)) {
    dir.create(tempDir)
    tempDirCreated <- TRUE
  }

  # write results to disk
  lapply(names(resultList), FUN = function(checkResultName) {
    checkResult <- resultList[[checkResultName]]
    checkResult <- dplyr::bind_cols(
      database_id = databaseId,
      checkResult
    )
    utils::write.csv(checkResult,
      file = file.path(
        tempDir,
        paste0(checkResultName, ".csv")
      ),
      row.names = FALSE
    )
  })
  filename <- ifelse(is.null(filename), databaseId, filename)
  zip::zip(
    zipfile = file.path(outputFolder, paste0(filename, ".zip")),
    files = list.files(tempDir, full.names = TRUE)
  )
  if (tempDirCreated) {
    unlink(tempDir, recursive = TRUE)
  }
}
