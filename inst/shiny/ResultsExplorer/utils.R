# utility functions

# load file into environment
loadFile <- function(file, folder, overwrite, i) {
  print(file)
  tableName <- gsub(".csv$", "", file)
  camelCaseName <- snakeCaseToCamelCase(tableName)
  data <- read.csv(file.path(folder, file), encoding = "UTF-8")

  if (grepl("drugVerbatimEndDate", file)) {
    data <- data %>%
      dplyr::mutate(minimum_verbatim_end_date = ifelse(class(minimum_verbatim_end_date) %in% c("numeric", "integer"),
        format(as.Date(as.POSIXct(minimum_verbatim_end_date, origin = "1970-01-01"))),
        minimum_verbatim_end_date
      )) %>%
      dplyr::mutate(maximum_verbatim_end_date = ifelse(class(maximum_verbatim_end_date) %in% c("numeric", "integer"),
        format(as.Date(as.POSIXct(maximum_verbatim_end_date, origin = "1970-01-01"))),
        maximum_verbatim_end_date
      ))
  } else if (grepl("conceptSummary", file)) {
    data <- data %>%
      dplyr::mutate(concept_code = as.character(concept_code)) %>%
      dplyr::mutate(valid_start_date = ifelse(class(valid_start_date) == "integer",
        format(as.Date(valid_start_date, origin = "1970-01-01")),
        valid_start_date
      )) %>%
      dplyr::mutate(valid_end_date = ifelse(class(valid_end_date) == "integer",
        format(as.Date(valid_end_date, origin = "1970-01-01")),
        valid_end_date
      ))
  } else if (grepl("drugDose", file)) {
    data <- data %>%
      dplyr::mutate(result_id = i,
                    estimate_value = as.character(estimate_value))
  } else if (grepl("drugSourceConceptsOverall", file)) {
    data <- data %>%
      dplyr::mutate(drug_source_value = as.character(.data$drug_source_value))
  }

  if (!overwrite && exists(camelCaseName, envir = .GlobalEnv)) {
    existingData <- get(camelCaseName, envir = .GlobalEnv)
    if (nrow(existingData) > 0) {
      if (nrow(data) > 0 &&
        all(colnames(existingData) %in% colnames(data)) &&
        all(colnames(data) %in% colnames(existingData))) {
        data <- data[, colnames(existingData)]
      }

      if (!isTRUE(all.equal(colnames(data), colnames(existingData), check.attributes = FALSE))) {
        stop(
          "Table columns do no match previously seen columns. Columns in ",
          file,
          ":\n",
          paste(colnames(data), collapse = ", "),
          "\nPrevious columns:\n",
          paste(colnames(existingData), collapse = ", ")
        )
      }
    }
    data <- rbind(existingData, data)
  }
  assign(camelCaseName, data, envir = .GlobalEnv)

  invisible(NULL)
}

# format a DED check output
formatResult <- function(result) {
  if (nrow(result) > 0) {
    result <- result %>%
      dplyr::rename(any_of(c(ingredient_id = "ingredient_concept_id"))) %>%
      dplyr::mutate_at(vars(starts_with("proportion_")), ~ 100 * .) %>%
      dplyr::rename_with(~ gsub("proportion_", "perc_", .x)) %>%
      dplyr::mutate_at(
        vars(which(sapply(., is.numeric) & !names(.) %in% c("ingredient_id", "drug_concept_id", "n"))),
        ~ signif(., 4)
      )
  }
  return(result)
}

# copied from SQLRender, so we don't need to include this dependency (and rJava)
snakeCaseToCamelCase <- function(string) {
  string <- tolower(string)
  for (letter in letters) {
    string <- gsub(paste("_", letter, sep = ""), toupper(letter), string)
  }
  string <- gsub("_([0-9])", "\\1", string)
  return(string)
}
