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

#' Mock Drug exposure tables for ingredients of interest
#'
#' @param drug_exposure drug exposure table
#' @param concept_ancestor concept_ancestor table
#' @param concept_relationship concept_relationship table
#' @param concept concept table
#' @param drug_strength drug strength table
#' @param ingredient_drug_records modified drug exposure table having drug name
#' @param drug_exposure_size the sample size of the drug exposure table
#' @param patient_size the number of unique patients in the drug exposure table
#' @param person person table
#' @param observation_period observation_period table
#' @param amount_val vector of possible numeric amount value for the drug in the drug strength table
#' @param den_val vector of possible numeric denominator value for the drug in drug strength table
#' @param amount_unit vector of possible amount unit type drug strength table representing milligram, milliliter and microgram
#' @param num_unit vector of possible numerator unit type drug strength table representing milligram, milliliter and microgram
#' @param denom_unit vector of possible numerator unit type drug strength table representing milligram, milliliter and hour
#' @param num_val vector of possible numeric numerator denominator value drug strength table
#' @param seed seed to make results reproducible
#'
#' @return CDMConnector CDM reference object to duckdb database with mock data include  concept_ancestor, concept, drug_strength, drug_exposure tables
#' @export
mockDrugExposure <- function(drug_exposure = NULL,
                             concept_ancestor = NULL,
                             concept_relationship = NULL,
                             concept = NULL,
                             drug_strength = NULL,
                             ingredient_drug_records = NULL,
                             drug_exposure_size = 100,
                             patient_size = 50,
                             person = NULL,
                             observation_period = NULL,
                             amount_val = c(NA,100, 200, 300),
                             den_val = c(1, 10, 100),
                             amount_unit = c(8587,8576,9655),
                             num_unit = c(8587,8576,9655),
                             denom_unit = c(8587,8576,8505),
                             num_val = c(1, 2, 3),
                             seed = 1) {

  #checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_int(drug_exposure_size, lower = 1)
  checkmate::assert_int(patient_size, lower = 1)
  checkmate::assertTRUE(drug_exposure_size >= patient_size)
  checkmate::assert_numeric(amount_val)
  checkmate::assert_numeric(den_val)
  checkmate::assert_numeric(num_val)
  checkmate::assert_numeric(amount_unit)
  checkmate::assert_numeric(denom_unit)
  checkmate::assert_numeric(num_unit)
  checkmate::reportAssertions(collection = errorMessage)
  # mock concept ancestor table


  if (is.null(concept_ancestor)) {
    ancestor_concept_id <- rep(1125315, each = 6)
    descendant_concept_id <-
      c(40162522, 1127078, 1127433, 40229134, 40231925, 19133768)
    min_levels_of_separation <- rep(0, each = 6)
    max_levels_of_separation <- rep(1, each = 6)


    concept_ancestor <-
      data.frame(
        ancestor_concept_id = ancestor_concept_id ,
        descendant_concept_id = descendant_concept_id,
        min_levels_of_separation = min_levels_of_separation,
        max_levels_of_separation = max_levels_of_separation
      )
  }
  # mock concept table
  if (is.null(concept)) {
    concept <-
      data.frame(
        concept_id = c(
          1125315,
          40162522,
          1127078,
          1127433,
          40229134,
          40231925,
          19133768,
          4132161,
          38000177,
          19082573
        ),
        concept_name = c(
          "acetaminophen",
          "acetaminophen 325 MG Oral Tablet",
          "acetaminophen 750 MG / Hydrocodone Bitartrate",
          "acetaminophen 325 MG / Oxycodone Hydrochloride",
          "acetaminophen 21.7 MG/ML / Dextromethorphan",
          "acetaminophen 325 MG / Hydrocodone Bitartrate",
          "acetaminophen 160 MG Oral Tablet",
          "Oral",
          "Prescription written",
          "Oral Tablet"
        ) ,
        domain_id = c(rep("Drug", 7),"Route","Type Concept","Drug"),
        vocabulary_id = c(rep("RxNorm", 7), "SNOMED","Drug Type","RxNorm"),
        concept_class_id = c("Ingredient", rep("Clinical Drug", 6),"Qualifier Value","Drug Type","Dose Form"),
        standard_concept = c(rep("S", 8), rep("Non-standard",2)),
        concept_code = c("161", "313782", "833036", "1049221", "1043400", "857005", "282464","26643006","OMOP4822241","Oral Tablet"),
        valid_start_date = c(rep(as.Date("1970-01-01"), 10)),
        valid_end_date = c(rep(as.Date("2099-12-31"), 10)),
        invalid_reason = c(rep(NA, 10))
      )
  }

  if (is.null(concept_relationship)) {
    # add a rxnorm dose relationship
    concept_relationship <-
      data.frame(concept_id_1 = concept$concept_id[2:7],
                 concept_id_2 = rep(19082573, 6),
                 relationship_id = rep("RxNorm has dose form", 6),
                 valid_start_date = rep(as.Date("1970-01-01"), 6),
                 valid_end_date = rep(as.Date("2099-12-31"), 6))
  }

  if (is.null(drug_strength)) {
    ancestor_concept_id <- rep(1125315, each = 6)
    descendant_concept_id <-
      c(40162522, 1127078, 1127433, 40229134, 40231925, 19133768)

    ingredient_concept_id <-
      sample(ancestor_concept_id,
             length(descendant_concept_id),
             replace = TRUE)

    amount_value <-
      sample(amount_val, length(descendant_concept_id), replace = TRUE)

    denominator_value <- dplyr::if_else(is.na(amount_value), sample(den_val,1),NA)

    numerator_value <- dplyr::if_else(is.na(amount_value), sample(num_val,1),NA)

    denominator_unit_concept_id <- dplyr::if_else(is.na(amount_value), sample(denom_unit,1),NA)

    numerator_unit_concept_id <- dplyr::if_else(is.na(amount_value), sample(num_unit,1),NA)

    amount_unit_concept_id <- dplyr::if_else(
      is.na(denominator_value) & is.na(numerator_value),sample(amount_unit,1), NA)

    invalid_reason <- c(rep(NA, length(descendant_concept_id)))

    valid_start_date <-
      rep(as.Date("1970-01-01"), length(descendant_concept_id))

    valid_end_date <-
      rep(as.Date("2099-12-31"), length(descendant_concept_id))

    drug_strength <-
      data.frame(
        drug_concept_id = as.numeric(descendant_concept_id),
        ingredient_concept_id = as.numeric(ingredient_concept_id),
        amount_value = as.numeric(amount_value),
        amount_unit_concept_id = as.numeric(amount_unit_concept_id),
        numerator_value = as.numeric(numerator_value),
        numerator_unit_concept_id = as.numeric(numerator_unit_concept_id),
        denominator_value = as.numeric(denominator_value),
        denominator_unit_concept_id = as.numeric(denominator_unit_concept_id),
        valid_start_date = as.Date(valid_start_date),
        valid_end_date = as.Date(valid_end_date),
        invalid_reason = as.character(invalid_reason)
      )
  }


  # ingredient_drug_records PLACEHOLDER
  set.seed(seed)
  if (is.null(ingredient_drug_records)) {
    drug_exposure_id <-
      as.integer(seq(1:drug_exposure_size)) #generate number of unique drug_exposure_id

  # putting into drug_exposure table
  ingredient_drug_records <-
    data.frame(
      drug_exposure_id = drug_exposure_id
    )

  }


  #drug_exposure
  set.seed(seed)
  if (is.null(drug_exposure)) {
      drug_exposure_id <-
        as.integer(seq(1:drug_exposure_size)) #generate number of unique drug_exposure_id
      person_id <-
        as.integer(sample(seq(1:patient_size),
                            drug_exposure_size,
                            replace = TRUE)) #generate number of unique patient id
      drug_concept_id <-
        sample(
          c(
            1125315,
            40162522,
            1127078,
            1127433,
            40229134,
            40231925,
            19133768
          ),
          drug_exposure_size,
          replace = TRUE
        ) #assign drug concept id to to each drug exposure

      # generate drug exposure start date
      drug_exposure_start_date <-  sample(seq(as.Date("2000-01-01"),
                                              as.Date("2020-01-01"),
                                              by = "day"),
                                          drug_exposure_size,
                                          replace = TRUE)
      # generate drug exposure end date to happens after drug exposure start date
      drug_exposure_end_date  <-
        drug_exposure_start_date + lubridate::days(sample(c(0, 7, 14, 21, 28, 30, 60, 90)
                                                          ,
                                                          drug_exposure_size,
                                                          replace = TRUE))
      # define other columns in the dataset
      verbatim_end_date <- drug_exposure_end_date
      drug_type_concept_id <-
        c(rep(38000177, drug_exposure_size))
      stop_reason <- c(rep(NA, drug_exposure_size))
      refills <- c(rep(0, drug_exposure_size))
      quantity <- sample(1:10, drug_exposure_size, replace = TRUE)
      days_supply <-
        as.integer(difftime(drug_exposure_end_date, drug_exposure_start_date, units = "days"))
      sig <- sample(c("TAKE 1 or 2 4 TIMES/DAY",
                      "1-2 TABLETS UP TO FOUR TIMES DAILY",
                      "1 TO 2 TABLETS UP TO FOUR TIMES DAILY AS REQUIRED"), drug_exposure_size, replace = TRUE)
      route_concept_id <- c(rep(4132161, drug_exposure_size))
      lot_number <- c(rep(0, drug_exposure_size))
      provider_id <- c(rep(0, drug_exposure_size))
      visit_occurrence_id <-
        as.integer(seq(1:drug_exposure_size))
      drug_source_value <-
        sample(c("857005", "282464", "313782", "1043400"),
               drug_exposure_size,
               replace = TRUE)
      drug_source_concept_id <- drug_concept_id
      route_source_value <- c(rep(NA, drug_exposure_size))
      dose_unit_source_value <- c(rep(NA, drug_exposure_size))

      # putting into drug_exposure table
      drug_exposure <-
        data.frame(
          drug_exposure_id = drug_exposure_id,
          person_id = person_id,
          drug_concept_id = drug_concept_id,
          drug_exposure_start_date = drug_exposure_start_date,
          drug_exposure_end_date = drug_exposure_end_date,
          verbatim_end_date = verbatim_end_date,
          drug_type_concept_id = drug_type_concept_id,
          stop_reason = stop_reason,
          refills = refills,
          quantity = quantity,
          days_supply = days_supply,
          sig = sig,
          route_concept_id = route_concept_id,
          lot_number = lot_number,
          provider_id = provider_id,
          visit_occurrence_id = visit_occurrence_id,
          drug_source_value = drug_source_value,
          drug_source_concept_id = drug_source_concept_id,
          route_source_value = route_source_value,
          dose_unit_source_value = dose_unit_source_value
        )
    }
  #cdm_source
  cdm_source <-
    data.frame(
      cdm_source_name = "DrugExposureMock",
      cdm_source_abbreviation = "DEM",
      cdm_holder = "ErasmusMC",
      source_description = "DrugExposureMock",
      source_documentation_reference = "",
      cdm_etl_reference = "https://github.com/darwin-eu/DrugExposureDiagnostics",
      source_release_date = "2022-08-12",
      cdm_release_date = "2022-08-12",
      cdm_version = "5.4",
      cdm_version_concept_id = "756265",
      vocabulary_version = "v5.0 22-JUN-22"
  )

  # person
  set.seed(seed)
  if (is.null(person)) {
    person_id <-
        as.integer(1:patient_size) #generate number of unique patient id
    gender_concept_id <- sample(
      c(8532,8507),
        patient_size,
        replace = TRUE
      ) #assign gender to each patient
    year_of_birth <-  sample(seq(as.Date("1960-01-01"),
                                   as.Date("2020-01-01"),
                                   by = "day"),
                               patient_size,
                               replace = TRUE)
    race_concept_id <- sample(
      c(8527,8515),
      patient_size,
      replace = TRUE
    ) #assign race to each patient
    ethnicity_concept_id <- sample(
      c(8532,8507),
      patient_size,
      replace = TRUE
    ) #assign ethnicity to patient

    # putting into person table
    person <-
      data.frame(
        person_id = person_id,
        gender_concept_id = gender_concept_id,
        year_of_birth = year_of_birth,
        race_concept_id = race_concept_id,
        ethnicity_concept_id = ethnicity_concept_id
      )
  }

  ## obersation_period
  set.seed(seed)
  if (is.null(observation_period)) {
      observation_period_id <-
        as.integer(seq(1:patient_size)) #generate number of unique drug_exposure_id
      person_id <- as.integer(sample(seq(1:patient_size),
                                     patient_size,
                                     replace = FALSE)) #generate number of unique patient id
      observation_period_start_date = sample(seq(as.Date("2000-01-01"),
                                                 as.Date("2010-01-01"),
                                                 by = "day"),
                                             patient_size,
                                             replace = TRUE)
      observation_period_end_date = sample(seq(as.Date("2010-01-01"),
                                               as.Date("2020-01-01"),
                                               by = "day"),
                                           patient_size,
                                           replace = TRUE)
      period_type_concept_id <- as.integer(seq(1:patient_size)) #any

      # putting into person table
      observation_period <-
        data.frame(
          observation_period_id = observation_period_id,
          person_id = person_id,
          observation_period_start_date = observation_period_start_date,
          observation_period_end_date = observation_period_end_date,
          period_type_concept_id = period_type_concept_id
        )

  }

  # into in-memory database
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  DBI::dbWriteTable(db, "person",
                    person,
                    overwrite = TRUE)

  DBI::dbWriteTable(db, "observation_period",
                    observation_period,
                    overwrite = TRUE)


  DBI::dbWriteTable(db, "concept_ancestor",
                    concept_ancestor,
                    overwrite = TRUE)

  DBI::dbWriteTable(db, "concept_relationship",
                    concept_relationship,
                    overwrite = TRUE)

  DBI::dbWriteTable(db,
                    "concept",
                    concept,
                    overwrite = TRUE)

  DBI::dbWriteTable(db, "drug_strength",
                    drug_strength,
                    overwrite = TRUE)

  DBI::dbWriteTable(db, "drug_exposure",
                    drug_exposure,
                    overwrite = TRUE)

  DBI::dbWriteTable(db, "cdm_source",
                    cdm_source,
                    overwrite = TRUE)

  DBI::dbWriteTable(db, "ingredient_drug_records",
                    ingredient_drug_records,
                    overwrite = TRUE)

  write_schema = "main"
  cdm <- CDMConnector::cdm_from_con(db, cdm_schema = "main", write_schema = write_schema) %>%
      CDMConnector::cdm_select_tbl(c(person, observation_period,concept_ancestor, concept_relationship,
                                   concept, drug_strength, drug_exposure, cdm_source))

  cdm$ingredient_drug_records <- dplyr::tbl(db, CDMConnector::inSchema(write_schema, "ingredient_drug_records"))

  return(cdm)
}
