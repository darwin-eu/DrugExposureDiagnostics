# Copyright 2022 DARWIN EU®
#
# This file is part of IncidencePrevalence
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
#' @param drug_exposure_size the sample size of the drug exposure table
#' @param patient_size the number of unique patients in the drug exposure table
#' @param amount_val vector of possible numeric amount value for the drug in the drug strength table
#' @param den_val vector of possible numeric denominator value for the drug in drug strength table
#' @param unit vector of possible unit type drug strength table please select from "", "actuat", "mg", "mL", "mL", "h".
#' @param num_val vector of possible numeric numerator denominator value drug strength table
#' @param seed seed to make results reproducible
#'
#'
#' @return CDMConnector CDM reference object to duckdb database with mock data include  concept_ancestor, concept, drug_strength, drug_exposure tables
mockDrugExposure <- function(drug_exposure = NULL,
                             concept_ancestor = NULL,
                             concept_relationship = NULL,
                             concept = NULL,
                             drug_strength = NULL,
                             drug_exposure_size = 100,
                             patient_size = 50,
                             amount_val = c(1, 2, 3),
                             den_val = c(1, 10, 100),
                             unit = c("", "actuat", "mg", "mL", "mL", "h"),
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
  checkmate::assert_character(unit)
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
          19133768
        ),
        concept_name = c(
          "Acetaminophen",
          "Acetaminophen 325 MG Oral Tablet",
          "Acetaminophen 750 MG / Hydrocodone Bitartrate",
          "Acetaminophen 325 MG / Oxycodone Hydrochloride",
          "Acetaminophen 21.7 MG/ML / Dextromethorphan",
          "Acetaminophen 325 MG / Hydrocodone Bitartrate",
          "Acetaminophen 160 MG Oral Tablet"
        ) ,
        domain_id = c(rep("Drug", 7)),
        vocabulary_id = c(rep("RxNorm", 7)),
        concept_class_id = c("Ingredient", rep("Clinical Drug", 6)),
        standard_concept = c(rep("S", 7)),
        concept_code = c(161, 313782, 833036, 1049221, 1043400, 857005, 282464),
        valid_start_date = c(rep(as.Date("1970-01-01"), 7)),
        valid_end_date = c(rep(as.Date("2099-12-31"), 7)),
        invalid_reason = c(rep(NA, 7))
      )
  }

  if (is.null(concept_relationship)) {
    # add a rxnorm dose relationship
    concept_relationship <-
      data.frame(concept_id_1 = concept$concept_id[1],
                 concept_id_2 = 9999,
                 relationship_id = "RxNorm has dose form")
    # add to concept
    concept <- concept %>%
      dplyr::bind_rows(
        data.frame(concept_id = 9999,
          concept_name = "injection"
      ))
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
    denominator_unit <-
      sample(unit, length(descendant_concept_id), replace = TRUE)
    numerator_unit <-
      sample(unit, length(descendant_concept_id), replace = TRUE)
    denominator_value <-
      sample(den_val, length(descendant_concept_id), replace = TRUE)
    numerator_value <-
      sample(num_val, length(descendant_concept_id), replace = TRUE)

    invalid_reason <- c(rep(NA, length(descendant_concept_id)))

    numerator_unit_concept_id <-
      sample(1:10, length(descendant_concept_id), replace = TRUE)

    denominator_unit_concept_id <-
      sample(1:10, length(descendant_concept_id), replace = TRUE)

    amount_unit_concept_id <-
      sample(1:10, length(descendant_concept_id), replace = TRUE)

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
        denominator_unit = as.character(denominator_unit),
        numerator_unit = as.character(numerator_unit),
        valid_start_date = as.Date(valid_start_date),
        valid_end_date = as.Date(valid_end_date),
        invalid_reason = as.character(invalid_reason)
      )
  }


  #drug_exposure
  set.seed(seed)
  concept_id <-
    if (is.null(drug_exposure)) {
      drug_exposure_id <-
        as.character(seq(1:drug_exposure_size)) #generate number of unique drug_exposure_id
      person_id <-
        as.character(sample(seq(1:patient_size),
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
        c(rep("38000177", drug_exposure_size))
      stop_reason <- c(rep(NA, drug_exposure_size))
      refills <- c(rep(0, drug_exposure_size))
      quantity <- c(rep(0, drug_exposure_size))
      days_supply <-
        as.integer(difftime(drug_exposure_end_date, drug_exposure_start_date, units = "days"))
      sig <- c(rep(NA, drug_exposure_size))
      route_concept_id <- c(rep(0, drug_exposure_size))
      lot_number <- c(rep(0, drug_exposure_size))
      provider_id <- c(rep(0, drug_exposure_size))
      visit_occurrence_id <-
        as.character(seq(1:drug_exposure_size))
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
  # into in-memory database
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept_ancestor",
                      concept_ancestor,
                      overwrite = TRUE)
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept_relationship",
                      concept_relationship,
                      overwrite = TRUE)
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db,
                      "concept",
                      concept,
                      overwrite = TRUE)
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "drug_strength",
                      drug_strength,
                      overwrite = TRUE)
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "drug_exposure",
                      drug_exposure,
                      overwrite = TRUE)
  })

  cdm <- CDMConnector::cdm_from_con(db,
                                    cdm_tables = c(
                                      "concept_ancestor",
                                      "concept_relationship",
                                      "concept",
                                      "drug_strength",
                                      "drug_exposure"
                                    ))

  return(cdm)
}
