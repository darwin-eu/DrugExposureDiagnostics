getInputDb <- function() {

  drug_exposure <- tibble::tibble(
    drug_exposure_id = c(1, 2, 3, 4, 5, 6, 7),
    person_id = c(1, 2, 3, 4, 5, 6, 7),
    drug_concept_id = c(40162522, 40162522, 1127078, 1127078, 1127078, 1127433, 1127433),
    ingredient_concept_id = rep(1125315, 7),
    ingredient = rep("acetaminophen", 7),
    drug_exposure_start_date = as.Date(c("2016-01-01","2017-01-01",NA,"2019-01-01","2020-01-01","2021-01-01","2022-01-01")),
    drug_exposure_end_date = as.Date(c("2016-01-02","2017-01-03",NA,"2019-01-05","2020-01-06","2021-01-07","2022-01-08")),
    verbatim_end_date = as.Date(c("2016-01-02","2017-01-03",NA,"2019-01-05","2020-01-06","2021-01-07","2022-01-08")),
    days_supply = c(1,3,NA,4,5,6,7),
    quantity = c(10,20,1,NA,2,3,NA),
    drug_type_concept_id = rep(0, 7),
    stop_reason = rep(0, 7),
    refills = rep(0, 7),
    sig = rep("0", 7),
    route_concept_id = rep(0, 7),
    lot_number = rep(0, 7),
    provider_id = rep(0, 7),
    visit_occurrence_id = rep(0, 7),
    drug_source_value = rep(0, 7),
    drug_source_concept_id = rep(0, 7),
    route_source_value = rep(0, 7),
    dose_unit_source_value = rep(0, 7)
  )

  drug_strength <- tibble::tibble(
    drug_concept_id = c(40162522, 1127078, 1127433),
    ingredient_concept_id =  rep(1125315, 3),
    amount_value = c(325,160,325),
    denominator_value = rep(as.numeric(NA), 3),
    numerator_value = rep(as.numeric(NA), 3),
    numerator_unit_concept_id = rep(as.numeric(NA), 3),
    denominator_unit_concept_id = rep(as.numeric(NA), 3),
    amount_unit_concept_id = rep(8576,3),
    valid_start_date = as.Date(rep("1970-01-01",3)),
    valid_end_date = as.Date(rep("2099-12-31",3)),
    invalid_reason = as.character(rep("none",3))
  )

  concept_relationship <-  data.frame(
      concept_id_1 = c(40162522,1127078, 1127433),
      concept_id_2 = rep(19082573,3),
      relationship_id = c("RxNorm has dose form","RxNorm has dose form","RxNorm has dose form"),
      valid_start_date = as.Date(rep("1970-01-01",3)),
      valid_end_date = as.Date(rep("2099-12-31",3))
  )

  mockDrugExposure(drug_exposure = drug_exposure,
                   drug_strength = drug_strength ,
                   concept_relationship = concept_relationship,
                   patient_size = 5)

}

test_that("checkDrugDose overall", {
  testData <- getInputDb()
  ingredient = 1125315
  minCellCount = 0

  result <- checkDrugDose(testData, ingredientConceptId = ingredient, minCellCount = minCellCount)

  expect_equal(nrow(result), 48)
  expect_equal(ncol(result), 16)
  expect_equal(colnames(result), c(
    "result_id","cdm_name","group_name","group_level","strata_name","strata_level",
    "variable_name","variable_level","estimate_name","estimate_type","estimate_value","additional_name",
    "additional_level","pattern_name","ingredient","ingredient_concept_id"
  ))
  expect_true(result$group_level[1] == "acetaminophen")
  expect_equal(result %>%
                 dplyr::filter(.data$estimate_name == "count" & .data$strata_level == "overall") %>%
                 dplyr::pull(estimate_value), "7")
  expect_equal(round(as.numeric(result %>%
                   dplyr::filter(.data$estimate_name == "median" & .data$strata_level == "overall") %>%
                   dplyr::pull(estimate_value),0)), 882)
  expect_true(!is.na(result %>%
                  dplyr::filter(.data$estimate_name == "min" & .data$strata_level == "overall") %>%
                  dplyr::pull(estimate_value)))
  expect_equal(result %>%
                 dplyr::filter(.data$strata_level %in% c("milligram")) %>%
                 dplyr::pull(strata_level),
               result %>%
                 dplyr::filter(.data$strata_level %in% c("milligram")) %>%
                 dplyr::pull(pattern_name))

  DBI::dbDisconnect(attr(testData, "dbcon"), shutdown = TRUE)

})
