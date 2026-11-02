test_that("provider check counts matched, missing and unmatched providers", {
  records <- tibble::tibble(
    drug_concept_id = c(10L, 10L, 20L, 20L, 20L),
    drug = c("a", "a", "b", "b", "b"),
    ingredient_concept_id = 1L, ingredient = "ingredient",
    person_id = c(1L, 2L, 3L, 4L, 5L),
    provider_id = c(1L, 1L, 2L, 99L, NA_integer_)
  )
  provider <- data.frame(provider_id = c(1L, 2L), provider_name = c("First", "Second"))
  cdm <- mockDrugExposure(ingredient_drug_records = records, provider = provider)
  con <- attr(cdm, "dbcon")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  overall <- getDrugProviders(cdm, byConcept = FALSE, sampleSize = 5) %>%
    dplyr::collect() %>% dplyr::arrange(.data$provider_id)
  expect_equal(overall$provider_id, c(1L, 2L, 99L, NA_integer_))
  expect_equal(overall$provider_name, c("First", "Second", NA, NA))
  expect_equal(overall$n_records, c(2L, 1L, 1L, 1L))
  expect_equal(overall$proportion_records, c(0.4, 0.2, 0.2, 0.2))
  byConcept <- getDrugProviders(cdm, byConcept = TRUE, sampleSize = 5) %>% dplyr::collect()
  expect_equal(sum(byConcept$n_records), 5L)
  expect_true(all(c("drug_concept_id", "provider_id", "provider_name") %in% names(byConcept)))
  obscured <- obscureCounts(overall, "drugProvidersOverall", minCellCount = 2)
  expect_false(obscured$result_obscured[1])
  expect_true(all(obscured$result_obscured[-1]))
  expect_true(all(is.na(obscured$proportion_records[-1])))
})

test_that("provider check is available through executeChecks", {
  cdm <- mockDrugExposure(provider = data.frame(provider_id = 7L, provider_name = "Provider"))
  con <- attr(cdm, "dbcon")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  result <- executeChecks(cdm, checks = "provider", minCellCount = 0, sample = NULL)
  expect_true(all(c("drugProvidersOverall", "drugProvidersByConcept") %in% names(result)))
  expect_true("Provider" %in% result$drugProvidersOverall$provider_name)
  expect_equal(sum(result$drugProvidersOverall$proportion_records), 1)
})
