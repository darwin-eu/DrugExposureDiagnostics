test_that("runBenchmarkExecuteSingleIngredient output contents, and format", {
  skip(message = "does not work with latest omopgenerics")

  cdm <- DrugExposureDiagnostics::mockDrugExposure()

  res <- runBenchmarkExecuteSingleIngredient(cdm)

  expect_true(tibble::is_tibble(res))
  expect_true("summarised_result" %in% class(res))
  expect_equal(
    colnames(res),
    colnames(omopgenerics::emptySummarisedResult())
  )
  expect_true(setequal(c("Time taken (seconds)", "Memory used (MB)"), unique(res$estimate_name)))
  expect_true(setequal(c("numeric"), unique(res$estimate_type)))
  expect_true(setequal(c("DrugExposureMock"), unique(res$cdm_name)))
  expect_true(setequal(c(1), unique(res$result_id)))
  expect_equal("character", class(res$estimate_value))
  expect_true(setequal(c("ingredient"), unique(res$variable_name)))
  expect_true(setequal(c("1125315"), unique(res$variable_level)))
  expect_true(all(sapply(res$estimate_value, function(x) !is.na(as.numeric(x)))))

  expectedSettings <- dplyr::tibble(
    result_id = 1,
    result_type = "ExecuteSingleIngredient benchmark",
    package_name = "DrugExposureDiagnostics",
    package_version = as.character(utils::packageVersion("DrugExposureDiagnostics"))
  )

  expect_equal(omopgenerics::settings(res), expectedSettings)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("runBenchmarkExecuteSingleIngredient given ingredients in wrong format", {
  skip(message = "does not work with latest omopgenerics")

  cdm <- DrugExposureDiagnostics::mockDrugExposure()

  expect_error(runBenchmarkExecuteSingleIngredient(cdm, ingredients = "1125315"))
  expect_error(runBenchmarkExecuteSingleIngredient(cdm, ingredients = c()))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("runBenchmarkExecuteSingleIngredient duplicate ingredients", {
  skip(message = "does not work with latest omopgenerics")

  cdm <- DrugExposureDiagnostics::mockDrugExposure()

  res <- runBenchmarkExecuteSingleIngredient(cdm, ingredients = rep(1125315, 2))
  expect_equal(c(1, 1, 2, 2), res$result_id)

  CDMConnector::cdm_disconnect(cdm)
})
