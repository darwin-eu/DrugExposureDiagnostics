test_that("Shiny app works", {
  skip()

  appdir <- system.file(package = "DrugExposureDiagnostics", "shiny", "ResultsExplorer")
  expect_true(dir.exists(appdir))
  shinytest2::test_app(appdir)
})
