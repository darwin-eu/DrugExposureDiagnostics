library(shinytest2)



test_that("{shinytest2} recording: ResultsExplorer", {
  app <- AppDriver$new(name = "ResultsExplorer", height = 958, width = 1376)
  app$set_inputs(`ingredientConcepts-columnPicker` = c("drug_concept_id", "drug", 
      "n_records", "n_patients", "domain_id", "vocabulary_id", "concept_class_id", 
      "dose_form", "result_obscured"))
  app$expect_values()
})
