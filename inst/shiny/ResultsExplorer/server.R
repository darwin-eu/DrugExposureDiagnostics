#### SERVER ------
server <- function(input, output, session) {
  commonInputs <- reactiveValues()
  commonInputs$databases <- commonInputsInitValue
  commonInputs$ingredients <- commonInputsInitValue

  dataPlotPanelServer(
    id = "ingredientConcepts",
    data = ingredientConcepts,
    downloadFilename = "IngredientConcepts.csv",
    description = "Ingredient concepts",
    commonInputs = commonInputs,
    selectedColumns = ingredientConceptColumnsSelected
  )
  dataPlotPanelServer(
    id = "drugRoutes",
    data = drugRoutes,
    dataByConcept = drugRoutesByConcept,
    downloadFilename = "DrugRoutes.csv",
    description = "Drug routes",
    commonInputs = commonInputs
  )
  dataPlotPanelServer(
    id = "drugTypes",
    data = drugTypes,
    dataByConcept = drugTypesByConcept,
    downloadFilename = "DrugTypes.csv",
    description = "Drug types",
    commonInputs = commonInputs
  )
  dataPlotPanelServer(
    id = "drugSourceConcepts",
    data = drugSourceConcepts,
    downloadFilename = "DrugSourceConcepts.csv",
    description = "Drug source concepts",
    commonInputs = commonInputs
  )
  dataPlotPanelServer(
    id = "drugExposureDuration",
    data = drugExposureDuration,
    dataByConcept = drugExposureDurationByConcept,
    downloadFilename = "DrugExposureDuration.csv",
    description = "Drug exposure duration",
    commonInputs = commonInputs
  )
  dataPlotPanelServer(
    id = "drugVariablesMissing",
    data = drugVariablesMissing,
    dataByConcept = drugVariablesMissingByConcept,
    downloadFilename = "DrugVariablesMissing.csv",
    description = "Drug variables missing",
    commonInputs = commonInputs
  )
  dataPlotPanelServer(
    id = "drugDaysSupply",
    data = drugDaysSupply,
    dataByConcept = drugDaysSupplyByConcept,
    downloadFilename = "DrugDaysSupply.csv",
    description = "Drug days supply",
    commonInputs = commonInputs
  )
  dataPlotPanelServer(
    id = "drugQuantity",
    data = drugQuantity,
    dataByConcept = drugQuantityByConcept,
    downloadFilename = "DrugQuantity.csv",
    description = "Drug quantity",
    commonInputs = commonInputs
  )
  dataPlotPanelServer(
    id = "drugSig",
    data = drugSig,
    dataByConcept = drugSigByConcept,
    downloadFilename = "DrugSig.csv",
    description = "Drug sig",
    commonInputs = commonInputs
  )
  dataPlotPanelServer(
    id = "drugVerbatimEndDate",
    data = drugVerbatimEndDate,
    dataByConcept = drugVerbatimEndDateByConcept,
    downloadFilename = "DrugVerbatimEndDate.csv",
    description = "Drug verbatim end date",
    commonInputs = commonInputs
  )
  dataPlotPanelServer(
    id = "drugDailyDose",
    data = drugDailyDose,
    dataByConcept = drugDailyDoseByConcept,
    downloadFilename = "drugDailyDose.csv",
    description = "Drug daily dose",
    commonInputs = commonInputs
  )
  metaDataServer(
    id = "metaData",
    data = metaData,
    downloadFilename = "metaData.csv",
    description = "Metadata",
    commonInputs = commonInputs
  )
}
