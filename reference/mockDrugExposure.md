# Mock Drug exposure tables for ingredients of interest

Mock Drug exposure tables for ingredients of interest

## Usage

``` r
mockDrugExposure(
  drug_exposure = NULL,
  concept_ancestor = NULL,
  concept_relationship = NULL,
  concept = NULL,
  drug_strength = NULL,
  ingredient_drug_records = NULL,
  drug_exposure_size = 100,
  patient_size = 50,
  person = NULL,
  observation_period = NULL,
  amount_val = c(NA, 100, 200, 300),
  den_val = c(1, 10, 100),
  amount_unit = c(8587, 8576, 9655),
  num_unit = c(8587, 8576, 9655),
  denom_unit = c(8587, 8576, 8505),
  num_val = c(1, 2, 3),
  seed = 1
)
```

## Arguments

- drug_exposure:

  drug exposure table

- concept_ancestor:

  concept_ancestor table

- concept_relationship:

  concept_relationship table

- concept:

  concept table

- drug_strength:

  drug strength table

- ingredient_drug_records:

  modified drug exposure table having drug name

- drug_exposure_size:

  the sample size of the drug exposure table

- patient_size:

  the number of unique patients in the drug exposure table

- person:

  person table

- observation_period:

  observation_period table

- amount_val:

  vector of possible numeric amount value for the drug in the drug
  strength table

- den_val:

  vector of possible numeric denominator value for the drug in drug
  strength table

- amount_unit:

  vector of possible amount unit type drug strength table representing
  milligram, milliliter and microgram

- num_unit:

  vector of possible numerator unit type drug strength table
  representing milligram, milliliter and microgram

- denom_unit:

  vector of possible numerator unit type drug strength table
  representing milligram, milliliter and hour

- num_val:

  vector of possible numeric numerator denominator value drug strength
  table

- seed:

  seed to make results reproducible

## Value

CDMConnector CDM reference object to duckdb database with mock data
include concept_ancestor, concept, drug_strength, drug_exposure tables
