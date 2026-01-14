# dataPlotPanel

Class to view the data and plot view of a DrugExposureDiagnostics check.

## Value

`self`

## Super class

[`DrugExposureDiagnostics::ShinyModule`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.md)
-\> `dataPlotPanel`

## Methods

### Public methods

- [`dataPlotPanel$new()`](#method-dataPlotPanel-new)

- [`dataPlotPanel$uiBody()`](#method-dataPlotPanel-uiBody)

- [`dataPlotPanel$clone()`](#method-dataPlotPanel-clone)

Inherited methods

- [`DrugExposureDiagnostics::ShinyModule$UI()`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.html#method-UI)
- [`DrugExposureDiagnostics::ShinyModule$server()`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.html#method-server)
- [`DrugExposureDiagnostics::ShinyModule$validate()`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Method to handle the back-end.

Initializer method

#### Usage

    dataPlotPanel$new(
      data,
      dataByConcept = NULL,
      id,
      title,
      description,
      plotPercentage,
      byConcept,
      downloadFilename,
      selectedColumns = colnames(data)
    )

#### Arguments

- `data`:

  data from the `DrugExposureDiagnostics` package.

- `dataByConcept`:

  data by drug concept

- `id`:

  the unique reference id for the module

- `title`:

  panel title

- `description`:

  description of data table

- `plotPercentage`:

  if plot by percentage should be enabled

- `byConcept`:

  add byConcept switch

- `downloadFilename`:

  filename of the downloaded file

- `selectedColumns`:

  default selected columns

- `input`:

  (`input`)  
  Input from the server function.

- `output`:

  (`output`)  
  Output from the server function.

- `session`:

  (`session`)  
  Session from the server function.

#### Returns

(`NULL`)

------------------------------------------------------------------------

### Method `uiBody()`

Method to include a
[tabPanel](https://rdrr.io/pkg/shiny/man/tabPanel.html) to include the
body.

#### Usage

    dataPlotPanel$uiBody()

#### Returns

(`tabItem`)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    dataPlotPanel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
