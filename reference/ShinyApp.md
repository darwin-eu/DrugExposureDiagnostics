# DrugExposureDiagnostics ShinyApp

DrugExposureDiagnostics shiny app that shows tables and plots

## Details

The module consists of the following:

- "dataPlotPanel":

  Table and a plot (bar or box) for each check.

- "metaDataPanel":

  Table containing the metadata.

## Super class

[`ShinyModule`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.md)
-\> `ShinyApp`

## Methods

### Public methods

- [`ShinyApp$new()`](#method-ShinyApp-initialize)

- [`ShinyApp$clone()`](#method-ShinyApp-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.html#method-UI)
- [`ShinyModule$server()`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `ShinyApp$new()`

Initializer method

#### Usage

    ShinyApp$new(resultList, database_id = NULL)

#### Arguments

- `resultList`:

  (`list`) List containing the output of the checks

- `database_id`:

  (`character`) Database identifier (optional)

#### Returns

(`invisible(self)`)

------------------------------------------------------------------------

### `ShinyApp$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ShinyApp$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
