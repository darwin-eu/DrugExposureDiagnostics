# metaDataPanel

Class to view the metadata of a DrugExposureDiagnostics execution.

## Super class

[`ShinyModule`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.md)
-\> `metaDataPanel`

## Methods

### Public methods

- [`metaDataPanel$new()`](#method-metaDataPanel-initialize)

- [`metaDataPanel$uiBody()`](#method-metaDataPanel-uiBody)

- [`metaDataPanel$clone()`](#method-metaDataPanel-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.html#method-UI)
- [`ShinyModule$server()`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu.github.io/DrugExposureDiagnostics/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `metaDataPanel$new()`

Method to handle the back-end.

Initializer method

#### Usage

    metaDataPanel$new(data, id, title, description, downloadFilename)

#### Arguments

- `data`:

  data from the `DrugExposureDiagnostics` package.

- `id`:

  the unique reference id for the module

- `title`:

  panel title

- `description`:

  description of data table

- `downloadFilename`:

  filename of the downloaded file

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

### `metaDataPanel$uiBody()`

Method to include a
[tabPanel](https://rdrr.io/pkg/shiny/man/tabPanel.html) to include the
body.

#### Usage

    metaDataPanel$uiBody()

#### Returns

(`tabItem`)

------------------------------------------------------------------------

### `metaDataPanel$clone()`

The objects of this class are cloneable with this method.

#### Usage

    metaDataPanel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
