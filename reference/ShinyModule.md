# Module Decorator Class

Module Decorator Class

Module Decorator Class

## Active bindings

- `instanceId`:

  (`character(1)`) Random ID

- `parentNamespace`:

  (`character(1)`) Namespace parent module

- `moduleName`:

  (`character(1)`) Module name

- `moduleId`:

  (`character(1)`) Module id `moduleName-instanceId`

- `namespace`:

  (`character(1)`) Namespace, composed like:
  `[parentNamespace-]moduleName-instanceId` where `parentNamespace` is
  optional

- `reactiveValues`:

  (`reactivevalues`) Reactive values. use
  [`shiny::isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html) to
  get a non-reactive item from the reactive environment.

## Methods

### Public methods

- [`ShinyModule$new()`](#method-ShinyModule-new)

- [`ShinyModule$validate()`](#method-ShinyModule-validate)

- [`ShinyModule$UI()`](#method-ShinyModule-UI)

- [`ShinyModule$server()`](#method-ShinyModule-server)

- [`ShinyModule$clone()`](#method-ShinyModule-clone)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    ShinyModule$new()

#### Returns

(`self`)

------------------------------------------------------------------------

### Method `validate()`

Validator method

#### Usage

    ShinyModule$validate()

#### Returns

(`self`)

------------------------------------------------------------------------

### Method `UI()`

Method to include a
[tagList](https://rstudio.github.io/htmltools/reference/tagList.html) to
include the body.

#### Usage

    ShinyModule$UI()

#### Returns

(`tagList`)

------------------------------------------------------------------------

### Method `server()`

Method to handle the back-end.

#### Usage

    ShinyModule$server(input, output, session)

#### Arguments

- `input`:

  (`input`) Input from the server function.

- `output`:

  (`output`) Output from the server function.

- `session`:

  (`session`) Session from the server function.

#### Returns

(`NULL`)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ShinyModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
