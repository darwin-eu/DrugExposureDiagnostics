ableToRun <- function() {
  list(
    shiny = all(
      require("shiny", quietly = TRUE, mask.ok = TRUE, character.only = TRUE),
      require("ggplot2", quietly = TRUE, mask.ok = TRUE, character.only = TRUE),
      require("plotly", quietly = TRUE, mask.ok = TRUE, character.only = TRUE),
      require("dplyr", quietly = TRUE, mask.ok = TRUE, character.only = TRUE)
    )
  )
}
