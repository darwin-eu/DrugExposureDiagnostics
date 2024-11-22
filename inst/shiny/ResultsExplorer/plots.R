# Plotting functionality

# Copied from CohortCharacteristics. The new version of this function doesn't work with our data
# and we rather not import another package for a single function
plotCharacteristics <- function(data, x = "variable_name", plotStyle = "barplot", facet = NULL,
                                colour = NULL, colourName = NULL, .options = list()) {
  xAxis <- x
  yAxis <- "estimate_value"
  vertical_x <- FALSE
  nVariableNames <- length(dplyr::pull(dplyr::distinct(dplyr::select(
    data,
    "variable_name"
  ))))
  if (nVariableNames != 1) {
    emptyPlot(
      "Only one variable name can be plotted at a time.",
      "Please filter variable_name column in results before passing to plotCharacteristics()"
    )
  }
  data <- dplyr::mutate(data, estimate_type = dplyr::if_else(.data$estimate_type ==
    "integer", "numeric", .data$estimate_type))
  estimateType <- dplyr::pull(dplyr::distinct(dplyr::select(
    data,
    "estimate_type"
  )))
  nEstimateTypes <- length(estimateType)
  if (nEstimateTypes != 1) {
    emptyPlot(
      "Only one estimate type can be plotted at a time.",
      "Please filter estimate_type column in results before passing to plotCharacteristics()"
    )
  }
  if (!estimateType %in% c("numeric", "percentage")) {
    emptyPlot(paste0(estimateType, " not currently supported by plotCharacteristics()"))
  }
  gg <- plotfunction(data, xAxis, yAxis,
    plotStyle = plotStyle,
    facetVarX = NULL, facetVarY = NULL, colorVars = colour,
    vertical_x, facet = facet, .options = .options
  )
  gg <- gg + ggplot2::theme_bw()
  if (estimateType == "numeric") {
    var <- unique(data$variable_name)
    if (xAxis == "estimate_value") {
      gg <- gg + ggplot2::ylab(var) + ggplot2::xlab("")
    }
    if (yAxis == "estimate_value") {
      gg <- gg + ggplot2::ylab(var) + ggplot2::xlab("")
    }
  }
  if (estimateType == "percentage") {
    if (xAxis == "estimate_value") {
      gg <- gg + ggplot2::xlab("Percentage") + ggplot2::ylab("")
    }
    if (yAxis == "estimate_value") {
      gg <- gg + ggplot2::ylab("Percentage") + ggplot2::xlab("")
    }
  }
  gg <- gg + ggplot2::theme_bw() + ggplot2::theme(legend.position = "top")
  if (!is.null(colourName)) {
    gg <- gg + ggplot2::labs(color = colourName, fill = colourName)
  } else {
    gg <- gg + ggplot2::labs(color = "", fill = "")
  }
  gg
}

plotfunction <- function(data,
                         xAxis = "variable_name",
                         yAxis = "estimate_value",
                         plotStyle = "scatterplot",
                         facetVarX = "variable_name",
                         facetVarY = c("group_level", "strata_level"),
                         colorVars = "variable_level",
                         vertical_x = FALSE,
                         facet = NULL,
                         .options = list()) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(inherits(data, "summarised_result"))
  all_vars <- c(xAxis, yAxis, facetVarX, facetVarY, colorVars)
  checkmate::assertTRUE(all(all_vars[!is.null(all_vars)] %in% colnames(data)))
  if (plotStyle == "density" && xAxis != "estimate_value") {
    stop(sprintf("If plotStyle is set to 'density', xAxis must be 'estimate_value'."))
  }

  checkmate::assertVector(facetVarX, add = errorMessage, null.ok = TRUE)
  checkmate::assertVector(facetVarY, add = errorMessage, null.ok = TRUE)

  if (nrow(data) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Empty Data Provided", subtitle = "No data available for plotting."))
  }

  if (plotStyle == "boxplot") {
    if (!all(c("q25", "median", "q75", "min", "max") %in% data$estimate_name)) {
      return(
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Data Provided",
            subtitle = "Boxplot needs to have min max q25 q75 in estimate_name"
          )
      )
    }
  }
  data <- data |>
    dplyr::mutate(color_combined = construct_color_variable(data, colorVars))

  if (is.null(facetVarX)) {
    data$overall <- "overall"
    facetVarX <- "overall"
  }

  if (is.null(facetVarY)) {
    data$overall <- "overall"
    facetVarY <- "overall"
  }

  data <- data |>
    dplyr::mutate(
      facet_combined_x = construct_variable(data, facetVarX),
      facet_combined_y = construct_variable(data, facetVarY)
    )

  if (!is.null(facet)) {
    data <- data |>
      tidyr::unite("facet_var",
        c(dplyr::all_of(.env$facet)),
        remove = FALSE, sep = "; "
      )
  }

  checkmate::assertTRUE(any(xAxis == "estimate_value", yAxis == "estimate_value"), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  df_dates <- data |> dplyr::filter(.data$estimate_type == "date")
  if (plotStyle != "density") {
    df_non_dates <- data |>
      dplyr::filter(!(.data$estimate_type %in% c("date", "logical"))) |>
      dplyr::mutate(estimate_value = round(as.numeric(.data$estimate_value), 2))
    if (nrow(df_non_dates) > 0) {
      df_non_dates <- df_non_dates |>
        dplyr::mutate(
          estimate_value =
            dplyr::if_else(.data$estimate_name == "percentage",
              .data$estimate_value / 100,
              .data$estimate_value
            )
        )
    }
  } else {
    df_non_dates <- data |>
      dplyr::filter(!(.data$estimate_type %in% c("date", "logical")))
  }

  # Start constructing the plot
  if (plotStyle == "scatterplot") {
    if (nrow(df_non_dates) > 0) {
      df_percent <- df_non_dates |> dplyr::filter(.data$estimate_name == "percentage")
      df_non_percent <- df_non_dates |> dplyr::filter(.data$estimate_name != "percentage")

      make_plot <- function(data, is_percent = FALSE) {
        if ("color_combined" %in% names(data)) {
          plot <- data |> ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes(
              x = !!rlang::sym(xAxis),
              y = !!rlang::sym(yAxis),
              color = .data$color_combined
            ))
          plot <- plot + ggplot2::labs(color = "Color")
        } else {
          plot <- data |> ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes(
              x = !!rlang::sym(xAxis),
              y = !!rlang::sym(yAxis)
            ))
        }

        if (is_percent) {
          if (xAxis == "estimate_value") {
            plot <- plot + ggplot2::scale_x_continuous(
              labels = scales::percent_format(accuracy = 1)
            )
          } else if (yAxis == "estiamte_value") {
            plot <- plot + ggplot2::scale_y_continuous(
              labels = scales::percent_format(accuracy = 1)
            )
          }
        }

        return(plot)
      }

      p_percent <- if (nrow(df_percent) > 0) {
        make_plot(df_percent, TRUE)
      } else {
        NULL
      }
      p_non_percent <- if (nrow(df_non_percent) > 0) {
        make_plot(df_non_percent, FALSE)
      } else {
        NULL
      }
    } else {
      p_percent <- p_non_percent <- NULL
    }
  } else if (plotStyle == "barplot" || plotStyle == "density") {
    if (nrow(df_non_dates) > 0) {
      # Separate data based on 'estimate_name'
      df_percent <- df_non_dates |> dplyr::filter(.data$estimate_name == "percentage")
      df_non_percent <- df_non_dates |> dplyr::filter(.data$estimate_name != "percentage")

      # Function to create bar plots
      create_bar_plot <- function(data, plotStyle, is_percent = FALSE) {
        if (plotStyle == "barplot") {
          if ("color_combined" %in% names(data)) {
            plot <- data |> ggplot2::ggplot(ggplot2::aes(
              x = !!rlang::sym(xAxis),
              y = !!rlang::sym(yAxis),
              fill = .data$color_combined
            )) +
              ggplot2::geom_col() +
              ggplot2::labs(fill = "Color")
          } else {
            plot <- ggplot2::ggplot(data, ggplot2::aes(
              x = !!rlang::sym(xAxis),
              y = !!rlang::sym(yAxis)
            )) +
              ggplot2::geom_col()
          }


          # Apply percent formatting if data is 'percentage' and for the correct axis
          if (is_percent) {
            if (xAxis == "estimate_value") {
              plot <- plot + ggplot2::scale_x_continuous(
                labels = scales::percent_format(accuracy = 1)
              )
            } else if (yAxis == "estimate_value") {
              plot <- plot + ggplot2::scale_y_continuous(
                labels = scales::percent_format(accuracy = 1)
              )
            }
          }
        } else if (plotStyle == "density") {
          data <- data |>
            dplyr::filter(.data$variable_name == "density") |>
            dplyr::mutate(estimate_value = as.numeric(.data$estimate_value))
          group_columns <- data |>
            dplyr::select(-c(
              "estimate_value", "estimate_name", "variable_level",
              if ("facet_combined_x" %in% names(df_non_dates)) "facet_combined_x" else NULL,
              if ("facet_combined_y" %in% names(df_non_dates)) "facet_combined_y" else NULL,
              if ("color_combined" %in% names(df_non_dates)) "color_combined" else NULL
            )) |>
            dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.) > 1)) |>
            dplyr::select(dplyr::where(~.)) |>
            names()

          data$group_identifier <- interaction(data |>
            dplyr::select(dplyr::all_of(group_columns)))

          density_data_wide <- data |>
            dplyr::mutate(estimate_value = as.list(.data$estimate_value)) |>
            tidyr::pivot_wider(names_from = "estimate_name", values_from = "estimate_value") |>
            tidyr::unnest(dplyr::everything())

          if ("color_combined" %in% names(density_data_wide)) {
            plot <- density_data_wide |>
              ggplot2::ggplot() +
              ggplot2::geom_line( # or geom_area() for filled areas
                ggplot2::aes(
                  x = .data$x, # Assuming 'x' is your data column
                  y = .data$y, # Pre-calculated density or other metric
                  group = .data$group_identifier, # Grouping variable
                  color = .data$color_combined # Use color to differentiate if it's a line plot
                  # fill = color_combined # Use fill instead of color if it's geom_area()
                )
              ) +
              ggplot2::labs(color = "Color")
          } else {
            plot <- density_data_wide |>
              ggplot2::ggplot() +
              ggplot2::geom_line( # or geom_area() for filled areas
                ggplot2::aes(
                  x = .data$x, # Assuming 'x' is your data column
                  y = .data$y, # Pre-calculated density or other metric
                  group = .data$group_identifier, # Grouping variable
                  # fill = color_combined # Use fill instead of color if it's geom_area()
                )
              ) +
              ggplot2::labs(color = "Color")
          }
        }
        return(plot)
      }

      if (plotStyle == "barplot") {
        # Create plots
        p_percent <- if (nrow(df_percent) > 0) {
          create_bar_plot(df_percent,
            plotStyle = "barplot",
            is_percent = TRUE
          )
        } else {
          NULL
        }
        p_non_percent <- if (nrow(df_non_percent) > 0) {
          create_bar_plot(df_non_percent,
            plotStyle = "barplot",
            is_percent = FALSE
          )
        } else {
          NULL
        }
      } else if (plotStyle == "density") {
        p_percent <- NULL
        p_non_percent <- if (nrow(df_non_percent) > 0) {
          create_bar_plot(df_non_percent,
            is_percent = FALSE,
            plotStyle = "density"
          )
        }
      }
    }
  } else if (plotStyle == "boxplot") {
    if (nrow(df_non_dates) > 0) {
      df_non_dates <- df_non_dates |>
        dplyr::filter(.data$estimate_name %in% c("q25", "median", "q75", "min", "max")) |>
        dplyr::mutate(
          estimate_value = as.numeric(.data$estimate_value),
          estimate_type = "numeric"
        )
      non_numeric_cols <- df_non_dates |>
        dplyr::select(-c(
          "estimate_value", "estimate_name",
          if ("facet_combined_x" %in% names(df_non_dates)) "facet_combined_x" else NULL,
          if ("facet_combined_y" %in% names(df_non_dates)) "facet_combined_y" else NULL,
          if ("color_combined" %in% names(df_non_dates)) "color_combined" else NULL
        )) |>
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.) > 1)) |>
        dplyr::select(dplyr::where(~.)) |>
        names()

      df_non_dates_wide <- df_non_dates |>
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(colnames(
            df_non_dates |>
              dplyr::select(-c("estimate_name", "estimate_value"))
          )),
          names_from = "estimate_name",
          values_from = "estimate_value"
        )


      if (length(non_numeric_cols) > 0) {
        df_non_dates_wide$group_identifier <- interaction(df_non_dates_wide |>
          dplyr::select(dplyr::all_of(non_numeric_cols)))
      } else {
        df_non_dates_wide$group_identifier <- "overall"
      }
    }

    if (nrow(df_dates) > 0) {
      df_dates <- df_dates |>
        dplyr::filter(.data$estimate_name %in% c("q25", "median", "q75", "min", "max")) |>
        dplyr::mutate(estimate_value = as.Date(.data$estimate_value))

      df_dates_wide <- df_dates |>
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(colnames(df_dates |>
            dplyr::select(-c(
              "estimate_name",
              "estimate_value"
            )))),
          names_from = "estimate_name", values_from = "estimate_value"
        )
      if (length(non_numeric_cols) > 0) {
        df_dates_wide$group_identifier <- interaction(df_dates_wide |>
          dplyr::select(
            dplyr::all_of(non_numeric_cols)
          ))
      } else {
        df_dates_wide$group_identifier <- "overall"
      }
    }

    # Check if the dataframe has rows to plot
    if (nrow(df_non_dates) > 0) {
      xcol <- ifelse(xAxis == "estimate_value", yAxis, xAxis)
      p_non_dates <- df_non_dates_wide |> ggplot2::ggplot(
        ggplot2::aes(x = .data[[xcol]])
      )

      if ("color_combined" %in% names(df_non_dates_wide)) {
        if (!all(is.na(df_non_dates_wide$color_combined))) {
          p_non_dates <- p_non_dates + ggplot2::aes(color = .data$color_combined) +
            ggplot2::labs(color = "Color")
        }
      }

      p_non_dates <- p_non_dates + ggplot2::geom_boxplot(
        ggplot2::aes(
          group = .data$group_identifier,
          lower = .data$q25,
          upper = .data$q75,
          middle = .data$median,
          ymin = .data$min,
          ymax = .data$max
        ),
        stat = "identity"
      )

      # Determine if the plot should be horizontal or vertical based on 'estimate_value'
      if (xAxis == "estimate_value") {
        # Horizontal plot
        p_non_dates <- p_non_dates +
          ggplot2::coord_flip()
      }
    } else {
      # Setup for empty data
      p_non_dates <- NULL
    }

    if (nrow(df_dates) > 0) {
      xcol <- ifelse(xAxis == "estimate_value", yAxis, xAxis)

      p_dates <- df_dates_wide |> ggplot2::ggplot(
        ggplot2::aes(x = .data[[xcol]])
      ) +
        ggplot2::labs(
          title = "Date Data",
          x = "Variable and Group Level",
          y = "Quantile Values"
        )

      if ("color_combined" %in% names(df_dates_wide)) {
        if (!all(is.na(df_dates_wide$color_combined))) {
          p_dates <- p_dates + ggplot2::aes(color = .data$color_combined) +
            ggplot2::labs(color = "Color")
        }
      }

      p_dates <- p_dates + ggplot2::geom_boxplot(
        ggplot2::aes(
          group = .data$group_identifier,
          lower = .data$q25,
          upper = .data$q75,
          middle = .data$median,
          ymin = .data$min,
          ymax = .data$max
        ),
        stat = "identity"
      ) +
        ggplot2::labs(
          title = "Date Data", x = "Variable and Group Level",
          y = "Quantile Values"
        )
      # Determine if the plot should be horizontal or vertical based on 'estimate_value'
      if (xAxis == "estimate_value") {
        # Horizontal plot
        p_dates <- p_dates +
          ggplot2::coord_flip()
      }
    } else {
      p_dates <- NULL
    }
  }

  if (suppressWarnings(!is.null(data$facet_combined_x) || !is.null(data$facet_combined_y))) {
    if (plotStyle == "scatterplot") {
      # Apply facet grid if either plot exists
      if (!is.null(p_percent) || !is.null(p_non_percent)) {
        if (!is.null(p_percent)) {
          theme_modification <- ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            )
          )

          facet_x_exists <- "facet_combined_x" %in% names(df_percent)
          facet_y_exists <- "facet_combined_y" %in% names(df_percent)

          # Construct the faceting formula based on the existence of the variables
          facet_formula <- paste0(
            ifelse(facet_y_exists, "facet_combined_y", "."),
            " ~ ",
            ifelse(facet_x_exists, "facet_combined_x", ".")
          )
          p_percent <- p_percent + ggplot2::facet_grid(
            rows = facet_formula,
            scales = "free"
          )
          if (vertical_x) {
            p_percent <- p_percent + theme_modification
          }
        }

        if (!is.null(p_non_percent)) {
          facet_x_exists <- "facet_combined_x" %in% names(df_non_percent)
          facet_y_exists <- "facet_combined_y" %in% names(df_non_percent)

          # Construct the faceting formula based on the existence of the variables
          facet_formula <- paste0(
            ifelse(facet_y_exists, "facet_combined_y", "."),
            " ~ ",
            ifelse(facet_x_exists, "facet_combined_x", ".")
          )
          p_non_percent <- p_non_percent + ggplot2::facet_grid(
            rows = facet_formula,
            scales = "free"
          )
          if (vertical_x) {
            p_non_percent <- p_non_percent + theme_modification
          }
        }
      }

      # Combine the plots or select the appropriate one
      p <- if (!is.null(p_percent) && !is.null(p_non_percent)) {
        ggpubr::ggarrange(p_percent, p_non_percent, nrow = 2)
      } else if (!is.null(p_percent)) {
        p_percent
      } else if (!is.null(p_non_percent)) {
        p_non_percent
      } else {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Numeric Data Provided",
            subtitle = "Scatterplot only supported numeric values"
          )
      }
    } else if (plotStyle == "boxplot") {
      if (!is.null(p_dates)) {
        facet_x_exists <- "facet_combined_x" %in% names(df_dates)
        facet_y_exists <- "facet_combined_y" %in% names(df_dates)

        # Construct the faceting formula based on the existence of the variables
        facet_formula <- paste0(
          ifelse(facet_y_exists, "facet_combined_y", "."),
          " ~ ",
          ifelse(facet_x_exists, "facet_combined_x", ".")
        )

        p_dates <- p_dates +
          ggplot2::facet_grid(rows = facet_formula, scales = "free")
        if (vertical_x) {
          p_dates <- p_dates + ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5
          ))
        }
      }
      if (!is.null(p_non_dates)) {
        facet_x_exists <- "facet_combined_x" %in% names(df_non_dates)
        facet_y_exists <- "facet_combined_y" %in% names(df_non_dates)

        # Construct the faceting formula based on the existence of the variables
        facet_formula <- paste0(
          ifelse(facet_y_exists, "facet_combined_y", "."),
          " ~ ",
          ifelse(facet_x_exists, "facet_combined_x", ".")
        )

        p_non_dates <- p_non_dates +
          ggplot2::facet_grid(rows = facet_formula, scales = "free")
        if (vertical_x) {
          p_non_dates <- p_non_dates + ggplot2::theme(
            axis.text.x =
              ggplot2::element_text(
                angle = 90,
                hjust = 1,
                vjust = 0.5
              )
          )
        }
      }

      p <- if (!is.null(p_dates) && !is.null(p_non_dates)) {
        ggpubr::ggarrange(p_dates, p_non_dates, nrow = 2)
      } else if (!is.null(p_dates)) {
        p_dates
      } else if (!is.null(p_non_dates)) {
        p_non_dates
      } else {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Data Provided",
            subtitle = "Boxplot needs to have min max q25 q75 in estimate_name"
          )
      }
    } else if (plotStyle == "barplot" || plotStyle == "density") {
      if (!is.null(p_percent)) {
        facet_x_exists <- "facet_combined_x" %in% names(df_percent)
        facet_y_exists <- "facet_combined_y" %in% names(df_percent)

        # Construct the faceting formula based on the existence of the variables
        facet_formula <- paste0(
          ifelse(facet_y_exists, "facet_combined_y", "."),
          " ~ ",
          ifelse(facet_x_exists, "facet_combined_x", ".")
        )

        p_percent <- p_percent +
          ggplot2::facet_grid(rows = facet_formula, scales = "free")

        if (vertical_x) {
          p_percent <- p_percent + ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5
          ))
        }
      }

      if (!is.null(p_non_percent)) {
        facet_x_exists <- "facet_combined_x" %in% names(df_non_percent)
        facet_y_exists <- "facet_combined_y" %in% names(df_non_percent)

        # Construct the faceting formula based on the existence of the variables
        facet_formula <- paste0(
          ifelse(facet_y_exists, "facet_combined_y", "."),
          " ~ ",
          ifelse(facet_x_exists, "facet_combined_x", ".")
        )


        p_non_percent <- p_non_percent +
          ggplot2::facet_grid(rows = facet_formula, scales = "free")
        if (vertical_x) {
          p_non_percent <- p_non_percent +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            ))
        }
      }

      # Combine the plots
      p <- if (!is.null(p_percent) && !is.null(p_non_percent)) {
        ggpubr::ggarrange(p_percent, p_non_percent, nrow = 2)
      } else if (!is.null(p_percent)) {
        p_percent
      } else if (!is.null(p_non_percent)) {
        p_non_percent
      } else {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Numeric Data Provided",
            subtitle = "Barplot only supported numeric values"
          )
      }
    }
  } else {
    if (plotStyle == "barplot" || plotStyle == "density") {
      p <- if (!is.null(p_percent) && !is.null(p_non_percent)) {
        if (vertical_x) {
          p_percent <- p_percent +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            ))
          p_non_percent <- p_non_percent +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            ))
        }
        ggpubr::ggarrange(p_percent, p_non_percent, nrow = 2)
      } else if (!is.null(p_percent)) {
        if (vertical_x) {
          p_percent <- p_percent +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            ))
        }
        p_percent
      } else if (!is.null(p_non_percent)) {
        if (vertical_x) {
          p_non_percent <- p_non_percent +
            ggplot2::theme(axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            ))
        }
        p_non_percent
      } else {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Numeric Data Provided",
            subtitle = "Barplot only supported numeric values"
          )
      }
    } else if (plotStyle == "boxplot") {
      if (!is.null(p_dates) || !is.null(p_non_dates)) {
        if (!is.null(p_dates) && is.null(p_non_dates)) {
          if (vertical_x) {
            p_dates <- p_dates +
              ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                vjust = 0.5
              ))
          }
          p <- p_dates
        } else if (is.null(p_dates) && !is.null(p_non_dates)) {
          if (vertical_x) {
            p_non_dates <- p_non_dates +
              ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                vjust = 0.5
              ))
          }
          p <- p_non_dates
        } else {
          if (vertical_x) {
            p_dates <- p_dates +
              ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                vjust = 0.5
              ))
            p_non_dates <- p_non_dates +
              ggplot2::theme(axis.text.x = ggplot2::element_text(
                angle = 90,
                hjust = 1,
                vjust = 0.5
              ))
          }
          p <- ggpubr::ggarrange(p_dates, p_non_dates, nrow = 2)
        }
      }
    } else if (plotStyle == "scatterplot") {
      p <- if (!is.null(p_percent) && !is.null(p_non_percent)) {
        ggpubr::ggarrange(p_percent, p_non_percent, nrow = 2)
      } else if (!is.null(p_percent)) {
        p_percent
      } else if (!is.null(p_non_percent)) {
        p_non_percent
      } else {
        ggplot2::ggplot() +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = "No Numeric Data Provided",
            subtitle = "Scatterplot only supported numeric values"
          )
      }
    }
  }

  if (!is.null(facet)) {
    facetNcols <- NULL
    if ("facetNcols" %in% names(.options)) {
      facetNcols <- .options[["facetNcols"]]
    }
    facetScales <- "fixed"
    if ("facetScales" %in% names(.options)) {
      facetScales <- .options[["facetScales"]]
    }
    p <- p +
      ggplot2::facet_wrap(ggplot2::vars(.data$facet_var),
        ncol = facetNcols,
        scales = facetScales
      )
  }

  return(p)
}

construct_variable <- function(data, facet_vars) {
  if (!is.null(facet_vars) && length(facet_vars) > 1) {
    unique_val_vars <- sapply(facet_vars, function(var) {
      dplyr::n_distinct(data[[var]], na.rm = TRUE) > 1
    })

    valid_vars <- facet_vars[unique_val_vars]

    if (length(valid_vars) > 1) {
      return(as.factor(interaction(data |> dplyr::select(dplyr::all_of(valid_vars)), sep = ".")))
    } else if (length(valid_vars) == 1) {
      return(as.factor(data[[valid_vars]]))
    }
  } else if (!is.null(facet_vars) && length(facet_vars) == 1) {
    if (dplyr::n_distinct(data[[facet_vars]], na.rm = TRUE) > 1) {
      return(as.factor(data[[facet_vars]]))
    }
  }
  return(NULL)
}

construct_color_variable <- function(data, color_vars) {
  if (!is.null(color_vars) && length(color_vars) >= 1) {
    combined_factor <- interaction(dplyr::select(
      data,
      dplyr::all_of(color_vars)
    ), sep = ".")
    return(as.factor(combined_factor))
  }
  return(NULL)
}

getUniqueCombinationsSr <- function(x) {
  xUniques <- x |>
    dplyr::select("group_name", "group_level") |>
    dplyr::distinct() |>
    visOmopResults::splitGroup() |>
    dplyr::mutate(id = dplyr::row_number())
  pairs <- xUniques |>
    dplyr::inner_join(
      xUniques |>
        dplyr::rename(
          "cohort_name_reference" = "cohort_name_comparator",
          "cohort_name_comparator" = "cohort_name_reference"
        ),
      by = c("cohort_name_comparator", "cohort_name_reference"),
      suffix = c("_x", "_y")
    ) |>
    dplyr::filter(.data$id_x < .data$id_y) |>
    dplyr::select("cohort_name_comparator", "cohort_name_reference") |>
    visOmopResults::uniteGroup(
      cols = c("cohort_name_reference", "cohort_name_comparator")
    )
  x <- x |>
    dplyr::inner_join(pairs, by = c("group_name", "group_level"))
  return(x)
}
getUniqueCombinations <- function(x, order) {
  dataCohortRef <- unique(x$cohort_name_reference)
  order <- order[order %in% dataCohortRef]
  for (i in 2:length(order)) {
    x <- x |>
      dplyr::anti_join(
        x |>
          dplyr::filter(
            .data$cohort_name_reference == .env$order[i],
            .data$cohort_name_comparator %in% .env$order[1:(i - 1)]
          ),
        by = colnames(x)
      )
  }
  return(x)
}

getTidyOverlap <- function(x) {
  cohort_counts <- x |>
    dplyr::filter(.data$cohort_name_reference == .data$cohort_name_comparator)
  byCol <- colnames(x)
  x <- x |>
    dplyr::filter(.data$cohort_name_reference != .data$cohort_name_comparator) |>
    dplyr::mutate(variable_level = "overlap") |>
    tidyr::pivot_wider(
      names_from = c("variable_level"),
      values_from = "estimate_value"
    ) |>
    dplyr::left_join(
      cohort_counts |>
        dplyr::mutate(variable_level = "reference") |>
        tidyr::pivot_wider(
          names_from = c("variable_level"),
          values_from = "estimate_value"
        ) |>
        dplyr::select(!"cohort_name_comparator"),
      by = byCol[!byCol %in% c("cohort_name_comparator", "variable_level", "estimate_value")]
    ) |>
    dplyr::left_join(
      cohort_counts |>
        dplyr::mutate(variable_level = "comparator") |>
        tidyr::pivot_wider(
          names_from = c("variable_level"),
          values_from = "estimate_value"
        ) |>
        dplyr::select(!"cohort_name_reference"),
      by = byCol[!byCol %in% c("cohort_name_reference", "variable_level", "estimate_value")]
    )
  return(x)
}

assertDensityEstimates <- function(x) {
  x <- x |>
    dplyr::mutate(
      estimate_name = dplyr::if_else(
        .data$estimate_name == "median", "q50", .data$estimate_name
      ),
      plot_id = paste0(
        .data$result_id, "; ", .data$cdm_name, "; ", .data$cohort_name_reference, "; ", .data$cohort_name_comparator, "; ",
        .data$strata_name, "; ", .data$strata_level
      )
    ) |>
    dplyr::filter(.data$estimate_name %in% c("x", "y", "q50")) |>
    dplyr::select(dplyr::all_of(
      c(
        "plot_id", "timing_label", "result_id", "cdm_name", "cohort_name_reference", "cohort_name_comparator", "strata_name", "strata_level",
        "estimate_name", "estimate_value"
      )
    ))

  if (!all(c("x", "y") %in% unique(x$estimate_name))) {
    cli::cli_abort("To generate a density plot, cohort_timing must have the estimates: x, y.
                   Additionally, if q50 (or median) is in the data, it will be included in the plot.")
  }

  xMedian <- x |>
    dplyr::filter(.data$estimate_name == "q50") |>
    dplyr::select("plot_id", "q50" = "estimate_value") |>
    dplyr::distinct()

  x <- x |> dplyr::filter(.data$estimate_name != "q50")

  x <- lapply(as.list(unique(x$plot_id)), function(plot_id, data = x) {
    data <- data |>
      dplyr::filter(.data$plot_id == .env$plot_id)
    data <- data |>
      dplyr::select(!dplyr::all_of(c("estimate_name", "estimate_value"))) |>
      dplyr::distinct() |>
      dplyr::left_join(
        dplyr::tibble(
          plot_id = plot_id,
          x = data$estimate_value[data$estimate_name == "x"],
          y = data$estimate_value[data$estimate_name == "y"]
        ),
        by = "plot_id"
      )
  }) |>
    dplyr::bind_rows() |>
    dplyr::left_join(xMedian, by = "plot_id")

  return(x)
}

emptyPlot <- function(title = "No result to plot",
                      subtitle = "") {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    )
}
