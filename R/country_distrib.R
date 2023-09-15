#'
#' #' Sahel Outcome Distribution Plot Helper
#' #'
#' #' Not sure how to plot the distributions of the variables you have, here is a
#' #' little helper function to help you do just that!
#' #'
#' #' @param data A data frame prepared with
#' #' @param var_type Type of variable. One of `c("Binary", "Count", "Continuous")`.
#' #' @param plot_type Type of Plot. One of
#' #' @param countries Vector of countries to plot. `c("Burkina Faso", "Niger", "Mauritania", "Senegal")` by default.
#' #' @param colors Vector of colors corresponding
#' #' @param title Plot title passed to `ggplot2::labs()`.
#' #' @param subtitle Plot subtitle passed to `ggplot2::labs()`.
#' #' @param caption Plot caption passed to `ggplot2::labs()`.
#' #' @param xtitle Plot x axis title passed to `ggplot2::labs()`.
#' #' @param ytitle Plot y axis title passed to `ggplot2::labs()`.
#' #' @param filltitle Plot title of the filling passed to `ggplot2::labs()`.
#' #' @param scale Plot title passed to `ggplot2::labs()`.
#' #' @param theme Name of the theme function applied to the resulting plot.
#' #'
#' #' @return A joint plot of all the variable distributions to compare them across countries.
#' #' @export
#' #'
#' #' @examples
#' #'
#'
#' sahel_plot_distrib <- function(data,
#'                                var_type = c("Binary", "Count", "Continuous"),
#'                                plot_type = c("Barplot Vert.",
#'                                              "Barplot Horiz.",
#'                                              "Ridge",
#'                                              "Boxplot",
#'                                              "Violin",
#'                                              "Half Violin"),
#'                                countries = c("Burkina Faso", "Niger",
#'                                              "Mauritania", "Senegal"),
#'                                colors = asp_palettes$Countries,
#'                                title = "",
#'                                subtitle = "",
#'                                caption = "",
#'                                xtitle = "",
#'                                ytitle = "",
#'                                filltitle = "",
#'                                scale = NULL,
#'                                theme = themeaspdark) {
#'
#'   # Check if var_type, country names and plot_type are valid
#'   if (!(var_type %in% c("Binary", "Count", "Continuous"))) {
#'     stop("Invalid var_type.")
#'   }
#'   if (!(
#'     plot_type %in% c(
#'       "Barplot Vert.",
#'       "Barplot Horiz.",
#'       "Ridge",
#'       "Boxplot",
#'       "Violin",
#'       "Half Violin"
#'     )
#'   )) {
#'     stop("Invalid plot_type.")
#'   }
#'   # Check if there are the same number of colors than countries
#'   if (length(countries) != length(colors)) {
#'     stop("Provide vectors of the same length for the color and countries arguments.")
#'   }
#'
#'   # Process color vector if it is a named vector:
#'   if (!is.null(names(colors))) {
#'     colors <- unname(colors)
#'   }
#'
#'   # Plot the data ----
#'   ## Binary variable ----
#'   if (var_type == "Binary") {
#'     if (plot_type == "Barplot Vert.") {
#'       out <-
#'         ggplot2::ggplot(data,
#'                         ggplot2::aes(x = country_names, y = mean, fill = variable)) +
#'         ggplot2::geom_bar(
#'           stat = "identity",
#'           position = ggplot2::position_dodge(0.6),
#'           color = NA,
#'           width = 0.5
#'         ) +
#'         ggplot2::geom_errorbar(
#'           data = data,
#'           ggplot2::aes(ymin = mean - CI, ymax = mean + CI),
#'           colour = "grey90",
#'           alpha = 0.9,
#'           linewidth = .5,
#'           width = 0.2,
#'           position = ggplot2::position_dodge(0.6)
#'         ) +
#'         ggplot2::scale_fill_manual(
#'           labels = c("Has Children", "Owns Animals", "Female Head"),
#'           values = colors
#'         ) +
#'         ggplot2::labs(
#'           title = "Has Children, Animals, Female Head",
#'           subtitle = "Bar Plot (For binary variables)",
#'           caption = "Synthetic Data. 95% Confidence Intervals",
#'           x = "",
#'           y = "",
#'           fill = "Variable"
#'         ) +
#'         theme()
#'     } else if (plot_type == "Barplot Horiz.") {
#'       out <-
#'         ggplot2::ggplot(data,
#'                         ggplot2::aes(x = country_names, y = mean, fill = variable)) +
#'         ggplot2::geom_bar(
#'           stat = "identity",
#'           position = ggplot2::position_dodge(0.6),
#'           color = NA,
#'           width = 0.5
#'         ) +
#'         ggplot2::geom_errorbar(
#'           data = data,
#'           ggplot2::aes(ymin = mean - CI, ymax = mean + CI),
#'           colour = "grey90",
#'           alpha = 0.9,
#'           linewidth = .5,
#'           width = 0.2,
#'           position = ggplot2::position_dodge(0.6)
#'         ) +
#'         ggplot2::coord_flip() +
#'         ggplot2::scale_x_discrete(labels = countries) +
#'         ggplot2::scale_fill_manual(
#'           labels = c("Has Children", "Owns Animals", "Female Head"),
#'           values = colors
#'         ) +
#'         ggplot2::labs(
#'           title = "Has Children, Animals, Female Head",
#'           subtitle = "Bar Plot (For binary variables)",
#'           caption = "Synthetic Data. 95% Confidence Intervals",
#'           x = "",
#'           y = "",
#'           fill = "Country"
#'         ) +
#'         theme()
#'     }
#'   } else if (var_type == "Count") {
#'     ## Count variable ----
#'     if (plot_type == "Ridge") {
#'       out <- ggplot2::ggplot(data) +
#'         geom_density_ridges(ggplot2::aes(x = value, y = variable, fill = country_names),
#'                             alpha = .5) +
#'         ggplot2::scale_fill_manual(
#'           countries,
#'           values = colors
#'         ) +
#'         ggplot2::scale_y_discrete(labels = c("Children", "Animals", "Years of education")) +
#'         ggplot2::labs(
#'           title = "Number of Kids, Animals, and Years of Education",
#'           subtitle = "Ridgeline Plot",
#'           caption = "Synthetic Data",
#'           x = "Count",
#'           y = "",
#'           fill = "Variable"
#'         ) +
#'         theme()
#'     } else if (plot_type == "Boxplot") {
#'       out <- ggplot2::ggplot(data,
#'                              ggplot2::aes(x = value, y = variable, fill = country_names)) +
#'         ggplot2::geom_boxplot(alpha = 0.8, color = "grey85") +
#'         ggplot2::scale_fill_manual(
#'           countries,
#'           values = colors
#'         ) +
#'         ggplot2::scale_y_discrete(labels = c("Children", "Animals", "Years of education")) +
#'         ggplot2::labs(
#'           title = "Number of Kids, Animals, and Years of Education",
#'           subtitle = "Ridgeline Plot",
#'           caption = "Synthetic Data",
#'           x = "Count",
#'           y = "",
#'           fill = "Variable"
#'         ) +
#'         theme()
#'     } else if (plot_type == "Violin") {
#'       out <- ggplot2::ggplot(data,
#'                              ggplot2::aes(x = value, y = variable, fill = country_names)) +
#'         ggplot2::geom_violin(alpha = 0.9) +
#'         ggplot2::geom_boxplot(
#'           width = 0.1,
#'           color = "grey90",
#'           alpha = 0.2,
#'           position = ggplot2::position_dodge(0.9)
#'         ) +
#'         ggplot2::scale_fill_manual(
#'           countries,
#'           values = colors
#'         ) +
#'         ggplot2::scale_y_discrete(labels = c("Children", "Animals", "Years of education")) +
#'         ggplot2::scale_x_log10() +
#'         ggplot2::labs(
#'           title = "Number of Kids, Animals, and Years of Education",
#'           subtitle = "Ridgeline Plot",
#'           caption = "Synthetic Data",
#'           x = "Count",
#'           y = "",
#'           fill = "Variable"
#'         ) +
#'         theme()
#'     } else if (plot_type == "Half Violin") {
#'       out <-
#'         ggplot2::ggplot(data,
#'                         ggplot2::aes(x = value, y = variable, fill = country_names)) +
#'         geom_flat_violin_h(
#'           alpha = 0.9,
#'           position = ggplot2::position_dodge(1),
#'           width = 3
#'         ) +
#'         ggplot2::geom_boxplot(
#'           width = 0.1,
#'           color = "grey90",
#'           alpha = 0.2,
#'           position = ggplot2::position_dodge(1)
#'         ) +
#'         ggplot2::scale_fill_manual(
#'           countries,
#'           values = colors
#'         ) +
#'         ggplot2::scale_y_discrete(labels = c("Children", "Animals", "Years of education")) +
#'         ggplot2::scale_x_log10() +
#'         ggplot2::labs(
#'           title = "Number of Kids, Animals, and Years of Education",
#'           subtitle = "Ridgeline Plot",
#'           caption = "Synthetic Data",
#'           x = "Count",
#'           y = "",
#'           fill = "Variable"
#'         ) +
#'         theme()
#'     }
#'   } else if (var_type == "Continuous") {
#'     ## Continuous variable ----
#'     if (plot_type == "Ridge") {
#'       out <- ggplot2::ggplot(data) +
#'         geom_density_ridges(ggplot2::aes(x = value, y = variable, fill = country_names),
#'                             alpha = .5) +
#'         ggplot2::scale_fill_manual(
#'           countries,
#'           values = colors
#'         ) +
#'         ggplot2::scale_y_discrete(labels = c("Consumption", "Income")) +
#'         ggplot2::scale_x_log10() +
#'         ggplot2::labs(
#'           title = title,
#'           subtitle = "Ridgeline Plot",
#'           caption = "Synthetic Data",
#'           x = "2016 USD PPP",
#'           y = "",
#'           fill = "Variable"
#'         ) +
#'         theme()
#'     } else if (plot_type == "Boxplot") {
#'       out <- ggplot2::ggplot(
#'         data,
#'         ggplot2::aes(x = value, y = variable, fill = country_names)
#'       ) +
#'         ggplot2::geom_boxplot(alpha = 0.8, color = "grey85") +
#'         ggplot2::scale_fill_manual(
#'           countries,
#'           values = colors
#'         ) +
#'         ggplot2::scale_y_discrete(labels = c("Consumption", "Income")) +
#'         ggplot2::scale_x_log10() +
#'         ggplot2::labs(
#'           title = title,
#'           subtitle = "Boxplot",
#'           caption = "Synthetic Data",
#'           x = "2016 USD PPP",
#'           y = "",
#'           fill = "Variable"
#'         ) +
#'         theme()
#'     } else if (plot_type == "Violin") {
#'       out <- ggplot2::ggplot(
#'         data,
#'         ggplot2::aes(x = value, y = variable, fill = country_names)
#'       ) +
#'         ggplot2::geom_violin(alpha = 0.9) +
#'         ggplot2::geom_boxplot(
#'           width = 0.1,
#'           color = "grey90",
#'           alpha = 0.2,
#'           position = ggplot2::position_dodge(0.9)
#'         ) +
#'         ggplot2::scale_fill_manual(
#'           countries,
#'           values = colors
#'         ) +
#'         ggplot2::scale_y_discrete(labels = c("Consumption", "Income")) +
#'         ggplot2::scale_x_log10() +
#'         ggplot2::labs(
#'           title = title,
#'           subtitle = subtitle,
#'           caption = "Synthetic Data",
#'           x = "2016 USD PPP",
#'           y = "",
#'           fill = "Variable"
#'         ) +
#'         theme()
#'     } else if (plot_type == "Half Violin") {
#'       out <- ggplot2::ggplot(
#'         data,
#'         ggplot2::aes(x = value, y = variable, fill = country_names)
#'       ) +
#'         geom_flat_violin_h(
#'           alpha = 0.9,
#'           position = ggplot2::position_dodge(1),
#'           width = 3
#'         ) +
#'         ggplot2::geom_boxplot(
#'           width = 0.1,
#'           color = "grey90",
#'           alpha = 0.2,
#'           position = ggplot2::position_dodge(1)
#'         ) +
#'         ggplot2::scale_fill_manual(
#'           countries,
#'           values = colors
#'         ) +
#'         ggplot2::scale_y_discrete(labels = c("Consumption", "Income")) +
#'         ggplot2::scale_x_log10() +
#'         ggplot2::labs(
#'           title = title,
#'           subtitle = subtitle,
#'           caption = caption,
#'           x = xtitle,
#'           y = ytitle,
#'           fill = filltitle
#'         ) +
#'         theme()
#'     } else {
#'       stop("Non-appropriate plot selected.")
#'     }
#'   }
#'   out
#' }
