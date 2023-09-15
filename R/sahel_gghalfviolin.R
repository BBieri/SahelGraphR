#' High-Level Wrapper Half Violin Plots per Country
#'
#' This function is a convenience wrapper for the violin plots within the Sahel
#' ASP plot. If you want a custom plot, check out the `geom_flat_violin_l()`
#' and `geom_flat_violin_r()` functions in this package.
#'
#' @param data A data frame
#' @param variable Variable name to plot. Continuous or count.
#' @param countries Variable name to plot. Continuous or count.
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param xtitle Plot x-axis title
#' @param ytitle Plot y-axis title
#' @param scale Scaling function from `{ggplot2}`
#' @param xlim Define plot limit on x-axis. Supply a vector of bounds, `c(325, 500)`.
#' @param ylim Define plot limit on y-axis. Supply a vector of bounds, `c(325, 500)`.
#' @param add_boxplot `TRUE` by default. Add a boxplot inside the violin plots?
#' @param theme Theme applied
#' @param orientation Orientation of the graph
#' @param alpha_boxplot Transparency for the boxplot element.
#' @param alpha_violin Transparency for the violin element.
#' @param width_boxplot Width for the boxplot element.
#' @param width_violin Width for the violin element.
#'
#' @return A ggplot object of half violin style.
#' @export
#'
#' @examples
#' library(patchwork)
#'
#' # Vertical Left
#' sahel_gghalfviolin(
#'   data = sahel_sim,
#'   variable = hh_income,
#'   countries = country_names,
#'   title = "Household Income",
#'   scale = ggplot2::scale_y_log10()
#' ) + sahel_gghalfviolin(
#'   data = sahel_sim,
#'   variable = hh_consumption,
#'   countries = country_names,
#'   title = "Household Consumption",
#'   scale = ggplot2::scale_y_log10()
#' )
#'
#' # Vertical Right
#' sahel_gghalfviolin(
#'   data = sahel_sim,
#'   variable = hh_income,
#'   countries = country_names,
#'   orientation = "Vertical-Right",
#'   title = "Household Income",
#'   scale = ggplot2::scale_y_log10()
#' ) + sahel_gghalfviolin(
#'   data = sahel_sim,
#'   variable = hh_consumption,
#'   countries = country_names,
#'   orientation = "Vertical-Right",
#'   title = "Household Consumption",
#'   scale = ggplot2::scale_y_log10()
#' )
#'
#' # Horizontal
#' sahel_gghalfviolin(
#'   data = sahel_sim,
#'   variable = hh_income,
#'   countries = country_names,
#'   orientation = "Horizontal",
#'   title = "Household Income",
#'   scale = ggplot2::scale_y_log10()
#' ) + sahel_gghalfviolin(
#'   data = sahel_sim,
#'   variable = hh_consumption,
#'   countries = country_names,
#'   orientation = "Horizontal",
#'   title = "Household Consumption",
#'   scale = ggplot2::scale_y_log10()
#' )

sahel_gghalfviolin <- function(data, variable, countries,
                               orientation = "Vertical-Left",
                               alpha_boxplot = 0.5,
                               alpha_violin = 0.8,
                               width_boxplot = 0.5,
                               width_violin = 0.8,
                               title = deparse(substitute(variable)),
                               subtitle = "Half-Violin Plot",
                               caption = NULL,
                               xtitle = NULL,
                               ytitle = NULL,
                               scale = ggplot2::geom_blank(),
                               xlim = NULL,
                               ylim = NULL,
                               add_boxplot = TRUE,
                               theme = themeaspdark){

  # Process color vector if it is a named vector:
  if (!is.null(names(colors))) {
    colors <- unname(colors)
  }

  if (add_boxplot == TRUE) {
    boxplot <- ggplot2::geom_boxplot(width = width_boxplot, color = "grey90",
                                     alpha = alpha_boxplot,
                                     position = ggplot2::position_dodge(0.9))
  }

  if (orientation == "Vertical-Left") {
    violinplot <-
      geom_flat_violin_l(
        alpha = alpha_violin,
        position = ggplot2::position_dodge(1),
        width = width_violin
      )
    coordflip <- ggplot2::coord_cartesian(
      xlim = xlim,
      ylim = ylim,
      expand = TRUE,
      default = FALSE,
      clip = "on"
    )
  } else if (orientation == "Vertical-Right") {
    violinplot <-
      geom_flat_violin_r(
        alpha = alpha_violin,
        position = ggplot2::position_dodge(1),
        width = width_violin
      )
    coordflip <- ggplot2::coord_cartesian(
      xlim = xlim,
      ylim = ylim,
      expand = TRUE,
      default = FALSE,
      clip = "on"
    )
  } else if (orientation == "Horizontal") {
    violinplot <-
      geom_flat_violin_r(
        alpha = alpha_violin,
        position = ggplot2::position_dodge(1),
        width = width_violin
      )
    coordflip <- ggplot2::coord_flip(xlim = xlim,
                                     ylim = ylim)
  } else {
    stop("Incorrect orientation argument.")
  }

  ggplot2::ggplot(data, ggplot2::aes(x = {{ countries }},
                                     y = {{ variable }},
                                     fill = {{ countries }})) +
    violinplot +
    boxplot +
    coordflip +
    ggplot2::scale_fill_manual(
      unique(data[[deparse(substitute(countries))]]),
      values = unname(asp_palettes$Countries)
    ) +
    scale +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = xtitle,
      y = ytitle
    ) +
    theme() +
    ggplot2::theme(legend.position = "none")
}
