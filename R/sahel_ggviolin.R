#' Individual Violin Plots per Country
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
#' @param add_boxplot `TRUE` by default. Add a boxplot inside the violin plots?
#' @param theme Theme applied
#' @param alpha_boxplot Transparency for the boxplot element.
#' @param alpha_violin Transparency for the violin element.
#' @param width_boxplot Width for the boxplot element.
#' @param width_violin Width for the violin element.
#'
#' @return A ggplot object of violin style.
#' @export
#'
#' @examples
#' library(patchwork)
#' sahel_ggviolin(
#'   data = sahel_sim,
#'   variable = hh_income,
#'   countries = country_names,
#'   title = "Household Income",
#'   scale = ggplot2::scale_x_log10
#' ) + sahel_ggviolin(
#'   data = sahel_sim,
#'   variable = hh_consumption,
#'   countries = country_names,
#'   title = "Household Consumption",
#'   scale = ggplot2::scale_x_log10
#' )

sahel_ggviolin <- function(data, variable, countries,
                           alpha_boxplot = 0.5,
                           alpha_violin = 0.8,
                           width_boxplot = 0.5,
                           width_violin = 0.8,
                           title = deparse(substitute(variable)),
                           subtitle = "Violin Plot",
                           caption = NULL,
                           xtitle = NULL,
                           ytitle = NULL,
                           scale = NULL,
                           add_boxplot = TRUE,
                           theme = themeaspdark){

  # Process color vector if it is a named vector:
  if (!is.null(names(colors))) {
    colors <- unname(colors)
  }
  scale <- if (!is.null(scale))
    match.fun(scale)
  if (!is.function(scale)) stop("argument scale is not a function!")

  if (add_boxplot == TRUE) {
    boxplot <- ggplot2::geom_boxplot(ggplot2::aes(x = {{ variable }},
                                                  y = {{ countries }}),
                                     width = width_boxplot, color = "grey90",
                                     alpha = alpha_boxplot,
                                     position = ggplot2::position_dodge(0.9))
  }

  ggplot2::ggplot(data) +
    ggplot2::geom_violin(ggplot2::aes(x = {{ variable }},
                                       y = {{ countries }},
                                       fill = {{ countries }}),
                          alpha = alpha_violin, width = width_violin) +
    boxplot +
    ggplot2::scale_fill_manual(
      unique(data[[deparse(substitute(countries))]]),
      values = unname(asp_palettes$Countries)
    ) +
    scale() +
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
