#' Individual Boxplot Plots per Country
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
#' @param theme Theme applied
#'
#' @return A ggplot object of boxplot style.
#' @export
#'
#' @examples
#' library(patchwork)
#' sahel_ggboxplot(
#'   data = sahel_sim,
#'   variable = hh_income,
#'   countries = country_names,
#'   title = "Household Income",
#'   scale = ggplot2::scale_x_log10
#' ) + sahel_ggboxplot(
#'   data = sahel_sim,
#'   variable = hh_consumption,
#'   countries = country_names,
#'   title = "Household Consumption",
#'   scale = ggplot2::scale_x_log10
#' )

sahel_ggboxplot <- function(data, variable, countries,
                           title = deparse(substitute(variable)),
                           subtitle = "Boxplot Plot",
                           caption = NULL,
                           xtitle = NULL,
                           ytitle = NULL,
                           scale = NULL,
                           theme = themeaspdark){

  # Process color vector if it is a named vector:
  if (!is.null(names(colors))) {
    colors <- unname(colors)
  }
  scale <- if (!is.null(scale))
    match.fun(scale)
  if (!is.function(scale)) stop("argument scale is not a function!")

  ggplot2::ggplot(data) +
    ggplot2::geom_boxplot(ggplot2::aes(x = {{ variable }},
                                       y = {{ countries }},
                                       fill = {{ countries }}),
                          alpha = .5,
                          color = "grey75") +
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
