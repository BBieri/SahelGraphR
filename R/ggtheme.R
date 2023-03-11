#' Dark theme for `{ggplot2}` graphs. Inspired by the excellent
#' `{hrbrthemes}` package.
#'
#' @param ... Additional arguments to be passed on further to `ggplot2::theme()`
#' @return Theme for ggplot graphs in accordance with Bernie's blog colors.
#' @export
#'
#' @examples
#' \donttest{
#'  library(ggplot2)
#'  # Prepare data
#'  cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  # aggregate
#'  colnames(cty_mpg) <- c("make", "mileage")  # change column names
#'  cty_mpg <- cty_mpg[order(cty_mpg$mileage), ]  # sort
#'  cty_mpg$make <- factor(cty_mpg$make, levels = cty_mpg$make)
#'  # Plot
#'  ggplot(cty_mpg, aes(x=make, y=mileage)) +
#'    geom_segment(aes(x=make,
#'                     xend=make,
#'                     y=0,
#'                     yend=mileage), color = "white") +
#'    geom_point(size=3, color = SahelGraphR::asp_palettes$Dark[["yellow-full"]]) +
#'    labs(title="Lollipop Chart",
#'         subtitle="Make Vs Avg. Mileage",
#'         caption="source: mpg") +
#'    SahelGraphR::themeaspdark() +
#'    theme(axis.text.x = element_text(angle=65, vjust=0.6))
#'  }

themeaspdark <- function(...) {
  hrbrthemes::theme_modern_rc(
    base_family = "sans",
    subtitle_family = "sans",
    caption_family = "sans"
  ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = SahelGraphR::asp_palettes$Dark[["bg-grey"]],
                                               color = NA),
      plot.background = ggplot2::element_rect(fill = SahelGraphR::asp_palettes$Dark[["bg-grey"]],
                                              color = NA),
      ...
    )
}


#' Light theme for `{ggplot2}` graphs
#'
#' @param ... Additional arguments to be passed on further to `ggplot2::theme()`
#' @return Theme for ggplot graphs inspired by coal paper and the excellent
#' `{hrbrthemes}` package.
#' @export
#'
#' @examples
#' \donttest{
#'  library(ggplot2)
#'  # Prepare data
#'  cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  # aggregate
#'  colnames(cty_mpg) <- c("make", "mileage")  # change column names
#'  cty_mpg <- cty_mpg[order(cty_mpg$mileage), ]  # sort
#'  cty_mpg$make <- factor(cty_mpg$make, levels = cty_mpg$make)
#'  # Plot
#'  ggplot(cty_mpg, aes(x = make, y = mileage)) +
#'    geom_segment(aes(x = make,
#'                     xend = make,
#'                     y = 0,
#'                     yend = mileage), color = "#C3CED6") +
#'    geom_point(size=3, color = "#C3CED6") +
#'    labs(title = "Lollipop Chart",
#'         subtitle = "Make Vs Avg. Mileage",
#'         caption = "source: mpg") +
#'    SahelGraphR::themeaspdark() +
#'    theme(axis.text.x = element_text(angle = 65, vjust = 0.6))
#'  }

themeasplight <- function(...) {
  hrbrthemes::theme_ipsum(
    base_family = "sans",
    subtitle_family = "sans",
    caption_family = "sans"
  ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = SahelGraphR::asp_palettes$Light[["bg-white"]],
                                               color = NA),
      plot.background = ggplot2::element_rect(fill = SahelGraphR::asp_palettes$Light[["bg-white"]],
                                              color = NA),
      panel.grid = ggplot2::element_line(colour = "#4c4d4c"),
      ...
    )
}
