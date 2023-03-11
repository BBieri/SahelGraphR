#' Complete list of palettes
#'
#' Use \code{\link{asp_palette}} to construct palettes of desired length.
#'
#' @export
asp_palettes <- list(
  "Dark" = c(
    "bg-grey"           = "#4c4d4c",
    "grey-crtl"         = "#939593",
    "blue-capital"      = "#0070c0",
    "green-psycho"      = "#00b050",
    "yellow-full"       = "#ffd635"
  ),
  "Light" = c(
    "bg-white"          = "#ffffff",
    "light-grey-crtl"   = "#e4e4e4",
    "smoke-capital"     = "#bdbdbd",
    "charcoal-psycho"   = "#454545",
    "anthracite-full"   = "#0a0a0a"
  )
)

#' A palette generator for the Sahel Adaptive Social Protection RCT
#'
#' These are a few color palettes useful for the production of graphs for the
#' Sahel ASP RCT program.
#' This function calls one of two official palettes in
#' \code{\link{asp_palette}}: one dark theme and one light theme.
#'
#' @param n Number of colors desired. If omitted, uses all colors.
#' @param name Name of desired palette. Current choices are:
#'   \code{IHEID}, \code{Centres}, and \code{SDGs}.
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colors.
#' @importFrom graphics rect par image text
#' @return A vector of colors.
#' @source Adapted from
#' \url{https://github.com/karthik/wesanderson/blob/master/R/colors.R}
#' @export
#' @keywords colors
#' @examples
#' asp_palette("Dark")
#' asp_palette("Light")
#'
#' # If you need more colors than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colors
#' pal <- asp_palette(21, name = "Dark", type = "continuous")
#' image(volcano, col = pal)

asp_palette <-
  function(name, n, type = c("discrete", "continuous")) {
    type <- match.arg(type)
    pal <- asp_palettes[[name]]
    if (is.null(pal))
      stop("Palette not found.")
    if (missing(n)) {
      n <- length(pal)
    }
    if (type == "discrete" && n > length(pal)) {
      stop("Number of requested colors greater than what palette can offer")
    }
    out <- switch(type,
                  continuous = grDevices::colorRampPalette(pal)(n),
                  discrete = pal[1:n])
    structure(out, class = "palette", name = name)
  }

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  # Print methods for palette objects
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  image(
    1:n,
    1,
    as.matrix(1:n),
    col = x,
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    bty = "n"
  )
  rect(0,
       0.9,
       n + 1,
       1.1,
       col = rgb(1, 1, 1, 0.8),
       border = NA)
  text((n + 1) / 2,
       1,
       labels = attr(x, "name"),
       cex = 1,
       family = "serif"
  )
}
