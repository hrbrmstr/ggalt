#' Bars, rectangles with bases on x-axis but with "identity" as the
#' default "stat"
#'
#' This is an alternative to \code{geom_bar} for making bar charts,
#' It uses  \code{stat="identity"} which requires you to map a variable to
#' the \code{y} aesthetic. There is no option to use another "stat".
#'
#' @export
#' @section Aesthetics:
#' \code{geom_col} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{fill}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @inheritParams ggplot2::geom_bar
#' @param width Bar width. By default, set to 90\% of the resolution of the data.
#' @examples
#' df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
#' ggplot(df, aes(trt, outcome)) + geom_col()
geom_col <- function(mapping = NULL, data = NULL,
                     position = "stack",
                     ...,
                     width = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomCol,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCol <- ggproto("GeomCol", GeomRect,
  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    transform(data,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
  },

  draw_panel = function(self, data, panel_scales, coord, width = NULL) {
    # Hack to ensure that width is detected as a parameter
    ggproto_parent(GeomRect, self)$draw_panel(data, panel_scales, coord)
  }
)
