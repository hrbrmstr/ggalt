#' Lollipop charts
#'
#' The lollipop geom is used to create lollipop charts.
#'
#' Lollipop charts are the creation of Andy Cotgreave going back to 2011. They
#' are a combination of a thin segment, starting at with a dot at the top and are a
#' suitable alternative to or replacement for bar charts.
#'
#' Use the \code{horizontal} parameter to abate the need for \code{coord_flip()}
#' (see the \code{Arguments} section for details).
#'
#' \if{html}{
#' A sample of the output from \code{geom_lollipop()}:
#'
#' \figure{geomlollipop01.png}{options: width="100\%" alt="Figure: geomlollipop01.png"}
#' }
#'
#' \if{latex}{
#' A sample of the output from \code{geom_lollipop()}:
#'
#' \figure{geomlollipop01.png}{options: width=10cm}
#' }
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#' @inheritParams ggplot2::layer
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#' @param horizontal \code{horizontal} is \code{FALSE} (the default), the function
#'   will draw the lollipops up from the X axis (i.e. it will set \code{xend}
#'   to \code{x} & \code{yend} to \code{0}). If \code{TRUE}, it wiill set
#'   \code{yend} to \code{y} & \code{xend} to \code{0}). Make sure you map the
#'   \code{x} & \code{y} aesthetics accordingly. This parameter helps avoid
#'   the need for \code{coord_flip()}.
#' @param point.size the size of the point
#' @param point.colour the colour of the point
#' @inheritParams ggplot2::layer
#' @export
#' @examples
#' df <- data.frame(trt=LETTERS[1:10],
#'                  value=seq(100, 10, by=-10))
#'
#' ggplot(df, aes(trt, value)) + geom_lollipop()
#'
#' ggplot(df, aes(value, trt)) + geom_lollipop(horizontal=TRUE)
geom_lollipop <- function(mapping = NULL, data = NULL, ...,
                          horizontal = FALSE,
                          point.colour = NULL, point.size = NULL,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomLollipop,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      horizontal = horizontal,
      point.colour = point.colour,
      point.size = point.size,
      ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLollipop <- ggproto("GeomLollipop", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "point.colour", "point.size", "horizontal"),
  default_aes = aes(
    shape = 19, colour = "black", size = 0.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),

  setup_data = function(data, params) {
    if (params$horizontal) {
      transform(data, yend = y, xend = 0)
    } else {
      transform(data, xend = x, yend = 0)
    }
  },

  draw_group = function(data, panel_scales, coord,
                        point.colour = NULL, point.size = NULL,
                        horizontal = FALSE) {

    points <- data
    points$colour <- point.colour %||% data$colour
    points$size <- point.size %||% (data$size * 2.5)

    gList(
      ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
      ggplot2::GeomPoint$draw_panel(points, panel_scales, coord)
    )

  },

  draw_key = draw_key_point
)


