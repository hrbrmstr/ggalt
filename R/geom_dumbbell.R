#' Dumbell charts
#'
#' The dumbbell geom is used to create dumbbell charts.
#'
#' Dumbbell dot plots — dot plots with two or more series of data — are an
#' alternative to the clustered bar chart or slope graph.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "segment")}
#' @inheritParams ggplot2::layer
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#' @param point.size.l the size of the left point
#' @param point.colour.l the colour of the left point
#' @param point.size.r the size of the right point
#' @param point.colour.r the colour of the right point
#' @inheritParams ggplot2::layer
#' @export
#' @examples
#' df <- data.frame(trt=LETTERS[1:5],
#'                  l=c(20, 40, 10, 30, 50),
#'                  r=c(70, 50, 30, 60, 80))
#'
#' ggplot(df, aes(y=trt, x=l, xend=r)) + geom_dumbbell()
geom_dumbbell <- function(mapping = NULL, data = NULL, ...,
                          point.colour.l = NULL, point.size.l = NULL,
                          point.colour.r = NULL, point.size.r = NULL,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomDumbbell,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      point.colour.l = point.colour.l,
      point.size.l = point.size.l,
      point.colour.r = point.colour.r,
      point.size.r = point.size.r,
      ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDumbbell <- ggproto("GeomDumbbell", Geom,
  required_aes = c("x", "xend", "y"),
  non_missing_aes = c("size", "shape",
                      "point.colour.l", "point.size.l",
                      "point.colour.r", "point.size.r"),
  default_aes = aes(
    shape = 19, colour = "black", size = 0.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),

  setup_data = function(data, params) {
    transform(data, yend = y)
  },

  draw_group = function(data, panel_scales, coord,
                        point.colour.l = NULL, point.size.l = NULL,
                        point.colour.r = NULL, point.size.r = NULL) {

    points.l <- data
    points.l$colour <- point.colour.l %||% data$colour
    points.l$size <- point.size.l %||% (data$size * 2.5)

    points.r <- data
    points.r$x <- points.r$xend
    points.r$colour <- point.colour.r %||% data$colour
    points.r$size <- point.size.r %||% (data$size * 2.5)

    gList(
      ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
      ggplot2::GeomPoint$draw_panel(points.l, panel_scales, coord),
      ggplot2::GeomPoint$draw_panel(points.r, panel_scales, coord)
    )

  },

  draw_key = draw_key_point
)


