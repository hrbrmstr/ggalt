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
#' @param size_x the size of the start point
#' @param colour_x the colour of the start point
#' @param size_xend the size of the end point
#' @param colour_xend the colour of the end point
#' @param dot_guide if \code{TRUE}, a leading dotted line will be placed before the left-most dumbbell point
#' @param dot_guide_size,dot_guide_colour singe-value aesthetics for \code{dot_guide}
#' @inheritParams ggplot2::layer
#' @export
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(trt=LETTERS[1:5], l=c(20, 40, 10, 30, 50), r=c(70, 50, 30, 60, 80))
#'
#' ggplot(df, aes(y=trt, x=l, xend=r)) +
#'   geom_dumbbell(size=3, color="#e3e2e1",
#'                 colour_x = "#5b8124", colour_xend = "#bad744",
#'                 dot_guide=TRUE, dot_guide_size=0.25) +
#'   labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
#'   theme_minimal() +
#'   theme(panel.grid.major.x=element_line(size=0.05))
geom_dumbbell <- function(mapping = NULL, data = NULL, ...,
                          colour_x = NULL, size_x = NULL,
                          colour_xend = NULL, size_xend = NULL,
                          dot_guide = FALSE, dot_guide_size = NULL,
                          dot_guide_colour = NULL,
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
      colour_x = colour_x,
      size_x = size_x,
      colour_xend = colour_xend,
      size_xend = size_xend,
      dot_guide = dot_guide,
      dot_guide_size = dot_guide_size,
      dot_guide_colour = dot_guide_colour,
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
                      "colour_x", "size_x",
                      "colour_xend", "size_xend",
                      "dot_guide", "dot_guide_size", "dot_guide_colour"),
  default_aes = aes(
    shape = 19, colour = "black", size = 0.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),

  setup_data = function(data, params) {
    transform(data, yend = y)
  },

  draw_group = function(data, panel_scales, coord,
                        colour_x = NULL, size_x = NULL,
                        colour_xend = NULL, size_xend = NULL,
                        dot_guide = NULL, dot_guide_size = NULL,
                        dot_guide_colour = NULL) {

    points.x <- data
    points.x$colour <- colour_x %||% data$colour
    points.x$xend <- NULL
    points.x$size <- size_x %||% (data$size * 1.2)

    points.xend <- data
    points.xend$x <- points.xend$xend
    points.xend$xend <- NULL
    points.xend$colour <- colour_xend %||% data$colour
    points.xend$size <- size_xend %||% (data$size * 1.25)

    dot_df <- data
    dot_df$xend <- ifelse(data$xend < data$x, data$xend, data$x)
    dot_df$x <- -Inf
    dot_df$linetype <- "11"
    dot_df$size <- dot_guide_size %||% (data$size * 0.5)
    dot_df$colour <- dot_guide_colour %||% "#5b5b5b"

    if (is.null(dot_guide) | !dot_guide) {

      gList(
        ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
        ggplot2::GeomPoint$draw_panel(points.x, panel_scales, coord),
        ggplot2::GeomPoint$draw_panel(points.xend, panel_scales, coord)
      )

    } else {

      gList(
        ggplot2::GeomSegment$draw_panel(dot_df, panel_scales, coord),
        ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
        ggplot2::GeomPoint$draw_panel(points.x, panel_scales, coord),
        ggplot2::GeomPoint$draw_panel(points.xend, panel_scales, coord)
      )

    }

  },

  draw_key = draw_key_point
)


