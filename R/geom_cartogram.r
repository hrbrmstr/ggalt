#' Polygons from a reference map
#'
#' This is pure annotation, so does not affect position scales.
#'
#' @export
#' @param map Data frame that contains the map coordinates.  This will
#'   typically be created using \code{\link{fortify}} on a spatial object.
#'   It must contain columns \code{x}, \code{long} or \code{longitude},
#'   \code{y}, \code{lat} or \code{latitude} and \code{region} or \code{id}.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
geom_cartogram <- function(mapping = NULL, data = NULL,
                     stat = "identity",
                     ...,
                     map,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  # Get map input into correct form

  stopifnot(is.data.frame(map))

  if (!is.null(map$latitude)) map$y <- map$latitude
  if (!is.null(map$lat)) map$y <- map$lat

  if (!is.null(map$longitude)) map$x <- map$longitude
  if (!is.null(map$long)) map$x <- map$long

  if (!is.null(map$region)) map$id <- map$region

  stopifnot(all(c("x", "y", "id") %in% names(map)))

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCartogram,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      map = map,
      na.rm = na.rm,
      ...
    )
  )
}

#' Geom Cartogram
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCartogram <- ggproto("GeomCartogram", GeomPolygon,
  draw_panel = function(data, panel_scales, coord, map) {
    # Only use matching data and map ids
    common <- intersect(data$map_id, map$id)
    data <- data[data$map_id %in% common, , drop = FALSE]
    map <- map[map$id %in% common, , drop = FALSE]

    # Munch, then set up id variable for polygonGrob -
    # must be sequential integers
    coords <- coord_munch(coord, map, panel_scales)
    coords$group <- coords$group %||% coords$id
    grob_id <- match(coords$group, unique(coords$group))

    # Align data with map
    data_rows <- match(coords$id[!duplicated(grob_id)], data$map_id)
    data <- data[data_rows, , drop = FALSE]

    grid::polygonGrob(coords$x, coords$y, default.units = "native", id = grob_id,
      gp = gpar(
        col = data$colour, fill = alpha(data$fill, data$alpha),
        lwd = data$size * .pt
      )
    )
  },

  required_aes = c("x", "y", "map_id")
)
